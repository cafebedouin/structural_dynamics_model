% ============================================================================
% CONSTRAINT STORY: personhood_boundary__birth_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:directionality_override/3,
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
 *   The birth-threshold reading of the personhood kernel grounds moral and
 *   legal standing in the fact of being born, not in demonstrated capacities,
 *   fitness markers, or rational agency. Under this reading, every human born
 *   alive possesses unconditional moral standing and legal protection from
 *   the state and other agents. This reading directly contradicts
 *   fitness-contingent framings that would condition personhood on capacity
 *   assessment, and it influences (without strictly foreclosing)
 *   potential-based readings that ground personhood in rational-agency
 *   trajectories. The constraint operates as a commitment-system
 *   instantiation: it grounds legitimacy in a fixed boundary (birth), it has
 *   authoritative adjudicators (states, courts), and it carries an
 *   interpretive tradition that must manage the tension between the
 *   categorical rule and individual variations in capacity. This story
 *   instantiates ONLY the birth-threshold reading; sibling readings are
 *   separate constraint stories with their own ε, beneficiary/victim
 *   structures, and classifications.
 *
 * KEY AGENTS:
 *   - Newborn infants: powerless, categorical beneficiaries under this reading; receive unconditional standing at birth
 *   - Severely disabled infants: powerless, simultaneously beneficiaries (protected by categorical rule) and potential payers (may experience resource scarcity if allocation is capacity-based)
 *   - Birth-threshold doctrine proponents: organized, beneficiaries of the rule's clarity and closure; vindicate universal human dignity
 *   - State authority: institutional, agenda-setter; bound to enforce uniform categorical personhood
 *   - Fitness-contingent advocates: organized, excluded; would argue for capacity-based personhood
 *   - Potential-based philosophers: organized, excluded; would condition standing on rational-agency potential
 *   - Philosophical observer: analytical seat recording the reading's structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.25).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.12).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Boundary: Birth Threshold Reading").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral_philosophy/commitment_systems").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, 'b0f1d079-d293-4133-818f-d2a424afc12d').
narrative_ontology:cs_kernel_codification('b0f1d079-d293-4133-818f-d2a424afc12d', formalized).
narrative_ontology:cs_authority_grounding('b0f1d079-d293-4133-818f-d2a424afc12d', lineage).
narrative_ontology:cs_interpretation_layer_present('b0f1d079-d293-4133-818f-d2a424afc12d').
narrative_ontology:cs_reading_relation('b0f1d079-d293-4133-818f-d2a424afc12d', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('b0f1d079-d293-4133-818f-d2a424afc12d', personhood_boundary__potential_based_reading, influences).
narrative_ontology:cs_axiom('b0f1d079-d293-4133-818f-d2a424afc12d', foundational, birth_confers_personhood).
narrative_ontology:cs_axiom_status(birth_confers_personhood, holdable).
narrative_ontology:cs_axiom_grounding('b0f1d079-d293-4133-818f-d2a424afc12d', birth_confers_personhood, deontological).
narrative_ontology:cs_axiom('b0f1d079-d293-4133-818f-d2a424afc12d', foundational, no_capacity_assessment_required).
narrative_ontology:cs_axiom_status(no_capacity_assessment_required, holdable).
narrative_ontology:cs_axiom_grounding('b0f1d079-d293-4133-818f-d2a424afc12d', no_capacity_assessment_required, conventional).
narrative_ontology:cs_reference_frame('b0f1d079-d293-4133-818f-d2a424afc12d', universal_born_human_dignity).
narrative_ontology:cs_drift_state('b0f1d079-d293-4133-818f-d2a424afc12d', contemporary_medical_capability_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b0f1d079-d293-4133-818f-d2a424afc12d', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, birth_threshold_doctrine_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, newborn_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, severely_disabled_infants).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, parents_and_caregivers).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, severely_disabled_infants).
narrative_ontology:constraint_victim(personhood_boundary__birth_threshold_reading, parents_and_caregivers).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_human_dignity).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, homicide_prohibition_scope).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Acquire unconditional moral standing and legal protection from the moment of birth under this reading. They cannot advocate for themselves; their standing rests on the categorical rule, not on demonstrated attributes. Killing a newborn is homicide under this framework, regardless of the infant's capacities or prior fitness demonstrations.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, newborn_infants, beneficiary,
    powerless, civilizational, trapped, universal).

% Receive unconditional standing at birth even if they will never develop rational agency or fitness markers. They cannot be excluded by the logic of this reading; however, they may bear costs if societal resources are allocated asymmetrically based on capacity, a tension this reading must manage.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, severely_disabled_infants, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, severely_disabled_infants, payer).

% Vindicate a metaphysically simple and legally administrable rule: personhood is conferred by the fact of birth, not by assessment of capacities. They benefit from the doctrine's clarity and its closure against alternative readings; the constraint's operation protects their framework from competing claims about fitness or potential.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, birth_threshold_doctrine_proponents, beneficiary,
    organized, generational, mobile, universal).

% Receive legal standing to advocate for their newborn's interests and protection from the state; they are also bound by the rule's enforcement — they cannot harm their child without legal liability. The constraint removes their discretion to make capacity-based exclusions but grants them proxies for the voiceless.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, parents_and_caregivers, beneficiary,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, parents_and_caregivers, payer).

% Is bound by this reading to grant every born human legal personhood and protection of life. The state cannot impose fitness tests, capacity requirements, or survival-of-the-fittest exclusions on born humans; it administers the categorical rule and enforces homicide prohibitions uniformly.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, state_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Are excluded from the policy conversation under this reading's authority structure. They would argue for capacity-based or fitness-dependent personhood; their framing is treated as logically incoherent or immoral within the framework that this reading instantiates. They remain outside the legitimacy boundary.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, fitness_contingent_advocates, excluded,
    organized, generational, mobile, universal).

% Are excluded from authoritative personhood adjudication under this reading. They would propose that severely disabled infants lacking rational-agency potential occupy an ambiguous status; this reading forecloses that position by making birth the unconditional criterion, rendering their claims structurally illegitimate within the birth-threshold framework.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, potential_based_philosophers, excluded,
    organized, generational, mobile, universal).

% Holds the neutral analytical seat, observing how the birth-threshold reading structures the personhood question, what it vindicates, what it excludes, and how it differs from sibling readings that ground personhood in fitness or potential.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, philosophical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, birth_threshold_doctrine_proponents).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universally administrable criterion for moral and legal standing that does not require ongoing assessment of individual capacities, thereby solving the coordination problem of how to ground unconditional human dignity without case-by-case fitness evaluation.
% TRANSFER_FUNCTION: Transfers moral standing from contingency (capacity, fitness, demonstrated rationality) to inevitability (being born); moves the burden of justification from defenders of inclusion to would-be excluders.
% ABSENT_VOICES: Infants cannot speak for themselves; severely disabled individuals who would be excluded under fitness readings cannot advocate against capacity-based criteria. Fitness-contingent and potential-based philosophers are structurally excluded from the legitimacy consensus — their objections are treated as categorically incoherent within this framework.
% DISAPPEARANCE_RATIONALE: If the birth-threshold personhood rule disappeared overnight — if states adopted fitness-contingent or potential-based personhood — the legal, moral, and institutional landscape would radically reorganize: newborns and severely disabled infants would lack legal protection against state harm, parental duties would be conditioned on capacity, and the homicide prohibition would apply selectively. Legal systems, child protection frameworks, and end-of-life medicine would require wholesale reconstruction.
% FOUNDING_PROBLEM: Pre-modern legal systems and some historical ethical frameworks granted personhood conditionally, resulting in infanticide, slavery, and denial of protection for the disabled. The birth-threshold reading was developed to solve the problem of ensuring unconditional protection for all humans regardless of capacity or demonstrated fitness, and to create a clear, administrable rule that could not be selectively applied.
% FOUNDING_PROBLEM_CORROBORATION: Historians of ethics (Peter Singer's work on speciesism and infanticide debates, Jonathan Glover's historical surveys) document that capacity-based exclusions DID lead to systematic harm and that the birth-threshold rule emerged as a protective response. However, contemporary fitness-contingent and potential-based philosophers dispute whether the founding problem is 'live' — they argue the birth threshold now over-protects by granting standing to beings who lack capacity, and that modern medicine enables capacity assessment, making selective personhood both possible and ethically necessary. The corroboration for the founding problem comes from historical witnesses outside the birth-threshold doctrine (historians, ethicists from competing frameworks); corroboration for the 'contested' status comes from active contemporary advocates of alternative readings.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__birth_threshold_reading, 0.25, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The birth-threshold reading is claimed as a MOUNTAIN — it presents personhood at birth as a natural fact, a self-evident boundary grounded in physical reality rather than constructed policy. However, the metrics show measurable extractiveness (0.25), suppression (0.12), and theater (0.08), which diagnostic indicates a FALSE SUMMIT: the 'natural' boundary is contested and benefits identifiable parties (doctrine proponents, organized advocates of the reading). The accessibility_collapse metric (0.92) is high because once the reading is articulated, alternatives become structurally illegitimate within the framework — the rule's absoluteness forecloses contingency. Resistance (0.31) is moderate because the reading faces active philosophical challenge from fitness-contingent and potential-based alternatives. The measurement series show extraction and suppression rising over the 2000-year interval: as the reading became institutionalized in legal systems, state enforcement required more active suppression of alternative frameworks (fitness-based exclusion, potential-based qualification). The theater ratio remains low, indicating the reading's enforcement is not primarily performative — it is backed by substantive legal machinery and genuine institutional commitment. The claim/metric gap is deliberate: the reading claims to be a natural law, but the authored metrics describe increasing institutional enforcement and managed contestation, signals of a constructed commitment system, not an emergent natural law.
 *
 * PERSPECTIVAL GAP:
 *   The agent seats compute very differently. From the state's analytical position (institutional power, long time horizon), the birth-threshold reading appears as necessary coordination — administrable, legally clear, not requiring ongoing capacity assessment. From the severely disabled infant's trapped position (powerless, no exit), the reading is simultaneously protective (unconditional standing) and constraining (resource allocation may remain capacity-based, creating asymmetry between moral standing and material provision). From the fitness-contingent philosopher's position (organized, mobile exit), the reading is an illegitimate closure that prevents more nuanced personhood assessment. The engine computes these divergent classifications from the structural data; the reading itself does not adjudicate which perspective is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs by stakeholder seat. Newborn and severely disabled infants receive d near 0.0 (full beneficiary: the rule grants them standing unconditionally, at no cost to themselves). Birth-threshold proponents receive d near 0.05-0.15 (partial beneficiary: they benefit from the rule's vindication but do not extract materially from it; they are organized with mobile exit). State authority sits near d=0.5 (symmetric: it gains administrative clarity but bears enforcement costs and must suppress alternatives). Fitness-contingent and potential-based advocates receive d near 0.95-1.0 (full target: the reading's enforcement actively excludes their frameworks from legitimacy). The directionality profile explains why different seats experience this reading so differently: some gain standing at no cost; others are administratively locked out.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading does not exhibit mandatrophy in the classical sense — its founding problem (ensuring unconditional protection against capacity-based exclusion) remains live in the face of active challenges from fitness-contingent and potential-based alternatives. The measurement series shows extraction and suppression RISING over the interval, not decaying into theatrical inertia, which indicates the reading is actively maintained through enforcement and interpretation, not abandoned or left to atrophy. However, the rising theater ratio (0.04 → 0.08) combined with the rising suppression requirement (0.06 → 0.12) does suggest that an increasing share of the reading's maintenance is devoted to managing contestation and suppressing alternative framings, rather than to genuine coordination. This is not mandatrophy — the reading still solves the founding problem — but it is a signal that the reading's authority is increasingly performative: it must be actively defended because the alternative readings are not self-evidently inferior, they are logically coherent competing framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_personhood,
    'Is the birth-threshold boundary a natural fact (grounded in the objective structure of human biology and moral reality) or a constructed rule (adopted for policy reasons and maintained by institutional enforcement)?',
    'Test via counterfactual: if a hypothetical society adopted a fitness-contingent personhood rule and administered it consistently, would that society''s members thereby discover a ''true'' alternative boundary, or would they be committing a category error? If the former, personhood is constructed and the birth threshold is contingent; if the latter, personhood is a natural fact and the birth threshold is discovered.',
    'If the boundary is constructed, the rising extractiveness and suppression metrics indicate the reading is becoming more institutional and less natural — a false summit. If the boundary is natural, the metrics reflect only the institutional defense of a true claim, not an underlying extractive structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_personhood, conceptual, 'Whether the birth boundary is discovered or constructed').

omega_variable(
    severely_disabled_standing_paradox,
    'Does the birth-threshold reading actually grant unconditional standing to severely disabled infants, or does it mask a latent capacity-based assessment that de facto excludes them?',
    'Empirical: examine institutional practice around end-of-life decision-making, resource allocation, and legal protection for severely disabled newborns. If the same states that formally adopt birth-threshold personhood routinely withdraw care from profoundly disabled infants, the reading''s unconditional standing is performative, not actual.',
    'If masked exclusion occurs, the reading is a tangled_rope (coordination story + asymmetric extraction) rather than a mountain. The standing is ostensible; the real boundary remains capacity-based.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(severely_disabled_standing_paradox, empirical, 'Whether the categorical rule conceals latent capacity-based exclusion').

omega_variable(
    alternative_reading_foreclosure,
    'Does the birth-threshold reading logically foreclose the fitness-contingent and potential-based readings, or do all three framings remain live as coherent alternatives?',
    'Logical analysis of the core claims: birth_threshold asserts ''being born is sufficient for personhood''; fitness_contingent asserts ''demonstrable capacity is necessary''; potential_based asserts ''capacity-potential is sufficient''. These are contradictory within any single framework but can coexist across different parties'' commitments. The foreclosure question turns on whether a single party can hold all three claims without incoherence (impossible — they contradict) or whether the readings are merely opposed alternatives (logically possible).',
    'If the birth-threshold reading forecloses the others within its framework, the reading_relations entries should be ''forecloses''. If the readings are opposed but logically independent, they coexist_with. This affects the engine''s computation of cross-reading coupling and contamination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether readings are logically exclusive or merely opposed').

omega_variable(
    institutional_extraction_driver,
    'What is driving the rising extractiveness and suppression metrics over the 2000-year interval? Is it: (a) increasing institutional capacity to enforce the reading against alternatives (institutional maturation), (b) increasing resistance from fitness-contingent and potential-based advocates (reflecting genuine contestation), or (c) accumulation of capacity-based exclusions masked behind the birth-threshold rule (latent extraction)?',
    'Temporal analysis: separate the drivers by examining specific historical moments of enforcement intensification, documented philosophical challenges, and institutional practice shifts. Correlate with external factors (medical technology enabling capacity assessment, feminist and disability-rights movements challenging fitness criteria, etc.).',
    '(a) supports the mountain claim but indicates the reading is increasingly defended rather than self-evident; (b) supports coexists_with rather than forecloses; (c) supports tangled_rope classification and false-summit reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_driver, empirical, 'What drives rising extraction/suppression over the interval').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(pers_tr_t0, projected).
narrative_ontology:measurement(pers_tr_t500, personhood_boundary__birth_threshold_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement_basis(pers_tr_t500, observed).
narrative_ontology:measurement(pers_tr_t1000, personhood_boundary__birth_threshold_reading, theater_ratio, 1000, 0.06).
narrative_ontology:measurement_basis(pers_tr_t1000, observed).
narrative_ontology:measurement(pers_tr_t1500, personhood_boundary__birth_threshold_reading, theater_ratio, 1500, 0.07).
narrative_ontology:measurement_basis(pers_tr_t1500, observed).
narrative_ontology:measurement(pers_tr_t2000, personhood_boundary__birth_threshold_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(pers_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(pers_be_t0, projected).
narrative_ontology:measurement(pers_be_t500, personhood_boundary__birth_threshold_reading, base_extractiveness, 500, 0.12).
narrative_ontology:measurement_basis(pers_be_t500, observed).
narrative_ontology:measurement(pers_be_t1000, personhood_boundary__birth_threshold_reading, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement_basis(pers_be_t1000, observed).
narrative_ontology:measurement(pers_be_t1500, personhood_boundary__birth_threshold_reading, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement_basis(pers_be_t1500, observed).
narrative_ontology:measurement(pers_be_t2000, personhood_boundary__birth_threshold_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement_basis(pers_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(pers_su_t0, projected).
narrative_ontology:measurement(pers_su_t500, personhood_boundary__birth_threshold_reading, suppression_requirement, 500, 0.08).
narrative_ontology:measurement_basis(pers_su_t500, observed).
narrative_ontology:measurement(pers_su_t1000, personhood_boundary__birth_threshold_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement_basis(pers_su_t1000, observed).
narrative_ontology:measurement(pers_su_t1500, personhood_boundary__birth_threshold_reading, suppression_requirement, 1500, 0.11).
narrative_ontology:measurement_basis(pers_su_t1500, observed).
narrative_ontology:measurement(pers_su_t2000, personhood_boundary__birth_threshold_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(pers_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__birth_threshold_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel admits three logically distinct readings, each instantiating a separate constraint. The birth_threshold_reading (this constraint) forecloses the fitness_contingent_reading and influences the potential_based_reading within its own framework. All three readings share the same kernel object (what makes a being a person?) but differ in their answer. This story links to its siblings via network.affects_constraints; the decomposition is documented in each story's commentary.kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__birth_threshold_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
