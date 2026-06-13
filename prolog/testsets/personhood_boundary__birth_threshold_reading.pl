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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: personhood_boundary__birth_threshold_reading
 *   human_readable: Personhood Begins at Birth (Threshold Reading)
 *   domain: moral/philosophical/commitment_system
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested kernel: personhood
 *   boundary. The birth-threshold reading declares that moral standing and
 *   legal personhood begin at birth, such that all born humans possess equal
 *   standing regardless of cognitive ability, fitness, or social utility. The
 *   reading is presented and often experienced as a natural law — an obvious
 *   or inevitable fact about human nature. Yet it is one of three live,
 *   internally coherent readings of the personhood boundary kernel; the
 *   others (fitness-contingent and potential-based) have historical
 *   institutional power and remain defended by credible philosophical
 *   traditions. This story captures the birth-threshold reading as a
 *   constraint: what it must do to persist, whom it benefits and whom it
 *   potentially excludes, and the structural tensions between its natural-law
 *   framing and its constructed institutional reality.
 *
 * KEY AGENTS:
 *   - all_born_humans (beneficiary, victim set — receive universal moral standing)
 *   - legal_protection_regimes (beneficiary, agenda-setter — administer the rule, gain simplicity and uniformity)
 *   - medical_and_legal_authorities (agenda-setter — enforce the boundary, train practitioners)
 *   - philosophical_traditions_affirming_birth_threshold (observer, reference frame for this reading)
 *   - fitness_contingent_tradition (excluded sibling reading — argues for contingent standing)
 *   - potential_based_tradition (excluded sibling reading — argues for agency-based standing)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__birth_threshold_reading, 0.31).
domain_priors:suppression_score(personhood_boundary__birth_threshold_reading, 0.28).
domain_priors:theater_ratio(personhood_boundary__birth_threshold_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(personhood_boundary__birth_threshold_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__birth_threshold_reading, mountain).
narrative_ontology:human_readable(personhood_boundary__birth_threshold_reading, "Personhood Begins at Birth (Threshold Reading)").
narrative_ontology:topic_domain(personhood_boundary__birth_threshold_reading, "moral/philosophical/commitment_system").

domain_priors:emerges_naturally(personhood_boundary__birth_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__birth_threshold_reading, '69678121-4250-4664-9237-b07e4ed3afeb').
narrative_ontology:cs_kernel_codification('69678121-4250-4664-9237-b07e4ed3afeb', fixed_text).
narrative_ontology:cs_authority_grounding('69678121-4250-4664-9237-b07e4ed3afeb', lineage).
narrative_ontology:cs_interpretation_layer_present('69678121-4250-4664-9237-b07e4ed3afeb').
narrative_ontology:cs_reading_relation('69678121-4250-4664-9237-b07e4ed3afeb', personhood_boundary__fitness_contingent_reading, forecloses).
narrative_ontology:cs_reading_relation('69678121-4250-4664-9237-b07e4ed3afeb', personhood_boundary__potential_based_reading, coexists_with).
narrative_ontology:cs_axiom('69678121-4250-4664-9237-b07e4ed3afeb', foundational, birth_is_non_negotiable_threshold).
narrative_ontology:cs_axiom_status(birth_is_non_negotiable_threshold, holdable).
narrative_ontology:cs_axiom_grounding('69678121-4250-4664-9237-b07e4ed3afeb', birth_is_non_negotiable_threshold, deontological).
narrative_ontology:cs_axiom('69678121-4250-4664-9237-b07e4ed3afeb', foundational, equal_standing_from_birth_forward).
narrative_ontology:cs_axiom_status(equal_standing_from_birth_forward, holdable).
narrative_ontology:cs_axiom_grounding('69678121-4250-4664-9237-b07e4ed3afeb', equal_standing_from_birth_forward, deontological).
narrative_ontology:cs_reference_frame('69678121-4250-4664-9237-b07e4ed3afeb', universal_human_dignity_from_birth).
narrative_ontology:cs_drift_state('69678121-4250-4664-9237-b07e4ed3afeb', contemporary_medical_ethics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69678121-4250-4664-9237-b07e4ed3afeb', '').
narrative_ontology:cs_kernel_id(personhood_boundary__birth_threshold_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, all_born_humans).
narrative_ontology:constraint_beneficiary(personhood_boundary__birth_threshold_reading, legal_protection_regimes).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, universal_human_dignity).
narrative_ontology:constraint_vindicates(personhood_boundary__birth_threshold_reading, equal_moral_status_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess moral standing and legal personhood from birth forward. No cognitive deficit, genetic condition, or disability can diminish this standing. Every born infant is protected from homicide by law; killing them is not permissible under any fitness or utility calculus. The constraint ensures that the most vulnerable born humans — those with severe disabilities, extreme prematurity, or conditions incompatible with independent life — retain full standing.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, all_born_humans, beneficiary,
    powerless, biographical, trapped, universal).

% Administer a bright-line rule: personhood = birth. This removes discretion about who counts. They charge homicide when a born infant is killed. They do not weigh the victim's cognitive capacity, prognosis, or social contribution. The rule is administratively simple and prevents arbitrary exclusion; it also constrains medical and parental authority over life-and-death decisions in ways that impose enforcement costs.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, legal_protection_regimes, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__birth_threshold_reading, legal_protection_regimes, agenda_setter).

% Train clinicians and practitioners to treat all born humans as persons with full standing. Yet they also face situations where the birth-threshold conflicts with medical discretion: non-treatment decisions for severely disabled or premature infants, DNR orders, withdrawal of life support. The constraint creates tension between universal standing (enforced) and case-by-case clinical judgment (permitted), which generates institutional pressure to find workarounds.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, medical_and_clinical_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Affirm the birth-threshold reading as essential protection against eugenic and fitness-based harm. They testify that disabled born infants require the constraint's safeguard; without it, fitness arguments would justify infanticide or non-treatment. But they also document that birth-threshold standing coexists with clinical discretion to withhold care, creating a gap between stated protection and actual practice. They monitor for mandatrophy.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, disability_rights_advocates, observer,
    organized, generational, mobile, national).

% Would argue that personhood should be contingent on demonstrated fitness, rationality, or social capacity. This reading is institutionally excluded from legal and medical authority; its proponents do not set policy, though they publish in academic venues and maintain living philosophical traditions. Their exclusion is what the constraint's institutional enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, fitness_contingent_philosophers, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(personhood_boundary__birth_threshold_reading, fitness_contingent_philosophers).

% Would argue that personhood grounds in potential for rational agency, which might withhold standing from some severely disabled born infants lacking any capacity for or prospect of rational thought. This reading coexists with the birth-threshold in academic discourse and some bioethics frameworks, but has reduced institutional power in legal systems enforcing homicide law. Their position is not as thoroughly suppressed as fitness-contingent readings but faces institutional barriers to implementation.
narrative_ontology:constraint_stakeholder(personhood_boundary__birth_threshold_reading, potential_based_philosophers, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(personhood_boundary__birth_threshold_reading, potential_based_philosophers).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__birth_threshold_reading, legal_protection_regimes).
narrative_ontology:fixing_cost_class(personhood_boundary__birth_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-negotiable boundary for moral standing: birth. This solves the coordination problem among legal systems, medical authorities, philosophers, and societies: rather than debating whether each born human merits standing based on fitness, capacity, or utility, all parties treat birth as sufficient. The rule enables legal systems to prosecute homicide uniformly, medical ethics to protect all infants without discretionary assessment, and moral communities to share a single criterion.
% TRANSFER_FUNCTION: Transfers moral standing (and its legal consequence, protection from homicide) from potential gatekeepers — parents, medical authorities, eugenic frameworks — to all born humans universally. No entity may revoke the standing; no condition can suspend it. The constraint moves the decision power: it is no longer with those who might apply fitness tests, but with the institutional rule itself.
% ABSENT_VOICES: Fitness-contingent and potential-based philosophical traditions are excluded from institutional frameworks that define legal personhood. They would argue that the birth-threshold is arbitrary, that fitness or capacity should determine standing, and that some born humans may lack merit-based personhood. They publish and teach but do not set law or clinical policy. The exclusion is structural: the birth-threshold framework does not permit their readings to be live options for legal and medical authority.
% DISAPPEARANCE_RATIONALE: If the birth-threshold constraint vanished, legal systems would fragment. Some jurisdictions would retain birth-based protection; others would adopt fitness or potential criteria, creating legal classes of born humans with diminished standing. Medical authorities would gain discretion to assign personhood case-by-case based on prognosis and capacity. Disabled born infants would face heightened vulnerability to infanticide and non-treatment. The constraint does not disappear into some natural remainder — it actively prevents a world in which fitness-based personhood operates. Yet that world remains conceivable and defended by live philosophical traditions.
% FOUNDING_PROBLEM: Historical and ongoing infanticide, exposure, enslavement, and eugenic harm directed at disabled, female, poor, and otherwise disvalued born humans. The founding problem: how to establish a rule that makes all born humans legally and morally protected, removing the ability of parents, states, or medical authorities to exclude born humans on grounds of fitness, utility, or social standing.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights organizations, human rights bodies, and medical historians document that infanticide and selective non-treatment of disabled infants remain live threats in many jurisdictions, confirming the founding problem persists. Pediatric ethicists outside the birth-threshold tradition argue that the problem should be solved by refined fitness criteria, not absolute thresholds. Philosophers and religious traditions testify to the historical prevalence of fitness-based infanticide. The founding problem's reality is corroborated; its correct solution is disputed.
narrative_ontology:disappearance_verdict(personhood_boundary__birth_threshold_reading, contested).
narrative_ontology:founding_problem_status(personhood_boundary__birth_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__birth_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__birth_threshold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__birth_threshold_reading_tests).

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
 *   The birth-threshold reading is CLAIMED as a mountain (emerges_naturally: true) because it asserts a natural fact about human ontology. The authored metrics describe low extractiveness (0.31) and suppression (0.28) because the constraint appears universally beneficial (no identifiable victims) and faces only moderate resistance (some philosophical and religious traditions contest it). Theater is very low (0.12) because the constraint's function (universal protection) and its operation (enforcement of homicide law) are tightly aligned — there is little gap between justification and practice, though some healthcare discretion for disabled infants (non-treatment decisions) does constitute a performative gap. Accessibility collapse is very high (0.92) because once the birth threshold is accepted, alternative personhood boundaries become nearly unthinkable in liberal legal contexts. The measurement series shows very slight uptick in extractiveness and suppression over the interval (0.28→0.31, 0.24→0.28) reflecting slow institutional hardening of the rule against emerging fitness-based challenges in medical ethics; this is NOT a Goodhart drift (theater stays low) but a strengthening of enforcement pressure. The FSM gate fires because beneficiaries are declared (all_born_humans, legal_protection_regimes) on a mountain claim. This is INTENTIONAL: the story documents the possible false summit — is this constraint a natural law that benefits humans as a side effect of being true, or a constructed institutional achievement that benefits some constituencies (legal systems, disabled infants) by excluding alternative readings? The omegas route the committer structure rather than the metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the all-born-humans seat: the constraint is purely beneficial, a protective boundary with no cost. From the legal-protection-regimes seat: the constraint is beneficial but imposes enforcement cost (must maintain uniformity, must prosecute cases where fitness arguments would justify different outcomes, must suppress alternative frameworks among medical staff). From the fitness-contingent tradition (excluded): the reading is arbitrary institutional power, denying natural variations in moral standing. From the potential-based tradition (excluded): the reading is over-protective, extending standing to beings that cannot exercise it rationally. The institutional framework enforces the asymmetry: only beneficiary and agenda-setter seats have standing to define the constraint; excluded traditions are shut out. This asymmetry is structural, not accidental, and is part of what makes the false-summit question live.
 *
 * DIRECTIONALITY LOGIC:
 *   All-born-humans derive low d (near 0.0, full beneficiary): they receive universal moral standing with no cost. They are trapped (cannot exit the boundary), but being trapped in a protective boundary is not extraction. Legal-protection-regimes derive moderate d (~0.3-0.4): they benefit from the bright-line rule (gain complexity reduction, gain institutional authority to apply it uniformly) but also bear the cost of enforcement (must suppress alternative readings, must invest in training, must prosecute homicide cases even when fitness considerations might cut the other way). Medical-and-legal-authorities sit at similar d: they administer the rule, which gives them authority but also constrains their discretion. Excluded traditions have no d in the framework because they are not parties to the institutional system — the constraint *suppresses* their readings, so there is no derivation path; they are out, analytically noted but structurally excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is clear: how to prevent infanticide, exposure, and enslavement of disabled or disvalued humans. The birth-threshold reading solves it by making birth non-negotiable. BUT: modern medical practice permits non-treatment decisions for severely disabled infants, which creates a gap between the reading's stated function (universal protection of all born humans) and its actual operation (universal standing that coexists with medical discretion to withhold life-sustaining care). If disabled infants are killed or allowed to die by withholding nutrition/oxygen, the founding problem is not solved — the constraint's mandate persists but the function has atrophied. The theater_ratio of 0.12 is low, suggesting the gap is not yet dominant, but the measurement series shows slow creep (0.08→0.12), indicating mounting pressure from medical ethics discussions. This is a candidate for mandatrophy if the gap widens: the birth-threshold reading would be maintained ceremonially while the real decisions about disabled infant survival are made by fitness-based clinical judgment (hidden under the headings of 'prognosis,' 'best interest,' 'quality of life'). The omega on disabled_infant_protection_mechanism directly addresses this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_threshold,
    'Is the birth-threshold reading a natural law — an irreducible fact about human ontology — or a constructed commitment that benefits identifiable constituencies by excluding alternative readings?',
    'Historical-comparative analysis: does the boundary originate in natural facts or in deliberate institutional choices? Does the rule persist because it reflects natural necessity or because parties with power over legal/medical authority maintain it? Do alternative readings exist with equal epistemic warrant but less institutional amplification?',
    'If natural law: the constraint is genuinely mountain-type, and measured extraction represents only the cost of enforcement against dissenters. If constructed: the beneficiaries (disabled infants who receive protection, legal regimes that gain simplicity) are identifiable, and the constraint is a false summit disguised as natural law — reclassification candidate for tangled_rope or snare depending on how much suppression of alternative readings is required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_threshold, conceptual, 'Whether the birth threshold is discovered or invented.').

omega_variable(
    suppression_of_alternative_readings,
    'How much institutional suppression is required to keep fitness-contingent and potential-based readings excluded from legal/medical authority, and would that suppression count as part of the constraint''s operative extraction?',
    'Institutional history: do medical licensing boards explicitly prohibit fitness-based personhood assignment? Do legal systems prosecute homicide uniformly regardless of victim''s cognitive status, or do they permit discretion for severe disability? Are alternative readings actively rejected or merely dormant?',
    'High suppression of alternatives would elevate the measured suppression metric and suggest the constraint''s persistence depends on coercion rather than natural inevitability. Low suppression would support the mountain classification — the reading is simply more compelling than alternatives, not forced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings, empirical, 'Whether institutional power is required to maintain the birth-threshold reading against live alternatives.').

omega_variable(
    disabled_infant_protection_mechanism,
    'Does the birth-threshold reading protect disabled born infants, or does it coexist with medical authority to withhold treatment, nutrition, or palliative care from infants deemed to have poor prognosis?',
    'Clinical and legal practice audit: (1) do hospitals and courts mandate treatment of all born infants regardless of disability, or do they permit parents and clinicians to withhold life-sustaining care? (2) Does the personhood boundary translate to enforceable protection, or is it ceremonial? (3) Do disability rights advocates affirm the birth-threshold reading protects disabled infants, or do they report it coexists with discretionary non-treatment?',
    'If enforcement is weak and disabled infants remain vulnerable to non-treatment decisions, the constraint is theater rather than substantive protection — extraction is that beneficiary actors (legal regimes, medical authority) claim to protect all born humans while permitting discretionary harm. This would raise theater_ratio and suggest mandatrophy (founding problem unsolved). If enforcement is strong, the constraint genuinely protects the victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disabled_infant_protection_mechanism, empirical, 'Whether the birth-threshold rule translates to actual protection of disabled infants or remains performative.').

omega_variable(
    kernel_reading_genealogy,
    'Is the birth-threshold reading the original or dominant historical position, or a modern reconstruction responding to eugenic and fitness-based practices?',
    'Historical scholarship on the genealogy of the birth-threshold doctrine: was it always the majority reading, or did fitness-contingent readings dominate for centuries and get suppressed by modern human rights frameworks? Does the reading''s apparent naturalness mask modern institutional construction?',
    'If historically dominant: supports the mountain framing (the reading reflects long consensus). If modern reconstruction: suggests the reading is a recent institutional achievement defending against historical alternatives — a more complex constraint than natural law, possibly demanding vigilance to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_genealogy, empirical, 'Genealogy of the birth-threshold reading as doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__birth_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__birth_threshold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(pers_tr_t0, projected).
narrative_ontology:measurement(pers_tr_t8, personhood_boundary__birth_threshold_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(pers_tr_t8, observed).
narrative_ontology:measurement(pers_tr_t16, personhood_boundary__birth_threshold_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(pers_tr_t16, observed).
narrative_ontology:measurement(pers_tr_t24, personhood_boundary__birth_threshold_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(pers_tr_t24, observed).
narrative_ontology:measurement(pers_tr_t32, personhood_boundary__birth_threshold_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement_basis(pers_tr_t32, observed).
narrative_ontology:measurement(pers_tr_t40, personhood_boundary__birth_threshold_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(pers_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__birth_threshold_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(pers_be_t0, projected).
narrative_ontology:measurement(pers_be_t8, personhood_boundary__birth_threshold_reading, base_extractiveness, 8, 0.29).
narrative_ontology:measurement_basis(pers_be_t8, observed).
narrative_ontology:measurement(pers_be_t16, personhood_boundary__birth_threshold_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement_basis(pers_be_t16, observed).
narrative_ontology:measurement(pers_be_t24, personhood_boundary__birth_threshold_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(pers_be_t24, observed).
narrative_ontology:measurement(pers_be_t32, personhood_boundary__birth_threshold_reading, base_extractiveness, 32, 0.32).
narrative_ontology:measurement_basis(pers_be_t32, observed).
narrative_ontology:measurement(pers_be_t40, personhood_boundary__birth_threshold_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(pers_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__birth_threshold_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement_basis(pers_su_t0, projected).
narrative_ontology:measurement(pers_su_t8, personhood_boundary__birth_threshold_reading, suppression_requirement, 8, 0.25).
narrative_ontology:measurement_basis(pers_su_t8, observed).
narrative_ontology:measurement(pers_su_t16, personhood_boundary__birth_threshold_reading, suppression_requirement, 16, 0.26).
narrative_ontology:measurement_basis(pers_su_t16, observed).
narrative_ontology:measurement(pers_su_t24, personhood_boundary__birth_threshold_reading, suppression_requirement, 24, 0.27).
narrative_ontology:measurement_basis(pers_su_t24, observed).
narrative_ontology:measurement(pers_su_t32, personhood_boundary__birth_threshold_reading, suppression_requirement, 32, 0.28).
narrative_ontology:measurement_basis(pers_su_t32, observed).
narrative_ontology:measurement(pers_su_t40, personhood_boundary__birth_threshold_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(pers_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__birth_threshold_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__birth_threshold_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__fitness_contingent_reading).
narrative_ontology:affects_constraint(personhood_boundary__birth_threshold_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three constraint stories, one per live reading. Each reading instantiates a structurally distinct constraint with different ε, different beneficiary/victim sets, and different classification. Birth-threshold reading: universal personhood at birth, low extractiveness, mountain-type claim. Fitness-contingent reading: personhood contingent on demonstrated fitness, higher extractiveness, snare-type claim enabling selective harm. Potential-based reading: personhood grounded in capacity for rational agency, moderate extractiveness, tangled-rope-type claim. The three stories are related via kernel identity and reading_relations; they do NOT merge into one story with measurement-parameter variants. Each tells its own structural story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
