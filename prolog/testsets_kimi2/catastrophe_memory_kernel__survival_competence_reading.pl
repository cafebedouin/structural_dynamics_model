% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Catastrophe Memory Kernel â Survival Competence Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint treats catastrophe-commemoration ritual as a distributed
 *   training system: mourning practice encodes and rehearses operational
 *   behaviors (fasting discipline, rapid mobilization, underground logistics)
 *   that preserve community survival competence across generations of peace.
 *   The reading is contested by sibling framings that see the same ritual as
 *   boundary enforcement, trauma encoding, or symbol preservation. The
 *   constraint extracts present labor and assimilation-opportunity from
 *   members â especially marginal members under majority pressure â and
 *   converts it into a collective, future-contingent resilience benefit.
 *
 * KEY AGENTS:
 *   - community_elders: agenda_setter (organized/identity_locked) â maintain and transmit ritual practice
 *   - threatened_community: beneficiary (organized/constrained) â receives encoded survival competence
 *   - marginal_members: payer (powerless/constrained) â bears highest boundary-maintenance and assimilation-opportunity costs
 *   - surrounding_majority: excluded (powerful/mobile) â exerts assimilation pressure but absent from ritual decisions
 *   - ethnographers: observer (analytical) â provides external corroboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Catastrophe Memory Kernel â Survival Competence Reading").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'eeee4998-8935-44bf-95b3-134c14e244aa').
narrative_ontology:cs_kernel_codification('eeee4998-8935-44bf-95b3-134c14e244aa', distributed).
narrative_ontology:cs_authority_grounding('eeee4998-8935-44bf-95b3-134c14e244aa', practice).
narrative_ontology:cs_interpretation_layer_present('eeee4998-8935-44bf-95b3-134c14e244aa').
narrative_ontology:cs_reading_relation('eeee4998-8935-44bf-95b3-134c14e244aa', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeee4998-8935-44bf-95b3-134c14e244aa', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('eeee4998-8935-44bf-95b3-134c14e244aa', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('eeee4998-8935-44bf-95b3-134c14e244aa', foundational, ritual_preserves_operational_survival_competence).
narrative_ontology:cs_axiom_status(ritual_preserves_operational_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('eeee4998-8935-44bf-95b3-134c14e244aa', ritual_preserves_operational_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('eeee4998-8935-44bf-95b3-134c14e244aa', foundational, intergenerational_rehearsal_necessary_for_threat_response).
narrative_ontology:cs_axiom_status(intergenerational_rehearsal_necessary_for_threat_response, holdable).
narrative_ontology:cs_axiom_grounding('eeee4998-8935-44bf-95b3-134c14e244aa', intergenerational_rehearsal_necessary_for_threat_response, empirically_contingent).
narrative_ontology:cs_reference_frame('eeee4998-8935-44bf-95b3-134c14e244aa', ritualized_survival_competence).
narrative_ontology:cs_drift_state('eeee4998-8935-44bf-95b3-134c14e244aa', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eeee4998-8935-44bf-95b3-134c14e244aa', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, threatened_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, marginal_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the ritual calendar, adjudicate correct performance, and transmit catastrophe narratives as operational instruction. Their standing depends on continuity of practice; they organize intergenerational rehearsal but do not personally capture the extracted labor as private rent.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, community_elders, agenda_setter,
    organized, generational, identity_locked, regional).

% The collective body that inherits the encoded survival competence â historically validated patterns for mobilization, resource hoarding, and underground logistics rehearsed in ritual form. Members receive a future-contingent resilience benefit but must maintain distinctive practices that forego majority assimilation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, threatened_community, beneficiary,
    organized, generational, constrained, national).

% Members most exposed to assimilation pressure â economically integrated with the majority, geographically dispersed, or intermarried â who bear the steepest costs of maintaining ritual distinctiveness. They pay the boundary-maintenance tax most acutely while receiving the survival-competence benefit most abstractly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, marginal_members, payer,
    powerless, biographical, constrained, local).

% The majority society that exerts assimilation pressure through economic integration, legal frameworks, and cultural prestige. They are structurally absent from ritual decision-making but determine the opportunity cost of non-assimilation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, surrounding_majority, excluded,
    powerful, biographical, mobile, national).

% Academic observers who document the ritual's survival-relevant features and compare catastrophe outcomes across communities. They provide external corroboration for the founding problem but neither pay nor benefit from the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ethnographers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rehearses and transmits operational competence for catastrophe survival â resource hoarding, rapid mobilization, coded communication, fasting logistics â that would atrophy during periods of peace without ritualized repetition.
% TRANSFER_FUNCTION: Transfers present-time labor, economic opportunity, and social integration from individual members (especially marginal ones) to a collective, future-contingent survival capacity stored in intergenerational practice.
% ABSENT_VOICES: Assimilated former members who left due to boundary costs; surrounding majority society that applies assimilation pressure; secular community members who read the ritual as pure symbolism rather than operational training. They are not present when the ritual's survival necessity is asserted.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, the specific operational competencies â underground logistics, rapid mourning-to-mobilization transitions, dietary discipline under scarcity â would not be transmitted. The community would lose its historically validated catastrophe-preparedness and face higher mortality or dispersion under persecution.
% FOUNDING_PROBLEM: How to preserve operational knowledge for surviving catastrophe when the community experiences long periods of peace during which that knowledge atrophies and individual memory dies.
% FOUNDING_PROBLEM_CORROBORATION: Elder historians and catastrophe survivors within the community attest the ritual preserved life. External historians and anthropologists corroborate that ritualized communities show higher survival rates under persecution. Assimilated former members and secular critics dispute that the contemporary threat level justifies the ongoing boundary costs.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the ritual plausibly encodes survival-relevant competence but extracts present labor and foregone assimilation from members, with marginal members paying disproportionately. Suppression is moderate (0.52) because persistence depends partly on identity-lock and social sanction rather than pure voluntary participation. Theater ratio is moderate (0.42) because an increasing share of ritual activity may serve identity-display rather than operational rehearsal as the original threat recedes. Accessibility collapse is moderate (0.48) because assimilation alternatives remain visible but are socially costly. Resistance is moderate (0.44) because marginal members and assimilated exits exert ongoing pressure. The metric series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as necessary collective insurance and intergenerational duty, while the marginal payer seat experiences it as a steadily rising identity-tax with a distant and abstract benefit. The engine computes this divergence from the structural data: agenda-setters and core beneficiaries have constrained but institutionally embedded exits, while marginal members have the same formal exit options but face higher effective assimilation barriers.
 *
 * DIRECTIONALITY LOGIC:
 *   The threatened_community sits near the beneficiary end because it receives the collective survival benefit. Marginal_members sit near the target end because they bear the highest boundary-maintenance costs and assimilation pressure. Community_elders sit near the middle: they administer the constraint and their identity is fused with it, but they do not personally capture the extracted surplus. The surrounding_majority is excluded â their directionality is analytically irrelevant because they are not governed by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â knowledge atrophy during peace â is structurally genuine and historically corroborated. The classification as tangled rope rather than snare is warranted because the coordination function (survival competence transmission) is not cover: external evidence links the ritual to catastrophe survival. However, the constraint has drifted toward higher theater and the cost distribution is asymmetric, which prevents classification as pure rope. The mandatrophy question is whether the survival function is still live or has become a legitimizing cover for boundary maintenance; the contested founding_problem_status reflects this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the ritual primarily encode survival competence, or is the survival framing a post-hoc legitimization of boundary maintenance and identity preservation?',
    'Comparative historical analysis of catastrophe survival rates between communities with and without these ritual practices, controlling for confounding factors; ethnographic study of whether ritual content maps to actual survival operations.',
    'If the survival competence reading is empirically unsupported, the constraint''s coordination function collapses toward pure boundary maintenance or trauma encoding, increasing effective extraction and shifting classification toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the survival competence reading is empirically grounded or a legitimizing frame.').

omega_variable(
    survival_benefit_testability,
    'Is the claimed survival benefit of the ritual empirically testable and tested, or unfalsifiable communal narrative?',
    'Quantitative historical-demographic study of persecution-event outcomes; natural experiment from communities that abandoned the ritual.',
    'If the benefit is unfalsifiable, the coordination half of the tangled rope dissolves and the constraint is revealed as primarily extractive. If corroborated, the moderate extraction is the legitimate price of intergenerational insurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_benefit_testability, empirical, 'Empirical testability of the survival-competence claim.').

omega_variable(
    assimilation_cost_distribution,
    'Are the boundary-maintenance costs (assimilation pressure) borne primarily by a marginal subgroup, or distributed evenly across the community?',
    'Demographic and economic analysis of exit rates, intermarriage patterns, and wealth differentials between core and marginal community members.',
    'If costs concentrate on marginal members, effective extraction is higher than the base measure suggests and the constraint leans toward snare. If evenly distributed, the tangled rope framing holds as shared sacrifice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assimilation_cost_distribution, empirical, 'Distribution of boundary-maintenance costs across community members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_survival_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(catastrophe_survival_tr_t10, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(catastrophe_survival_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(catastrophe_survival_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(catastrophe_survival_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(catastrophe_survival_tr_t50, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(catastrophe_survival_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(catastrophe_survival_be_t10, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(catastrophe_survival_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(catastrophe_survival_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(catastrophe_survival_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(catastrophe_survival_be_t50, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 50, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_kernel__survival_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'catastrophe ritual' conflates four structurally distinct claims with different epsilon values, beneficiary structures, and functional outputs. The kernel decomposes into four sibling constraints (survival_competence_reading, boundary_maintenance_reading, symbol_continuity_reading, trauma_encoding_reading) because each reading assigns a different referent and structural relationship to the same ritual practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
