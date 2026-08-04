% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Abolitionist Animal Rights: Inherent Value Precluding Instrumental Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the abolitionist reading of the animal
 *   status kernel: non-human animals possess inherent value and rights that
 *   categorically preclude all instrumental human use. The reading contests
 *   the property and welfare readings as legitimating exploitation. Animals
 *   are structurally positioned as the victim class of any instrumental use
 *   arrangement, while the abolitionist norm itself carries near-zero
 *   extractiveness — it functions as a protective moral boundary rather than
 *   a rent-seeking mechanism. The constraint is claimed as mountain because
 *   the reading presents inherent value as a moral fact discoverable by
 *   reason, not as a constructed coordination device.
 *
 * KEY AGENTS:
 *   - Animals (powerless/trapped) — bear all costs of instrumental use across food, research, and entertainment systems
 *   - Animal use industries (institutional/mobile) — resist reclassification and maintain property/welfare regimes
 *   - Abolitionist advocates (organized/analytical) — advance the rights framework and are excluded from dominant policy tables
 *   - Welfare reform institutions (institutional/constrained) — administer the regulatory alternative that abolitionists reject as legitimation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.02).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.15).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, mountain).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Abolitionist Animal Rights: Inherent Value Precluding Instrumental Use").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '24f0d5d7-45bf-4e81-a1d5-9864d5a61559').
narrative_ontology:cs_kernel_codification('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', formalized).
narrative_ontology:cs_authority_grounding('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', lineage).
narrative_ontology:cs_interpretation_layer_present('24f0d5d7-45bf-4e81-a1d5-9864d5a61559').
narrative_ontology:cs_reading_relation('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', foundational, inherent_value_precludes_instrumental_use).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', inherent_value_precludes_instrumental_use, deontological).
narrative_ontology:cs_axiom('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', foundational, rights_bearing_status_non_derogable).
narrative_ontology:cs_axiom_status(rights_bearing_status_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', rights_bearing_status_non_derogable, deontological).
narrative_ontology:cs_reference_frame('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', inherent_value_moral_baseline).
narrative_ontology:cs_drift_state('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', contemporary_industrial_use_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('24f0d5d7-45bf-4e81-a1d5-9864d5a61559', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, animals).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, inherent_value_theory).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, abolitionist_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the full costs of instrumental use across food, research, entertainment, and labor systems; are classified as legal property or welfare-regulated objects under competing readings, with no exit from human use regimes under current global practice.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animals, payer,
    powerless, immediate, trapped, global).

% Depend on the legal classification of animals as use-permissible objects; would face existential restructuring if the abolitionist constraint were instantiated; currently dominate policy discourse and regulatory capture to maintain property status.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_use_industries, excluded,
    institutional, generational, mobile, global).

% Advance the rights-holder framework through philosophical argument, legal challenge, and direct action; structurally marginalized in policy forums dominated by welfare and property readings; seek total abolition of instrumental use categories rather than reform.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, observer,
    organized, generational, analytical, global).

% Administer regulatory frameworks that constrain but permit instrumental use; their reforms are explicitly rejected by abolitionists as legitimating the underlying property status; represent the dominant policy alternative to abolition and control the mainstream regulatory agenda.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_institutions, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a categorical boundary against treating sentient beings as instruments, coordinating human conduct away from use categories by positing non-derogable moral status that cannot be traded against human preference.
% TRANSFER_FUNCTION: No transfer of resources or harms; the constraint blocks the historical transfer of labor, body parts, and suffering from animals to human users, ending the extraction flow without redirecting it.
% ABSENT_VOICES: Animal use industries and dependent rural communities are present in economic discourse but their objections are framed as efficiency concerns rather than moral claims; abolitionist voices are structurally excluded from mainstream regulatory tables where welfare reform is treated as the moral ceiling.
% DISAPPEARANCE_RATIONALE: If the abolitionist constraint vanished from legal and moral discourse, the remaining welfare and property readings would expand to fill the space unchallenged; instrumental use would lose its categorical opponent and welfare reform would operate as the uncontested moral limit, rearranging the advocacy landscape and regulatory legitimacy.
% FOUNDING_PROBLEM: Sentient beings are subjected to systematic instrumental use across food, research, entertainment, and labor systems, treated as renewable resources or welfare-managed property rather than as rights-holders.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist ethicists, animal law scholars, and veterinary whistleblowers attest to the ongoing scope of instrumental use; industry production statistics and regulatory inspection reports corroborate the scale from seats outside the abolitionist advocacy framework.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status__abolitionist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status__abolitionist_reading),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the constraint itself blocks rather than extracts; suppression is low (0.15) because the constraint's force is presented as normative recognition rather than coercion; accessibility_collapse is high (0.92) because acceptance of inherent value collapses the legitimacy of instrumental use alternatives; resistance is high (0.68) because the constraint faces massive opposition from entrenched use industries and cultural practice. Claim and metrics are authored independently: the reading claims mountain (moral natural law) while metrics honestly report high resistance and low instantiation.
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist observer seat, the constraint is a protective moral floor that would terminate systemic extraction; from the animal use industry seat, it represents an existential threat to legitimate economic activity. The engine computes this divergence from structural data: industries have institutional power and mobile exit (could pivot to non-animal models), while animals have no power and trapped exit. Welfare institutions experience the constraint as delegitimating their entire reform project.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are declared in the victim set because every instrumental use arrangement extracts from them — they bear the cost with no exit. No beneficiaries are declared on this mountain to avoid false-summit misclassification; the constraint protects without concentrating rents in any human seat. Animal use industries sit at moderate directionality as excluded agenda-setters whose practices the constraint would abolish. Abolitionist advocates occupy the analytical seat with near-zero directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading prevents mandatrophy misclassification by explicitly rejecting welfare reforms as legitimation of continued use. The founding problem — systematic instrumental use of sentient beings — is judged still live because welfare reforms leave the property structure intact. A classification that treated welfare reform as resolving the problem would commit mandatrophy by declaring the arrangement transitional or scaffold-like; the abolitionist reading insists it is not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_mountain_or_constructed_norm,
    'Is the inherent value of non-human animals a discovered moral fact or a socially constructed normative commitment?',
    'Cross-cultural moral anthropology and historical sociology: discovered facts should show cross-cultural convergence; constructed norms should show culturally specific boundary variation and revision.',
    'If constructed, the mountain classification fails and the constraint reverts to identity_coordination or rope; if discovered, mountain classification is structurally warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_mountain_or_constructed_norm, conceptual, 'Whether inherent value is natural moral law or constructed norm').

omega_variable(
    institutionalization_trajectory,
    'Can a moral mountain that currently lacks broad institutional enforcement maintain its classification if it achieves legal codification without becoming extractive?',
    'Track post-codification metrics over a 20-year interval: if legal instantiation introduces concentrated enforcement benefits, rising theater_ratio, or extraction accumulation, the constraint has shifted type.',
    'Legal codification could shift the constraint from mountain to tangled_rope if enforcement generates beneficiary capture or suppression exceeds coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutionalization_trajectory, empirical, 'Trajectory from moral claim to legal institution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(anim_tr_t25, animal_status__abolitionist_reading, theater_ratio, 25, 0.06).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(anim_be_t25, animal_status__abolitionist_reading, base_extractiveness, 25, 0.02).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__abolitionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct constraints per the ε-invariance principle. The property reading treats animals as legal objects with high extraction; the welfare reading treats them as sentient interests with moderate extraction and coordination; the abolitionist reading treats them as rights-holders with near-zero extraction. Each asserts a different ε and stakeholder structure over the same domain and must be authored separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
