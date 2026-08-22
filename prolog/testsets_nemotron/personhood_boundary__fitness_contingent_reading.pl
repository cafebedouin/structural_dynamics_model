% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Personhood Boundary — Fitness-Contingent Reading
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures one reading of the contested
 *   personhood_boundary kernel: the fitness-contingent reading, which holds
 *   that moral personhood requires demonstrated capacities (rationality,
 *   autonomy, self-awareness, reciprocal engagement) and that entities
 *   lacking these capacities — notably pre-fitness infants and severely
 *   disabled persons — fall outside the moral community. The reading reached
 *   peak institutional power in mid-20th-century eugenics programs (Aktion
 *   T4, compulsory sterilization laws, institutionalization regimes) and
 *   persists in contemporary bioethics debates about 'after-birth abortion,'
 *   neonatal euthanasia protocols, and quality-of-life guardianship. The
 *   claimed type is snare: the coordination story (resource triage) is cover
 *   for a structure that extracts the most fundamental right — the right to
 *   have rights — from the most vulnerable, using state and medical
 *   enforcement to suppress alternatives (birth-threshold, potential-based,
 *   relational personhood).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.92).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.98).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Boundary — Fitness-Contingent Reading").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, 'b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3').
narrative_ontology:cs_kernel_codification('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', distributed).
narrative_ontology:cs_authority_grounding('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', extraction).
narrative_ontology:cs_reading_relation('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', personhood_boundary__potential_based_reading, influences).
narrative_ontology:cs_axiom('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', foundational, personhood_requires_demonstrated_fitness).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrated_fitness, holdable).
narrative_ontology:cs_axiom_grounding('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', personhood_requires_demonstrated_fitness, deontological).
narrative_ontology:cs_axiom('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', foundational, pre_fitness_entities_lack_moral_standing).
narrative_ontology:cs_axiom_status(pre_fitness_entities_lack_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', pre_fitness_entities_lack_moral_standing, deontological).
narrative_ontology:cs_reference_frame('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', pre_eugenics_personhood_consensus).
narrative_ontology:cs_drift_state('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', contemporary_bioethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b86e6450-ba1f-4d6f-a217-0cfc3ebf90f3', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_eugenics_apparatus).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, medical_selection_authorities).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, ableist_social_order).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_children).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, cognitively_impaired_adults).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, marginalized_families).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, moral_standing_requires_demonstrated_capacity).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, social_utility_as_personhood_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers fitness-testing protocols that determine which infants and disabled persons enter the moral community. Writes the criteria, operates the testing infrastructure, and authorizes exclusion. Extracts legitimacy and resource control from the power to define personhood. Faces no meaningful exit — it is the sovereign administrator of the boundary.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_eugenics_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Implement fitness assessments (developmental milestones, cognitive testing, 'quality of life' prognoses). Gain professional authority, research funding, and institutional prestige from their gatekeeping role. Can technically dissent but face career termination and professional ostracism — exit is constrained by identity investment in the medical selection paradigm.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, medical_selection_authorities, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, medical_selection_authorities, agenda_setter).

% The broader cultural and economic structure that treats demonstrated capacity as the price of admission to full humanity. Benefits from the exclusion of care burdens, the justification for resource allocation toward 'productive' lives, and the ideological coherence of meritocratic personhood. Exit would require dismantling the capacity-identity fusion that organizes social value — identity-locked at the structural level.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, ableist_social_order, beneficiary,
    organized, civilizational, identity_locked, national).

% Newborns and young children who have not yet passed the fitness threshold (developmental milestones, cognitive benchmarks, absence of severe disability). They bear the ultimate cost: denial of moral standing means their lives can be terminated, institutionalized, or experimented upon without legal consequence. No exit exists — they cannot speak, organize, or flee. The constraint constitutes their ontological vulnerability.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, immediate, trapped, local).

% Children who fail fitness tests permanently. Their personhood is permanently contested or denied. They bear extraction through: lethal 'treatment' withdrawal, segregation in institutions, denial of education, medical experimentation. Families who advocate for them face state pressure. No exit — the constraint defines them as outside the moral community by definition.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_children, payer,
    powerless, biographical, trapped, local).

% Adults who never passed or lost fitness status. Subject to guardianship that strips legal agency, involuntary sterilization, institutionalization, denial of medical care. The fitness-contingent reading makes their personhood revocable — a status that must be continuously performed. Trapped by the very criterion that excludes them.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, cognitively_impaired_adults, payer,
    powerless, biographical, trapped, local).

% Families of pre-fitness and disabled persons — disproportionately poor, racialized, and politically disenfranchised. Bear the emotional, economic, and legal costs of defending their children's personhood against the state apparatus. Their voices are excluded from the fitness-criterion debate; they are treated as biased by love. Exit is constrained by poverty and the same structural ableism that targets their children.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, marginalized_families, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, marginalized_families, excluded).

% Philosophers, historians, and ethicists who analyze the personhood boundary from outside the enforcement structure. They see the full architecture: the kernel (personhood_boundary), the three readings, and the structural consequences of each. Their exit is analytical — they can change frameworks but the constraint's operation continues regardless.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, moral_philosophy_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of resource allocation in scarcity by restricting full moral standing to those who demonstrate capacity for reciprocal social contribution. The 'fitness test' operates as a triage mechanism: society invests care, education, and legal protection only in entities that pass.
% TRANSFER_FUNCTION: Moves the full package of moral standing (right to life, bodily integrity, legal personhood, claim to social resources) FROM pre-fitness and disabled entities TO the state-medical apparatus and the ableist social order. The transfer is enforced through: lethal authority over non-persons, guardianship stripping, institutionalization pipelines, and the ideological naturalization of capacity-as-personhood.
% ABSENT_VOICES: The pre-fitness infants and severely disabled persons themselves — they cannot speak, organize, or testify. Their would-be advocates (marginalized families, disability rights organizers) are structurally excluded from the criterion-setting process by being labeled 'emotionally compromised.' The birth-threshold and potential-based readings' proponents are also absent from the fitness-contingent framework's internal deliberation — they are foreclosed by the reading's core premise.
% DISAPPEARANCE_RATIONALE: If the fitness-contingent reading vanished overnight, the state would lose its legal authority to deny personhood to infants and disabled persons based on capacity tests. The eugenics apparatus would lose its mandate. Medical selection authorities would lose their gatekeeping function. The ableist social order would face a legitimacy crisis. The world would rearrange toward either birth-threshold or potential-based personhood — both of which extend standing to at least some currently excluded entities.
% FOUNDING_PROBLEM: How should a society allocate scarce care resources and determine who counts as a full moral agent when not all human beings demonstrate the capacities traditionally associated with personhood (rationality, autonomy, reciprocity)?
% FOUNDING_PROBLEM_CORROBORATION: Proponents (Singer, Tooley, Giubilini/Minerva) attest the problem remains live — they argue scarce medical resources and the coherence of moral theory require capacity criteria. Opponents (disability rights movement, Catholic bioethics, human rights frameworks) attest the problem is a false framing: the scarcity is manufactured by the same social order that benefits from exclusion, and the 'traditional capacities' are themselves socially constructed. The disability rights movement's testimony — from outside the benefiting parties — corroborates that the founding problem is contested, not live.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is extremely high (0.92) because the constraint takes everything — life, liberty, legal standing — from its victims while delivering concentrated benefits (resource control, professional authority, ideological coherence) to its beneficiaries. Suppression is near-total (0.98) because the constraint's persistence depends on: (a) the physical impossibility of exit for pre-fitness infants, (b) the legal removal of personhood from disabled persons, (c) the professional and social ostracism of dissenters, and (d) the ideological naturalization of capacity-as-personhood that makes the boundary appear descriptive rather than constructed. Theater ratio is low (0.12) — the enforcement is overt and the extraction is the point, not a performance. Accessibility collapse is high (0.88) because once the fitness criterion is accepted, alternatives (birth, potential, relational personhood) appear logically incoherent within the framework. Resistance is near-zero (0.04) at the victim level — the constraint constitutes its victims as voiceless by definition.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute radically different effective extractions across seats. From the state apparatus seat (beneficiary, institutional power, arbitrage exit), the constraint appears as coordination — a necessary triage mechanism. From the pre-fitness infant seat (powerless, trapped, immediate horizon), it is total extraction with no coordination function whatsoever. From the marginalized family seat (moderate power, constrained exit, excluded role), it is a snare that extracts their children's lives while denying them standing to object. This seat divergence IS the measurement — the fitness-contingent reading's claim to be 'rational resource allocation' collapses when viewed from the victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The state eugenics apparatus and medical selection authorities are structural beneficiaries (d ≈ 0.1): they collect authority, resources, and legitimacy from operating the fitness boundary. The ableist social order is a diffuse beneficiary (d ≈ 0.25) — it extracts ideological coherence and resource allocation advantages but is identity-locked into the capacity-personhood fusion. All victim seats are full targets (d ≈ 1.0): pre-fitness infants, severely disabled children, cognitively impaired adults, and marginalized families bear the total extraction with zero exit. The observer seat (moral philosophy) sits at d ≈ 0.5 — it analyzes but does not bear or collect from the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resource triage under scarcity) is contested — disability rights testimony shows the scarcity is manufactured by the same order that benefits from exclusion. The constraint persists not because the problem remains live, but because the beneficiaries (state apparatus, medical authorities, ableist order) have captured the criterion-setting power. This is mandatrophy: the arrangement's original coordination function (if it ever had one) has been entirely displaced by extraction, yet the constraint persists through active enforcement and ideological naturalization. The 'fitness test' is no longer a response to scarcity — it is the mechanism that produces the scarcity it claims to manage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criterion_naturalness,
    'Is the fitness criterion (demonstrated rationality/autonomy/reciprocity) a genuine natural kind that tracks moral reality, or a constructed boundary that serves the ableist social order?',
    'Cross-cultural and historical analysis: if personhood boundaries vary radically across societies and track power structures rather than cognitive thresholds, the criterion is constructed. Convergent evolution of capacity criteria across independent traditions would support natural-kind status.',
    'If constructed, the constraint is a snare masquerading as a rope — the coordination function is post-hoc justification for extraction. If natural, the constraint may be a tragic rope (genuine coordination under scarcity) rather than a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fitness_criterion_naturalness, conceptual, 'Whether the fitness criterion reflects moral reality or power structure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the near-total suppression structural (state violence, legal denial) or internalized (families and disabled persons accepting the fitness criterion as legitimate)?',
    'Post-exit trajectory analysis: if suppression persists after the extractive mechanism is removed (e.g., in jurisdictions that have adopted birth-threshold personhood), reclassify as partially internalized. Disability rights movement''s own rejection of the criterion is evidence against full internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would amplify the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the fitness-contingent personhood boundary.').

omega_variable(
    kernel_framing_alternative,
    'Does the personhood_boundary kernel admit a fourth reading — relational personhood (personhood constituted by recognition relationships) — that would restructure the entire constraint family?',
    'Philosophical engagement with relational ontology (Kittay, Silvers, Scully) and empirical study of societies with relational personhood norms. If relational personhood coherently dissolves the fitness/birth/potential trichotomy, the kernel itself is under-specified.',
    'If a fourth reading exists, the current three-reading family is incomplete. The fitness-contingent reading''s extraction would be revealed as contingent on a specific (and contested) kernel framing, not on the kernel itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether the kernel admits a relational reading that dissolves the current trichotomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_tr_t1900, personhood_boundary__fitness_contingent_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_tr_t1920, personhood_boundary__fitness_contingent_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_tr_t1940, personhood_boundary__fitness_contingent_reading, theater_ratio, 1940, 0.08).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_tr_t1960, personhood_boundary__fitness_contingent_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_tr_t1980, personhood_boundary__fitness_contingent_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_tr_t2000, personhood_boundary__fitness_contingent_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_tr_t2024, personhood_boundary__fitness_contingent_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_be_t1900, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_be_t1920, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_be_t1940, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1940, 0.88).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_be_t1960, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1960, 0.72).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_be_t1980, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_be_t2000, personhood_boundary__fitness_contingent_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_be_t2024, personhood_boundary__fitness_contingent_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_su_t1900, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_su_t1920, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1920, 0.82).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_su_t1940, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1940, 0.99).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_su_t1960, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_su_t1980, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1980, 0.88).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_su_t2000, personhood_boundary__fitness_contingent_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(personhood_boundary__fitness_contingent_reading_su_t2024, personhood_boundary__fitness_contingent_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three constraint stories with radically different ε values. fitness_contingent_reading (this story) has ε ≈ 0.92 — it excludes the most entities and requires active enforcement. birth_threshold_reading has ε ≈ 0.05 — it grants standing automatically, requiring minimal enforcement. potential_based_reading has ε ≈ 0.45 — it excludes some disabled infants but includes most, with moderate enforcement. The fitness-contingent reading is the downstream extractive form; the birth-threshold reading is the upstream coordination form. The potential-based reading sits between them structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__fitness_contingent_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
