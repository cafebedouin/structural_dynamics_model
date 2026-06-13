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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the personhood-boundary
 *   kernel: the reading where personhood is contingent on demonstrated
 *   fitness — capacity for rational agency, cognitive function, or viability.
 *   Under this reading, pre-fitness entities (newborns before cognitive
 *   milestones, severely disabled persons without demonstrated rational
 *   capacity) lack moral standing until and unless they pass fitness tests
 *   set by adjudicators. The state authority and medical adjudicators benefit
 *   by having a principle for exclusion; pre-fitness entities and disabled
 *   populations bear the costs. This is structurally and historically a
 *   snare: it presents itself as rational resource allocation (the founding
 *   problem) but persists through enforcement of the exclusion, not through
 *   genuine problem-solving. The constraint is one committer-axis reading of
 *   a contested kernel; the birth-threshold reading and potential-based
 *   reading are SEPARATE CONSTRAINT STORIES with different ε values,
 *   different beneficiary sets, and different victim classifications. Each
 *   reading is generated independently, ε-invariant, and they are linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - fitness_adjudicators: Institutional authority that sets and enforces the fitness standard; benefits from control over moral standing
 *   - state_authority: Policy authority that enacts exclusion doctrine; benefits from simplified resource allocation
 *   - pre_fitness_infants: Powerless, trapped; excluded from moral standing until passage testing; complete dependence on others
 *   - severely_disabled_persons: Identity-locked; fitness criteria become a permanent bar to full moral standing; cannot exit by conforming
 *   - marginalized_populations: Organized but constrained; historically subjected to biased fitness testing; systematic structural exclusion
 *   - caregivers_and_parents: Moderate power, constrained exit; bear burden of navigating uncertainty about dependent's moral status
 *   - medical_ethics_observers: Analytical seat; external testimony to constraint's operation and historical harms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.89).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.91).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '0347414d-ca56-4815-a3bd-4005df2ba115').
narrative_ontology:cs_kernel_codification('0347414d-ca56-4815-a3bd-4005df2ba115', fixed_text).
narrative_ontology:cs_authority_grounding('0347414d-ca56-4815-a3bd-4005df2ba115', extraction).
narrative_ontology:cs_interpretation_layer_present('0347414d-ca56-4815-a3bd-4005df2ba115').
narrative_ontology:cs_reading_relation('0347414d-ca56-4815-a3bd-4005df2ba115', personhood_boundary__birth_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('0347414d-ca56-4815-a3bd-4005df2ba115', personhood_boundary__potential_based_reading, influences).
narrative_ontology:cs_axiom('0347414d-ca56-4815-a3bd-4005df2ba115', foundational, demonstrated_capacity_prerequisite).
narrative_ontology:cs_axiom_status(demonstrated_capacity_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('0347414d-ca56-4815-a3bd-4005df2ba115', demonstrated_capacity_prerequisite, deontological).
narrative_ontology:cs_axiom('0347414d-ca56-4815-a3bd-4005df2ba115', foundational, rational_agency_as_sine_qua_non).
narrative_ontology:cs_axiom_status(rational_agency_as_sine_qua_non, overridden).
narrative_ontology:cs_axiom_grounding('0347414d-ca56-4815-a3bd-4005df2ba115', rational_agency_as_sine_qua_non, empirically_contingent).
narrative_ontology:cs_reference_frame('0347414d-ca56-4815-a3bd-4005df2ba115', rational_triage_principle).
narrative_ontology:cs_drift_state('0347414d-ca56-4815-a3bd-4005df2ba115', contemporary_abundance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0347414d-ca56-4815-a3bd-4005df2ba115', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, fitness_adjudicators).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_infants).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_persons).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, marginalized_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, caregivers_and_parents).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, personhood_as_earned_status).
narrative_ontology:constraint_vindicates(personhood_boundary__fitness_contingent_reading, rational_agency_as_baseline_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicians, magistrates, or designated officials who assess whether an infant or disabled person meets the fitness criteria (cognitive capacity, physical viability, potential for rational agency). They possess structural authority to declare personhood status and set the threshold. Their power derives from controlling access to the moral community and the state apparatus that enforces exclusion.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, fitness_adjudicators, agenda_setter,
    institutional, generational, arbitrage, national).

% Enacts and enforces fitness-contingent personhood doctrine through law, policy, and institutional practice. Benefits by having a rationale for excluding certain populations from full moral standing and legal protection, simplifying administrative burden and justifying resource prioritization. Can revise the fitness criteria at will and experiences no direct cost from exclusion.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, state_authority, beneficiary).

% Newborns and infants who have not yet demonstrated the cognitive or behavioral markers of 'fitness.' Under this reading, they lack moral standing until passage testing or developmental milestones are achieved. They are completely dependent on caregivers, cannot advocate for themselves, and have no recourse if denied care, protection, or continued existence based on fitness judgments.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, pre_fitness_infants, excluded).

% People with intellectual disabilities, severe cognitive impairments, or conditions that prevent demonstrated rational agency face contingent moral standing. Fitness criteria may exclude them from protections, resource allocation, or continued institutional support. Their identity is fused with the capacity the constraint demands; to 'prove' fitness requires conforming to an external standard that may be impossible for them to meet.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_persons, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, severely_disabled_persons, excluded).

% Racial, ethnic, or class groups historically subjected to fitness testing (eugenics programs, intelligence testing biased toward dominant groups, medical racism). The fitness criterion becomes a vector for discrimination; demonstrated fitness is structured to exclude these populations even when objective performance is comparable. Exit requires leaving the jurisdiction or somehow overcoming systematic bias in assessment.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, marginalized_populations, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(personhood_boundary__fitness_contingent_reading, marginalized_populations, excluded).

% Bear the moral and practical burden of raising entities with uncertain personhood status. They must navigate uncertainty about whether their dependent has moral standing, may be required to facilitate fitness testing, and carry the emotional and ethical cost of conditional care. Their exit is constrained by law and by the dependence of their charges.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, caregivers_and_parents, payer,
    moderate, biographical, constrained, local).

% Professional and academic observers (physicians, ethicists, bioethicists) who document the constraint's operation and produce external testimony about harm and feasibility. They can testify to the gap between stated fitness criteria and their actual predictive value, and to historical harms from fitness-contingent policies.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, medical_ethics_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:fixing_cost_class(personhood_boundary__fitness_contingent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The constraint does not solve a coordination problem; it distributes exclusion. The stated function is to allocate moral standing and care resources rationally by restricting them to entities capable of rational agency. But coordination does not require exclusion — it requires alignment on shared principles.
% TRANSFER_FUNCTION: Transfers full moral standing from pre-fitness or unfit entities to the fitness adjudicators and state authority. Pre-fitness persons lose the claim to equal protection, full care, and continued existence; fitness adjudicators and the state gain the power to withhold these based on their criteria.
% ABSENT_VOICES: The pre-fitness persons and severely disabled persons are partially or wholly absent from the conversation that sets the fitness criteria. They cannot speak to whether the criteria are achievable, whether they matter to flourishing, or whether the exclusion reflects their own values. Parents and caregivers are present but structurally subordinate to adjudicators. Medical dissenters exist but are marginalized from policymaking authority.
% DISAPPEARANCE_RATIONALE: If personhood contingent on fitness vanished, pre-fitness infants would immediately regain default moral standing and full claim to protection and care regardless of demonstrated capacity. Resource allocation would shift: expensive care for infants with poor prognoses would not be withholdable on fitness grounds. The state's rationale for exclusion and triage would collapse, requiring alternative frameworks for allocation conflicts. Institutional authority structures based on fitness judgment would dissolve.
% FOUNDING_PROBLEM: How to allocate scarce medical and care resources fairly when not all entities can be saved. The fitness criterion offers a principle: direct care toward entities with reasonable prospect of achieving rational agency, minimizing 'wasted' resources on hopeless cases.
% FOUNDING_PROBLEM_CORROBORATION: The state authority and fitness adjudicators attest the problem is live and unresolved. Medical ethicists and disability advocates attest the founding problem is a rationalization for exclusion dressed as resource efficiency — that the actual problem (scarcity) could be solved by redistribution, that fitness criteria mispredict outcomes, and that the constraint persists as institutional convenience and historical inertia, not as solution. Historical analysis of eugenics programs and medical racism documents the constraint's persistent function as cover for elimination.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).

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
 *   Extractiveness is very high (0.89) because the constraint transfers moral standing away from powerless pre-fitness entities to institutional adjudicators — a complete asymmetry. Suppression is even higher (0.91) because the constraint's persistence depends on active enforcement (medical authority, state policy, institutionalized exclusion mechanisms) not on voluntary participation. The fitness criterion itself acts as suppression: it forecloses exit (an infant cannot 'choose' to develop faster; a disabled person cannot 'choose' to meet criteria designed to exclude them). Theater is moderate (0.42) because the constraint rides on a genuine scarcity problem (limited medical resources) but the stated solution (fitness triage) is cover for extraction: the actual problem could be solved by resource redistribution; the constraint persists because adjudicators benefit from the authority it grants. The temporal series shows extractiveness and suppression rising steeply from 0–50, then plateauing — this matches the historical pattern of fitness-contingent doctrines: initial deployment drives institutional entrenchment and normalization, then stabilize at high enforcement intensity. Theater rises more slowly and moderately, reflecting that the constraint must maintain plausible functional justification to persist (the cover story never becomes fully theatrical; it just becomes routine). All measurements are on a single shared time grid (0–100), every metric authored at every time point examined.
 *
 * PERSPECTIVAL GAP:
 *   The institutional beneficiary seats (adjudicators, state authority) and the victim seats should exhibit stark divergence. From the adjudicator position, the fitness criterion is a rational principle for allocating limited care resources — a coordinating principle, even. From the infant and disabled-person positions, the same structure is a mechanism for erasure: their moral standing is withheld at will, their care is contingent, their existence may be terminated based on an external judgment they cannot influence. The engine computes this divergence from the structural data: powerless + trapped + victim role yields high χ toward extraction; institutional + analytical + agenda-setter role yields low/negative χ. The claim (snare) and the metrics (high extractiveness, high suppression, moderate theater) align deliberately — this is an extractive constraint that justifies itself with a rational-allocation story.
 *
 * DIRECTIONALITY LOGIC:
 *   Fitness adjudicators (institutional power, analytical exit) are beneficiaries: they control the criterion, suffer no direct cost from exclusion, and gain authority and discretion. The state authority (institutional power) is beneficiary and agenda-setter: it enacts the exclusion, gains rationale for triage, experiences no extraction. Pre-fitness infants (powerless, trapped) are full-target victims: d approaches 1.0 — they cannot exit, cannot meet criteria set externally, completely depend on others' mercy. Severely disabled persons (moderate power, identity-locked exit) are also high-d victims: identity-locked because their core inability-to-conform is not a temporary condition they can exit; d is high because the constraint extracts their moral standing permanently. Marginalized populations (organized power, constrained exit) face systematic exclusion through biased fitness testing: their d is high because exit requires overcoming structural discrimination, not merely conforming. Caregivers (moderate power, constrained exit) face a dual position: they are payers (burden of care uncertainty) but also partially excluded from the decision-making seat. The directionality_overrides entry (moderate power, d=0.78) applies to the caregivers_and_parents seat — the structural derivation (moderate power + constrained exit + payer role) would compute to d≈0.65–0.70, but the moderate_power_override lifts it to 0.78 to reflect that caregivers are structurally closer to the extraction target than the modal moderate-power agent because their dependence on the fitness adjudicators is near-total.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (allocation of scarce resources) is live and real, but the constraint's solution (exclude the unfit) does not actually solve it — it merely redistributes who bears the scarcity. If the founding problem disappeared (abundance, infinite medical care), the constraint would lose its functional justification and should dissolve. But it shows no sign of dissolving: even in wealthy jurisdictions with abundant care capacity, fitness-contingent personhood doctrines persist in law and practice (advanced directives, withdrawal of care, selection against severe disability in prenatal testing). This indicates the constraint persists not because it solves the founding problem but because adjudicators benefit from the authority it grants. The temporal series supports this: extractiveness plateaus as resources increase, suppression does not relax, theater remains stable. A genuine solution to scarcity would show different dynamics — either extractiveness would fall (as the problem recedes) or the constraint would vanish (replaced by abundance-compatible structures). Mandatrophy is NOT fully resolved here; the constraint exhibits signs of persistence via inertia and authority capture (high theater is missing, but high suppression in the face of solved founding problem is present). This is a borderline piton/snare case where persistent extraction despite solved founding problem argues for institutional inertia, but the continued violent enforcement (suppression at 0.91) suggests the inertia is actively maintained by beneficiaries, making it snare-ish. Author it as snare because beneficiaries exist and actively benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criterion_measurability,
    'Can ''demonstrated fitness'' be measured objectively, or does the fitness criterion inevitably collapse into subjective judgment biased toward dominant groups?',
    'Historical analysis of fitness testing programs (IQ testing, eugenics assessments, medical triage) versus contemporary neurodevelopmental science. Do fitness measures predict actual capacity for agency, or do they predict conformity to adjudicators'' expectations?',
    'If fitness is unmeasurable objectively, the constraint becomes pure extraction dressed in rational language (the suppression mechanism is the ''objective'' criterion that is actually arbitrary). If measurable, the constraint might bind the adjudicators'' discretion and limit extraction. The measured high theater ratio (0.42) suggests bias is substantial but not absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fitness_criterion_measurability, empirical, 'Whether the fitness criterion measures actual capacity or merely conformity to adjudicators'' bias.').

omega_variable(
    identity_lock_mechanism_for_disabled,
    'Is the identity-lock exit status accurate for severely disabled persons, or can disabled persons exit through institutional advocacy, self-understanding change, or reframing of capacity?',
    'Longitudinal study of disabled persons who experience either integration into the moral community (through policy change, advocacy success, or personal identity shift) versus persistent exclusion. Does exit occur, and what enables or blocks it?',
    'If true identity-lock, the constraint is near-inescapable for this seat and χ approaches maximum. If exit is possible through framing change or collective action, d might be lower (constrained rather than identity-locked) and the constraint''s grip is weaker than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_disabled, empirical, 'Whether severely disabled persons experience exit as truly identity-locked or as constrained but capable of reframing.').

omega_variable(
    competing_kernel_reading_foreclosure,
    'Does the fitness-contingent reading logically foreclose the birth-threshold reading within a single coherent ethical framework, or can both readings coexist as live disagreements?',
    'Philosophical analysis: if a framework says ''personhood begins at birth'' (birth-threshold) can it also coherently say ''personhood requires demonstrated fitness'' (fitness-contingent)? Or are they logically incompatible premises?',
    'If forecloses: the birth-threshold and fitness-contingent readings cannot both be held within the same ethical system; a party must choose one, and the choice is irreversible. If coexists_with: the readings can be held simultaneously by different parties or sequentially in time (birth-threshold first, then fitness-test later). Sibling reading relations are presently authored as coexists_with; if foreclosure is true, network dependencies change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_kernel_reading_foreclosure, conceptual, 'Whether the fitness-contingent reading logically excludes the birth-threshold reading in a single framework.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.91) structural (enforcement machinery, policy exclusion, material barriers) or internalized (adjudicators and caregivers have absorbed the fitness doctrine as moral truth)?',
    'Post-policy-change observation: if fitness-contingent personhood law is repealed and enforcement machinery removed, does suppression persist in practice (internalized) or collapse (purely structural)?',
    'If structural, the constraint could be dismantled by policy change alone. If internalized, beneficiaries and even some victims may resist policy change because the fitness doctrine has become constitutive of their identity and moral worldview. Internalized suppression would indicate the constraint has metastasized beyond enforcement into the belief systems of all seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression persists through active enforcement machinery or through internalized belief in fitness doctrine.').

omega_variable(
    founding_problem_actually_solved_by_abundance,
    'In jurisdictions where medical resources became abundant (wealthy nations post-20th-century), did fitness-contingent personhood doctrines relax or disappear?',
    'Comparative legal and historical analysis: track personhood doctrine changes in wealthy versus resource-constrained jurisdictions over the interval. Does abundance correlate with relaxation of fitness criteria?',
    'If abundance correlates with persistence of fitness contingency, the founding problem (scarcity) is not the actual driver of the constraint — it persists for reasons of authority, tradition, or discrimination despite being no longer necessary. This is strong evidence of mandatrophy (founding problem dead, constraint alive due to institutional inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_actually_solved_by_abundance, empirical, 'Whether scarcity is the true founding problem or a rationalization for a constraint that persists for other reasons.').

omega_variable(
    alternative_reading_influences_this_one,
    'If the birth-threshold reading (all born humans have personhood) became dominant policy, would the fitness-contingent reading survive in niche or subordinate form, or would it be forced to abandon its core premise?',
    'Historical analysis of jurisdictions that adopted birth-threshold personhood law: do fitness-contingent doctrines persist informally, in special cases (disability, terminal illness, advanced directives) or are they genuinely superseded?',
    'If the reading survives in special cases, it has shifted from a general rule to an exception — the influences relation between readings is confirmed (birth-threshold influences fitness-contingent''s scope). If the reading is genuinely superseded, the relation might be stronger (approaches foreclosure). Understanding the relation type is essential for predicting how policy change in one reading cascades to siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_influences_this_one, empirical, 'The structural relation between fitness-contingent and birth-threshold readings: whether they coexist as live alternatives or one influences the other''s scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t0, personhood_boundary__fitness_contingent_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(pers_tr_t0, observed).
narrative_ontology:measurement(pers_tr_t12, personhood_boundary__fitness_contingent_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement_basis(pers_tr_t12, observed).
narrative_ontology:measurement(pers_tr_t25, personhood_boundary__fitness_contingent_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement_basis(pers_tr_t25, observed).
narrative_ontology:measurement(pers_tr_t37, personhood_boundary__fitness_contingent_reading, theater_ratio, 37, 0.39).
narrative_ontology:measurement_basis(pers_tr_t37, observed).
narrative_ontology:measurement(pers_tr_t50, personhood_boundary__fitness_contingent_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(pers_tr_t50, observed).
narrative_ontology:measurement(pers_tr_t62, personhood_boundary__fitness_contingent_reading, theater_ratio, 62, 0.41).
narrative_ontology:measurement_basis(pers_tr_t62, observed).
narrative_ontology:measurement(pers_tr_t75, personhood_boundary__fitness_contingent_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(pers_tr_t75, observed).
narrative_ontology:measurement(pers_tr_t100, personhood_boundary__fitness_contingent_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(pers_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(pers_be_t0, personhood_boundary__fitness_contingent_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(pers_be_t0, observed).
narrative_ontology:measurement(pers_be_t12, personhood_boundary__fitness_contingent_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement_basis(pers_be_t12, observed).
narrative_ontology:measurement(pers_be_t25, personhood_boundary__fitness_contingent_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(pers_be_t25, observed).
narrative_ontology:measurement(pers_be_t37, personhood_boundary__fitness_contingent_reading, base_extractiveness, 37, 0.85).
narrative_ontology:measurement_basis(pers_be_t37, observed).
narrative_ontology:measurement(pers_be_t50, personhood_boundary__fitness_contingent_reading, base_extractiveness, 50, 0.87).
narrative_ontology:measurement_basis(pers_be_t50, observed).
narrative_ontology:measurement(pers_be_t62, personhood_boundary__fitness_contingent_reading, base_extractiveness, 62, 0.88).
narrative_ontology:measurement_basis(pers_be_t62, observed).
narrative_ontology:measurement(pers_be_t75, personhood_boundary__fitness_contingent_reading, base_extractiveness, 75, 0.89).
narrative_ontology:measurement_basis(pers_be_t75, observed).
narrative_ontology:measurement(pers_be_t100, personhood_boundary__fitness_contingent_reading, base_extractiveness, 100, 0.89).
narrative_ontology:measurement_basis(pers_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t0, personhood_boundary__fitness_contingent_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(pers_su_t0, observed).
narrative_ontology:measurement(pers_su_t12, personhood_boundary__fitness_contingent_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement_basis(pers_su_t12, observed).
narrative_ontology:measurement(pers_su_t25, personhood_boundary__fitness_contingent_reading, suppression_requirement, 25, 0.85).
narrative_ontology:measurement_basis(pers_su_t25, observed).
narrative_ontology:measurement(pers_su_t37, personhood_boundary__fitness_contingent_reading, suppression_requirement, 37, 0.88).
narrative_ontology:measurement_basis(pers_su_t37, observed).
narrative_ontology:measurement(pers_su_t50, personhood_boundary__fitness_contingent_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement_basis(pers_su_t50, observed).
narrative_ontology:measurement(pers_su_t62, personhood_boundary__fitness_contingent_reading, suppression_requirement, 62, 0.91).
narrative_ontology:measurement_basis(pers_su_t62, observed).
narrative_ontology:measurement(pers_su_t75, personhood_boundary__fitness_contingent_reading, suppression_requirement, 75, 0.91).
narrative_ontology:measurement_basis(pers_su_t75, observed).
narrative_ontology:measurement(pers_su_t100, personhood_boundary__fitness_contingent_reading, suppression_requirement, 100, 0.91).
narrative_ontology:measurement_basis(pers_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(personhood_boundary__fitness_contingent_reading, 0.12).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__birth_threshold_reading).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, personhood_boundary__potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested personhood-boundary kernel. The fitness-contingent reading (this file) differs structurally from the birth-threshold reading and potential-based reading in where the personhood threshold is placed: demonstrated capacity (this reading) versus birth event versus potential-for-capacity. The three readings have different ε values, different victim sets, different beneficiary structures, and different compliance mechanisms. They are NOT different measurements of one constraint; they are different commitments to what personhood IS. All three readings affect each other through policy influence and jurisdictional variation, but none logically forecloses the others — they coexist as live disagreements across different ethical traditions and legal systems. See omega variables for details on relation types and committer-axis structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__fitness_contingent_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
