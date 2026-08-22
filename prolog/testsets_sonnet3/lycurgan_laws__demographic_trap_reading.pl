% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Immutability as Demographic Death Spiral
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the demographic-trap reading of the Lycurgan
 *   kernel: the constitutional order attributed to Lycurgus — kleros land
 *   allotment, the agoge, the syssitia mess-contribution requirement, and the
 *   fixed citizenship boundary — is read as a brittle system whose formal
 *   unrevisability, rather than any external shock, converted a workable
 *   coordination device into a slow-moving demographic collapse. Under this
 *   reading the immutability itself is the extraction mechanism: as partible
 *   inheritance and dowry practices fragmented kleros holdings across
 *   generations, families who fell below the mess-contribution threshold lost
 *   citizenship permanently, with no institutional mechanism to re-equalize
 *   holdings, admit qualified perioikoi, or adjust the threshold — because
 *   doing so would concede the laws were revisable at all. The oliganthropia
 *   (citizen shortage) that Aristotle documents and that later reformers Agis
 *   IV and Cleomenes III tried and failed to reverse is, on this reading, the
 *   predictable terminus of a system that could not update its own membership
 *   rules.
 *
 * KEY AGENTS:
 *   - homoioi_incumbent_families: entrenched beneficiaries whose land and status the rigid rule protects, even as the same rule eventually claims their own descendants
 *   - ephorate_and_gerousia: the enforcing institution, structurally unable to use its own reform powers without undermining the sacral legitimacy it depends on
 *   - hypomeiones_declining_citizens and impoverished_spartiate_lineages: the direct victims, citizens by birth stripped of status by an unrevisable economic threshold
 *   - helot_population: bears the compounding security cost of a shrinking ruling class
 *   - perioikoi_communities: an available but permanently excluded replenishment population
 *   - later_historians_and_reformers: the analytical seat corroborating the trap from outside Spartan self-mythology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.72).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.81).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Immutability as Demographic Death Spiral").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, 'bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3').
narrative_ontology:cs_kernel_codification('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', fixed_text).
narrative_ontology:cs_authority_grounding('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', lineage).
narrative_ontology:cs_interpretation_layer_present('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3').
narrative_ontology:cs_reading_relation('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', lycurgan_laws__sacral_fidelity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', lycurgan_laws__adaptive_fiction_reading, influences).
narrative_ontology:cs_axiom('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', foundational, unrevisability_is_operationally_binding).
narrative_ontology:cs_axiom_status(unrevisability_is_operationally_binding, holdable).
narrative_ontology:cs_axiom_grounding('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', unrevisability_is_operationally_binding, empirically_contingent).
narrative_ontology:cs_axiom('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', secondary, rigid_membership_rules_generate_terminal_extraction_absent_correction_mechanism).
narrative_ontology:cs_axiom_status(rigid_membership_rules_generate_terminal_extraction_absent_correction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', rigid_membership_rules_generate_terminal_extraction_absent_correction_mechanism, instrumental).
narrative_ontology:cs_reference_frame('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', lycurgan_founding_settlement).
narrative_ontology:cs_drift_state('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', fourth_century_oliganthropia_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('bda6cb09-3de8-4dbe-a9ac-39d39b6bbcd3', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_families).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, ephorate_and_gerousia).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, hypomeiones_declining_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, impoverished_spartiate_lineages).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helot_population).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, perioikoi_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the largest surviving kleros land allotments and the social standing of full Spartiate citizenship. Benefit from a rigid membership rule that keeps competitors from being admitted or land from being redistributed, but are themselves bound by the same equal-contribution mess requirement that will eventually bankrupt their own weaker heirs through partible inheritance and dowry inflation.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_families, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_families, agenda_setter).

% Administer and enforce the agoge, the mess contributions, and the citizenship-forfeiture rule. Their entire legitimacy rests on the claim that the Lycurgan order is untouchable; they have the formal power to reform the kleros allocation or contribution thresholds but treat any such move as sacrilege, so the machinery that could fix the collapse is run by the people most institutionally committed to never using it.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephorate_and_gerousia, agenda_setter,
    institutional, civilizational, identity_locked, regional).

% Spartiate-born men who can no longer meet the syssitia mess contribution because their kleros has fragmented across generations of inheritance and dowry payment. They fall out of full citizenship into an inferior status with no path back, despite the polis needing every hoplite it can field; the rule that strips them is the same rule their fathers benefited from.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, hypomeiones_declining_citizens, payer,
    moderate, biographical, trapped, local).

% Families whose land holdings have shrunk below the viable threshold across successive generations. They cannot legally sell, consolidate, or acquire additional kleros to restore viability because land tenure and inheritance rules are treated as fixed; their children either fail the agoge sponsorship or cannot marry advantageously, accelerating the family's exit from the citizen body entirely.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, impoverished_spartiate_lineages, payer,
    powerless, generational, trapped, local).

% Bound agricultural laborers whose subjugation is what frees Spartiate men to train full-time for war. As the citizen body shrinks, the ratio of helots to Spartiates worsens, intensifying the krypteia and periodic terror used to keep the vastly larger subject population from revolting; they bear the security cost of a shrinking ruling class that refuses to change its own reproduction and inheritance rules.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_population, payer,
    powerless, civilizational, trapped, regional).

% Free but non-citizen townsfolk who supply craft goods, some military manpower, and trade the Spartan economy structurally needs but who have no path to citizenship regardless of wealth or service, because the citizenship boundary is treated as fixed by ancestral law. Their exclusion means the shrinking Spartiate body cannot be replenished from an obvious adjacent population.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, perioikoi_communities, excluded,
    moderate, generational, constrained, regional).

% Ancient and modern analysts (Aristotle's Politics critique of Spartan land law, later reform attempts under Agis IV and Cleomenes III) who trace the oliganthropia — the citizen shortage — directly to the inheritance and mess-contribution rules and to the refusal to revise them even as the crisis became visible generations before Leuctra.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, later_historians_and_reformers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, homoioi_incumbent_families).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The original kleros allotment and agoge system solved a genuine coordination problem: producing a standing body of full-time, materially equal hoplite citizens who could not be economically differentiated into factions, binding the polis together against a much larger subject population.
% TRANSFER_FUNCTION: Over generations the arrangement transfers citizenship itself away from families whose land fragments below the mess-contribution threshold, concentrating land and status among fewer incumbent lineages while the total pool of eligible citizens shrinks; it also transfers security costs onto the helot population as the citizen-to-helot ratio worsens.
% ABSENT_VOICES: The hypomeiones losing status, the perioikoi who could have been naturalized to replenish numbers, and the helots bearing intensified control are not voices in the ephors' or gerousia's deliberations; the sacral framing of the laws as ancestral and divine forecloses the kind of open debate in which they could be heard.
% DISAPPEARANCE_RATIONALE: Had the inheritance rules, mess-contribution threshold, or citizenship boundary been revisable, kleros consolidation could have been checked, perioikoi or promoted helots could have been naturalized to replenish the citizen body, and the oliganthropia crisis documented by Aristotle would likely not have progressed to the point where Sparta could field only a few thousand homoioi by the 4th century BCE — the entire manpower base of Spartan military and political power depended on the numbers this constraint bled away.
% FOUNDING_PROBLEM: Archaic Sparta needed a stable, materially undifferentiated citizen-soldier class immune to the factional strife that destabilized other poleis, achieved by fixing land allotments and status rules so no citizen could out-accumulate another.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle's Politics (Book II) explicitly analyzes Spartan land law and the oliganthropia as a structural defect stemming from unequal inheritance under a nominally equal system, writing from outside the Spartan citizen body and with no stake in defending Lycurgan legitimacy; the later reform attempts of kings Agis IV and Cleomenes III, from inside the Spartan royal institution itself, corroborate that the founding equality problem had already failed and the immutability was itself identified as the obstacle to fixing it.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 to 0.72 over the four-century interval because the mechanism is genuinely slow: in the archaic and early classical periods the equal-kleros system functions closely to its coordination promise, and status loss is rare. As inheritance fragmentation compounds across generations, an increasing share of the citizen body is extracted from (stripped of status, land, or military standing) with no corresponding release valve. Theater ratio climbs alongside it (0.12 to 0.42) as the sacral rhetoric of Lycurgan permanence intensifies precisely as the material system it describes visibly fails — invoking ancestral sanctity substitutes for the reform that would actually address the shrinking citizen rolls. Suppression climbs from 0.55 to 0.81 as the krypteia and control apparatus over helots must intensify to compensate for a shrinking Spartiate manpower base relative to a stable or growing subject population.
 *
 * DIRECTIONALITY LOGIC:
 *   Homoioi incumbent families and the ephorate/gerousia sit near the beneficiary end: they hold the land, run the enforcement machinery, and their institutional standing depends on the rule's sanctity. Hypomeiones and impoverished lineages sit near the full-target end: they are structurally trapped by identity (Spartiate birth defines their whole social existence, making exit unthinkable even as citizenship is stripped) and by law (no legal path to requalify). Helots are targets of an intensifying control apparatus that is a downstream cost of the citizen shortage, not a direct party to the kleros rule, hence identity_locked exit is not assigned to them — they are simply trapped by chattel status. Perioikoi are excluded rather than extracted from directly; the constraint's cost to them is the foreclosed option of naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing factional inequality among citizen-soldiers — was real and, for a period, solved. Classifying this as snare rather than mountain or rope prevents two symmetric errors: treating the decline as pure bad luck (obscuring that the immutability rule itself is the mechanism preventing correction) and treating the original Lycurgan settlement as always-already extractive (obscuring that it functioned as genuine coordination before land fragmentation exceeded the system's designed tolerances). The mandatrophy is resolved in the sense that the founding problem is dead — Sparta by the 4th century BCE no longer needed to worry about maintaining equality among a citizen body it could barely field — yet the unrevisability persisted and continued extracting citizenship from the shrinking margin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_operationally_real_or_narrative_only,
    'Was Lycurgan unrevisability an operationally binding constraint on Spartan governance, or primarily a legitimating narrative behind which more adaptive practice occurred (as the adaptive_fiction_reading holds)?',
    'Comparative institutional history: documented instances (or their absence) of covert land reallocation, informal citizenship grants, or quiet threshold adjustments in the archaeological and epigraphic record, cross-checked against the population decline curve — if adaptations occurred but were insufficient in scale, the trap reading and the fiction reading are compatible at different magnitudes; if no adaptation occurred at all, the trap reading is the stronger structural account.',
    'If unrevisability was substantially fictional and covert adaptation occurred but was simply insufficient, this reading''s snare classification should be read as a rate-of-adaptation failure rather than a pure suppression mechanism, shifting it toward a tangled_rope where partial coordination persisted alongside partial extraction. If unrevisability was operationally real, the snare classification stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_operationally_real_or_narrative_only, conceptual, 'Whether the kernel''s unrevisability was structurally binding or a legitimating fiction over adaptive practice.').

omega_variable(
    demographic_causation_sufficiency,
    'Is constitutional unrevisability sufficient by itself to explain the oliganthropia, or did external shocks (the 464 BCE earthquake, repeated helot revolts, the human cost of the Peloponnesian War) do most of the causal work, with the legal rigidity only preventing recovery rather than causing the decline?',
    'Demographic modeling comparing citizen population trajectories against dated external shock events versus the steady background rate of kleros fragmentation under partible inheritance; a trajectory dominated by shock-correlated step declines rather than smooth generational decay would favor the exogenous-shock account.',
    'If external shocks dominate, this reading''s extractiveness attribution to the constitutional structure itself is overstated and the constraint functions more as an amplifier of exogenous shocks than a standalone extraction mechanism, which would lower the authored epsilon on causal-attribution grounds. If the decline is dominated by the smooth, shock-independent inheritance dynamic, the trap reading''s causal claim is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_causation_sufficiency, empirical, 'Whether the demographic collapse is primarily endogenous to the legal structure or primarily driven by external shocks the structure merely failed to absorb.').

omega_variable(
    kernel_reading_incommensurability,
    'Do the three kernel readings (demographic_trap, sacral_fidelity, adaptive_fiction) describe the same historical Sparta under different evaluative frames, or do they make incompatible empirical claims about what Spartan institutions actually did?',
    'This is inherent to the committer structure: each reading is authored as its own constraint with its own epsilon per the ε-invariance principle. Resolution would require establishing, independently of framing, what the ephorate and gerousia actually did with land and citizenship rules across the classical period — a question the fragmentary source record (largely later, idealizing sources like Plutarch and Xenophon) may not permit definitively resolving.',
    'If the readings are empirically incommensurable (make different factual claims), only one can be historically correct even though all three remain authored as valid constraint stories per the kernel-reading convention; if they are evaluatively incommensurable (agree on facts, differ on normative framing), they can coexist as this schema''s coexists_with relation asserts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the three sibling kernel readings differ empirically or merely evaluatively.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__demographic_trap_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__demographic_trap_reading, theater_ratio, 160, 0.27).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__demographic_trap_reading, theater_ratio, 240, 0.34).
narrative_ontology:measurement(lycu_tr_t320, lycurgan_laws__demographic_trap_reading, theater_ratio, 320, 0.39).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__demographic_trap_reading, theater_ratio, 400, 0.42).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__demographic_trap_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__demographic_trap_reading, base_extractiveness, 160, 0.5).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__demographic_trap_reading, base_extractiveness, 240, 0.6).
narrative_ontology:measurement(lycu_be_t320, lycurgan_laws__demographic_trap_reading, base_extractiveness, 320, 0.68).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__demographic_trap_reading, base_extractiveness, 400, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__demographic_trap_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(lycu_su_t160, lycurgan_laws__demographic_trap_reading, suppression_requirement, 160, 0.68).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__demographic_trap_reading, suppression_requirement, 240, 0.73).
narrative_ontology:measurement(lycu_su_t320, lycurgan_laws__demographic_trap_reading, suppression_requirement, 320, 0.78).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__demographic_trap_reading, suppression_requirement, 400, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the lycurgan_laws kernel, decomposed per the epsilon-invariance principle because the natural-language label 'Lycurgan constitution' conflates structurally distinct claims: whether the laws were divinely fixed and required fidelity (sacral_fidelity_reading, low authored extraction, near-mountain), whether apparent immutability masked covert adaptive practice (adaptive_fiction_reading, moderate extraction concentrated in the gap between public myth and private practice), or whether the immutability was operationally real and structurally caused demographic collapse (this reading, high extraction, snare classification). Each carries its own epsilon; they are linked here rather than merged because averaging or parameterizing a single 'Lycurgan constraint' across these claims would violate the requirement that epsilon be an intrinsic, non-observer-relative property of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
