% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor-Violence Legitimacy (Composite: Drop + Contraction)
 *   domain: social/legal/historical
 *
 * SUMMARY:
 *   The legitimacy of honor-violence (dueling) declined in Western European
 *   and American legal systems between the 18th and 20th centuries. This
 *   composite reading claims that TWO structurally distinct mechanisms
 *   operated simultaneously and reinforced each other: (1) DROP — external
 *   costs (legal penalties, loss of property, family economic ruin, social
 *   ostracism) made violence increasingly impractical and expensive,
 *   constraining the behavior without changing the underlying framework's
 *   legitimacy; (2) CONTRACTION — the conceptual definition of honor itself
 *   was redefined to exclude violence, making violent conduct inherently
 *   dishonorable and therefore structurally unthinkable for those who
 *   internalized the new framework. The DROP mechanism creates a temporary
 *   coordination problem (violence-practitioners pay high costs; custodians
 *   maintain the old framework). The CONTRACTION mechanism creates a
 *   permanent identity redefinition (violent practitioners cannot maintain
 *   self-conception as honorable). This reading instantiates the COMPOSITE
 *   operation of both, making neither mechanism alone sufficient.
 *
 * KEY AGENTS:
 *   - honor_system_custodians: Elite legal and cultural authorities who define honor and enforce the transition (institutional power, constrained exit — define the new framework and collect authority)
 *   - violent_practitioners: Nobles and gentlemen for whom dueling WAS honor; now face legal and social costs PLUS ideological delegitimacy (powerful but constrained exit — caught between old identity and new framework)
 *   - non_elite_duelists: Military officers and professionals who adopted dueling for status; lack power to resist redefinition (moderate power, identity_locked exit — most vulnerable to double extraction)
 *   - legal_enforcement_apparatus: Courts and state authorities that prosecute dueling and gain institutional power (institutional power, agenda-setter — beneficiary and enforcer)
 *   - cultural_commentators: Philosophers and writers who narrate the transition and make contraction intellectually coherent (moderate power, mobile exit — bridge the two mechanisms)
 *   - excluded_honor_claimants: Women and non-elite who were never in the old system but gain access through the new framework (powerless, trapped exit — beneficiaries they are, but without voice in the transition)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.67).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.72).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor-Violence Legitimacy (Composite: Drop + Contraction)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "social/legal/historical").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'cf158c65-aa4d-49b0-a4fe-a2a149451b0e').
narrative_ontology:cs_kernel_codification('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', distributed).
narrative_ontology:cs_authority_grounding('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', extraction).
narrative_ontology:cs_interpretation_layer_present('cf158c65-aa4d-49b0-a4fe-a2a149451b0e').
narrative_ontology:cs_reading_relation('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', foundational, external_costs_necessary).
narrative_ontology:cs_axiom_status(external_costs_necessary, holdable).
narrative_ontology:cs_axiom_grounding('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', external_costs_necessary, empirically_contingent).
narrative_ontology:cs_axiom('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', foundational, conceptual_redefinition_necessary).
narrative_ontology:cs_axiom_status(conceptual_redefinition_necessary, holdable).
narrative_ontology:cs_axiom_grounding('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', conceptual_redefinition_necessary, deontological).
narrative_ontology:cs_reference_frame('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', violence_inclusive_honor_framework).
narrative_ontology:cs_drift_state('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', post_institutional_criminalization, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('cf158c65-aa4d-49b0-a4fe-a2a149451b0e', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, honor_system_custodians).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, violent_practitioners).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, non_elite_duelists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, legal_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, violence_victims_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elite legal and cultural authorities (courts, salons, law codifiers) who control the definition of honor and manage the transition from violence-inclusive to violence-exclusive frameworks. They maintain institutional authority over what counts as honorable conduct and enforce the redefinition through cultural approbation and legal sanction. Benefit from the constraint because it concentrates honor capital in their hands and delegitimizes rival violence-based honor claims.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, honor_system_custodians, agenda_setter,
    institutional, generational, constrained, national).

% Nobles and gentlemen for whom dueling was a core honor practice and marker of status. As external costs mount (legal penalties, social ostracism, economic damage to families) AND the conceptual ground shifts (honor redefined to exclude violence), they face a double bind: continuing violence becomes both practically ruinous and ideologically untenable. Their exit is constrained because abandoning the honor system entirely means social death, yet adhering to it now means criminal liability and cultural shame.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, violent_practitioners, payer,
    powerful, biographical, constrained, national).

% Lower-status practitioners (military officers, professionals outside the high aristocracy) who adopted dueling to claim honor credentials but lack the institutional power to resist the redefinition. They bear the costs of the transition without the power to defend or redefine the system. Their identity as honorable persons becomes unattainable as the rules shift beneath them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, non_elite_duelists, payer,
    moderate, biographical, identity_locked, regional).

% Courts and state authorities that prosecute dueling and enforce the new honor framework. They benefit from expanded state authority over conduct previously private and honor-governed; their institutional power grows as the honor system's enforcement becomes legalized. They are the mechanism through which the redefinition is made binding.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, legal_enforcement_apparatus, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, legal_enforcement_apparatus, agenda_setter).

% Families of those killed or maimed in duels. They benefit from the constraint in that dueling becomes rare and then illegitimate, reducing the likelihood their members face challenge or death. But they have no say in whether the honor system remains or is redefined; their benefit is incidental to the institutional agenda.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, violence_victims_families, beneficiary,
    powerless, biographical, trapped, regional).

% Writers, philosophers, and public intellectuals who articulate the case for redefining honor to exclude violence. They bridge the two mechanisms (drop and contraction) by narrating the transition and making the new honor framework intellectually coherent. They do not directly enforce the constraint but their interpretive work makes the redefinition legible and acceptable.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, cultural_commentators, observer,
    moderate, biographical, mobile, national).

% Women and non-nobility who were never admitted to the honor-through-violence system but whose honor claims are now rendered legitimate through non-violent means (education, patronage, cultural attainment). They would object that the system's previous exclusion was unacknowledged and that the new framework still privileges elite access to honor capital.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, excluded_honor_claimants, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__composite_reading, honor_system_custodians).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for adjudicating reputation and status claims without recourse to state law (initially) or to arbitrary force (finally). The honor system coordinates social rank and resolves disputes over precedence within a rule-bound, albeit violent, system. When violence is removed, the coordination function persists in a non-violent form: honor is now assigned through approved conduct (education, cultural attainment, legal standing, institutional position).
% TRANSFER_FUNCTION: Transfers status capital, social precedence, and decision-making authority about honor from violent practitioners and rival power bases to institutional custodians (courts, salons, law-givers) who control the definition of honorable conduct. Those who resist or continue violence lose honor; those who accept the redefinition retain status under new terms.
% ABSENT_VOICES: Women excluded from the original honor system; non-elite practitioners who claimed honor through the violence framework but lack power to resist its closure; victims of violence who were never consulted on whether dueling's legitimacy should be preserved or redefined; rival frameworks for honor (merchant, artisanal, intellectual) that existed but were subordinated to the elite martial system.
% DISAPPEARANCE_RATIONALE: If the honor-violence constraint disappeared, dueling would resume where it had been suppressed (or accelerate where it was merely contained by rising costs). Without the constraint, the honor system would either reinvert to violence-inclusive form or collapse entirely into legal/economic reputation systems. The institutional and cultural apparatus for enforcing the new definition would lose authority, and rival honor frameworks would compete for legitimacy.
% FOUNDING_PROBLEM: Elite status disputes required resolution without recourse to state courts (which were distrusted or absent in early periods) and without descent into uncontrolled violence. Dueling provided a rule-bound combat system that adjudicated honor claims and maintained social hierarchy, while limiting escalation through ritualized conduct. The constraint coordinated this system and gave it legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal scholarship (Kiernan, McAleer) attests that the founding problem of elite status dispute resolution without state courts was real in the early period and is now moot; state legal systems have expanded to cover status disputes. Elite non-dueling practices (institutional advancement, publication, patronage) now adjudicate status without violence. The reading that the founding problem is dead is corroborated by the absence of dueling even when legal penalties were relaxed; the constraint persists through cultural definition (contraction) rather than through the original coordination problem (drop).
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The composite reading measures extractiveness at 0.67 (higher than drop alone would suggest, lower than pure institutional capture). This is because the constraint involves BOTH mechanisms: extractiveness starts low (0.38) when drop is the only operative mechanism — violent practitioners face high costs but might still find the conduct legitimate and worth the price. As contraction mechanisms activate (cultural redefinition, identity internalization), extractiveness rises asymptotically toward 0.67 by time 30 and plateaus there. The rise reflects the compounding effect: early in the interval, a practitioner might resist on the grounds that 'dueling is still honorable despite the costs.' Later, contraction makes even the desire to duel dishonorable and ideologically untenable. Suppression rises faster than extractiveness (0.35 to 0.72) because the constraint's enforcement burden increases as behavior-change must be accomplished through identity redefinition (harder to enforce) rather than through simple cost-penalty. Theater ratio rises and plateaus early (0.12 to 0.44) because the constraint enters piton-territory by midpoint: the founding problem (elite status dispute resolution through violence) is dead, but the institutional apparatus for maintaining the redefinition persists through cultural performance. By the endpoint, the constraint is both a real institutional beneficiary (legal authority) AND a theatrical maintenance of a dead coordination function (honor through non-violent conduct is the new real function; the old violence-exclusion is just narration by institutions that benefit from controlling the definition).
 *
 * PERSPECTIVAL GAP:
 *   From the composite_reading's seat, the honor-violence constraint is a tangled-rope with overdetermined decline (both mechanisms matter). From the drop_reading's seat (external costs as primary), it trends toward a temporary coordination problem (costs rise, practitioners abstain, constraint dissolves if costs fall). From the contraction_reading's seat (identity redefinition as primary), it trends toward an entrenched cultural lock-in (the new definition is self-perpetuating regardless of external costs). These are NOT the same constraint viewed from different angles — they are genuinely different causal structures, and the composite reading's claim that BOTH operate means it disagrees with both siblings on the mechanism but acknowledges both mechanisms are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor_system_custodians are full beneficiaries (d near 0.0): they control the definition, enforce the transition, and collect institutional authority. Their exit options are 'constrained' by institutional role, not by the constraint — they are defending their own system. Violent_practitioners are full targets (d near 1.0): they lose conduct-legitimacy, face legal penalties, and have identity redefined against their will. Their exit is 'constrained' because abandoning the honor system entirely means social death (not an option), but continuing requires accepting the new definition. Non_elite_duelists are higher-extraction targets (d near 0.95): same as violent_practitioners but with lower institutional power to resist (moderate vs. powerful). Legal_enforcement_apparatus are beneficiaries but as secondary_role to agenda_setter: they enforce the new framework and gain institutional authority, but they do not originate the cultural redefinition (custodians do). Violence_victims_families are beneficiaries (d near 0.15): they genuinely prefer the constraint (fewer family deaths), but they have no role in maintaining or enforcing it. Excluded_honor_claimants are ambiguous: they benefit from the new framework (can now claim honor through non-violence) but were not consulted and remain powerless; they are excluded from the conversation, not extracted from in the current constraint. Cultural_commentators sit near symmetric (d near 0.5): they participate in articulating the new framework (beneficiaries of influence) but are constrained by what cultural consensus already supports (cannot unilaterally enforce contraction); they are part of the extraction machinery but also constrained by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading resolves mandatrophy by showing that the founding problem is DEAD (elite status dispute resolution through violence is no longer necessary) but the constraint PERSISTS through two different mechanisms: (1) institutional extraction (custodians and legal authority benefit from control of definition and enforcement), (2) behavioral lock-in (violent practitioners cannot conceive of themselves as honorable through violence, even if costs fell). The theater_ratio rising from 0.12 to 0.44 signals the shift from coordinating function to performance: early interval has real function (violence is expensive and socially costly, so the constraint is needed to manage the trade-off); late interval the function is dead (no one needs to adjudicate status through violence anymore; the constraint persists because institutions benefit and because culture has internalized the redefinition). Mandatrophy is NOT fully resolved because the constraint could persist indefinitely through cultural lock-in even if institutional beneficiaries lost power — this is the identity-lock mechanism operating on a population scale, not just individuals. The case for mandatrophy is strong (founding problem is dead; theater is high) but the constraint's inertial persistence through cultural redefinition complicates the verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drop_vs_contraction_primacy,
    'Which mechanism drove the decline of honor-violence legitimacy: rising external costs (legal penalties, economic damage, social ostracism) making violence impractical, OR the conceptual redefinition of honor to exclude violence altogether?',
    'Counterfactual comparison: jurisdictions that reduced external costs without redefining honor (e.g. relaxed dueling laws but kept honor framework) versus jurisdictions that redefined honor without external cost rise. Historical analysis of which mechanism precedes the other chronologically and which is necessary/sufficient for decline.',
    'If drop is primary, the constraint is a temporary coordination problem with extractiveness bounded by external cost levels; if contraction is primary, the constraint becomes a permanent redefinition of category membership with extraction built into the new framework. This reading claims BOTH operate simultaneously and mutually reinforce, making drop insufficient alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_vs_contraction_primacy, conceptual, 'The primacy and sufficiency of the drop mechanism versus the contraction mechanism.').

omega_variable(
    beneficiary_identity_maintenance,
    'Do honor-system custodians and legal authorities genuinely benefit from the constraint, or do they claim benefit while the constraint persists primarily through cultural lock-in and behavioral internalization?',
    'Archival evidence of custodian advocacy for the redefinition (letters, policy statements, legal arguments). Measurement of actual authority/revenue accrual to institutions after the transition. Counterfactual: what would custodians lose if the constraint collapsed and violence-honor reappeared?',
    'If custodians benefit substantially (institutional power, cultural authority), the constraint is genuinely tangled-rope (coordination + asymmetric extraction). If their benefit is incidental and the constraint persists despite no clear concentrated beneficiary, it trends toward piton (performance of authority maintaining a dead function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_maintenance, empirical, 'Whether the constraint''s institutional beneficiaries actively maintain it or merely preside over its cultural decay.').

omega_variable(
    violent_practitioner_exit_semantics,
    'Is the violent_practitioners seat trapped (external barriers: legal penalties, economic ruin) or identity_locked (internal barriers: can no longer conceive of themselves as honorable through violence, even if external costs disappeared)?',
    'Rare historical cases where legal penalties for dueling were suspended (e.g. amnesty, regime change) and observe whether practitioners resumed dueling or remained abstinent. Memoirs and private correspondence showing whether practitioners experienced the change as forced (external) or as a shift in self-conception (internal).',
    'If trapped, the constraint is contingent on external cost maintenance and could revert if costs fell. If identity_locked, the constraint is internalized and persists even if external costs vanish — a deeper form of extraction because it operates through redefinition of the self.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violent_practitioner_exit_semantics, empirical, 'Whether violent practitioners are externally constrained or internally redefined in their exit options.').

omega_variable(
    kernel_contest_reading_relationship,
    'This reading (composite) claims BOTH drop and contraction mechanisms operate and reinforce each other. How does this relate to the sibling readings (drop_reading and contraction_reading) that claim each mechanism is primary?',
    'Clarification of the kernel itself: Is the kernel ''the legitimacy of honor-violence'' (in which case drop and contraction are two different answers to the same question) or is it ''the mechanism of decline'' (in which case the readings decompose into different causal stories)? The structure of the kernel determines whether the readings are competitors or complementary.',
    'If the kernel is legitimacy, the readings are in logical tension (you cannot have drop AND contraction both being THE mechanism — one must be primary). If the kernel is mechanism, the readings decompose and composite_reading is the most complex description. The allOf constraint on cs_structure.reading_relations forces clarity: forecloses or coexists_with, not ''both in some coherent reading.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reading_relationship, conceptual, 'The logical structure of the kernel and the reading_relations it implies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t5, honor_violence_legitimacy__composite_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(hono_tr_t5, observed).
narrative_ontology:measurement(hono_tr_t10, honor_violence_legitimacy__composite_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(hono_tr_t10, observed).
narrative_ontology:measurement(hono_tr_t15, honor_violence_legitimacy__composite_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(hono_tr_t15, observed).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t25, honor_violence_legitimacy__composite_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(hono_tr_t25, observed).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__composite_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(hono_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t5, honor_violence_legitimacy__composite_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(hono_be_t5, observed).
narrative_ontology:measurement(hono_be_t10, honor_violence_legitimacy__composite_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(hono_be_t10, observed).
narrative_ontology:measurement(hono_be_t15, honor_violence_legitimacy__composite_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(hono_be_t15, observed).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t25, honor_violence_legitimacy__composite_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(hono_be_t25, observed).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__composite_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(hono_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t5, honor_violence_legitimacy__composite_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(hono_su_t5, observed).
narrative_ontology:measurement(hono_su_t10, honor_violence_legitimacy__composite_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(hono_su_t10, observed).
narrative_ontology:measurement(hono_su_t15, honor_violence_legitimacy__composite_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(hono_su_t15, observed).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t25, honor_violence_legitimacy__composite_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hono_su_t25, observed).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__composite_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(hono_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-story kernel family (honor_violence_legitimacy). The composite_reading claims BOTH the drop_reading (external costs) and contraction_reading (conceptual redefinition) mechanisms operate simultaneously. The drop_reading isolates external-cost dynamics; the contraction_reading isolates cultural-redefinition dynamics; the composite_reading instantiates their mutual reinforcement. The three stories are linked by network.affects_constraints and share the same kernel_id but different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
