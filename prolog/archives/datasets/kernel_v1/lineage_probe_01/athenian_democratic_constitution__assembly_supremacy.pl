% ============================================================================
% CONSTRAINT STORY: athenian_democratic_constitution__assembly_supremacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_athenian_democratic_constitution__assembly_supremacy, []).

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
 *   constraint_id: athenian_democratic_constitution__assembly_supremacy
 *   human_readable: Athenian Assembly Supremacy: Direct Democratic Decision-Making
 *   domain: political/historical/democratic_theory
 *
 * SUMMARY:
 *   Athens' constitution, according to the assembly supremacy reading, is the
 *   ekklesia itself: the assembled citizens making war, finance, and law
 *   directly, meeting approximately forty times per year, with no government
 *   authority above them to appeal to. This constraint instantiates one of
 *   four competing readings of what makes Athens' constitutional system work.
 *   The assembly supremacy reading emphasizes the mechanism of direct
 *   decision-making and the absence of a permanent executive with veto power.
 *   From this perspective, the constraint is a pure coordination mechanism
 *   (rope) — it solves the problem of 'who decides?' by answering 'whoever
 *   attends and votes.' No extraction, no suppression of the voting demos,
 *   minimal theater. However, this reading coexists with three structurally
 *   incompatible-seeming alternatives: the accountability_machinery reading
 *   (Athens is defined by its audit system), the exclusionary_base reading
 *   (Athens is defined by who is excluded), and the sortition_and_rotation
 *   reading (Athens is defined by the lottery for offices). The kernel
 *   contest is not resolved by empirical discovery — all four readings
 *   accurately describe real institutional features of Athens. The contest is
 *   resolved by understanding which feature is constitutive of the
 *   constitutional kernel and which are secondary. This story instantiates
 *   the assembly_supremacy reading.
 *
 * KEY AGENTS:
 *   - Voting Citizens Present: Primary beneficiary (powerful/mobile) — authority flows to whoever attends assembly; benefit is direct and immediate
 *   - Demos Collectively: Primary beneficiary (organized/mobile) — the citizen body as an organized whole experiences the assembly as pure coordination mechanism
 *   - Excluded Non-Citizens: Secondary victim (moderate/constrained) — women, slaves, metics, distant farmers experience assembly supremacy as suppression with no coordination benefit; exit costs are catastrophic
 *   - Magistrates: Institutional actors (institutional/constrained) — office-holders experience mixed coordination (assembly confirms power) and extraction (assembly can overturn, ostracize, or prosecute)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — sees assembly supremacy as a coordination type that solves collective decision-making without permanent executive delegation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(athenian_democratic_constitution__assembly_supremacy, 0.38).
domain_priors:suppression_score(athenian_democratic_constitution__assembly_supremacy, 0.42).
domain_priors:theater_ratio(athenian_democratic_constitution__assembly_supremacy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(athenian_democratic_constitution__assembly_supremacy, extractiveness, 0.38).
narrative_ontology:constraint_metric(athenian_democratic_constitution__assembly_supremacy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(athenian_democratic_constitution__assembly_supremacy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(athenian_democratic_constitution__assembly_supremacy, rope).
narrative_ontology:human_readable(athenian_democratic_constitution__assembly_supremacy, "Athenian Assembly Supremacy: Direct Democratic Decision-Making").
narrative_ontology:topic_domain(athenian_democratic_constitution__assembly_supremacy, "political/historical/democratic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(athenian_democratic_constitution__assembly_supremacy, '9a364d4a-c197-4131-851a-7c17c9810997').
narrative_ontology:cs_kernel_codification('9a364d4a-c197-4131-851a-7c17c9810997', fixed_text).
narrative_ontology:cs_authority_grounding('9a364d4a-c197-4131-851a-7c17c9810997', lineage).
narrative_ontology:cs_interpretation_layer_present('9a364d4a-c197-4131-851a-7c17c9810997').
narrative_ontology:cs_reading_relation('9a364d4a-c197-4131-851a-7c17c9810997', athenian_democratic_constitution__accountability_machinery, coexists_with).
narrative_ontology:cs_reading_relation('9a364d4a-c197-4131-851a-7c17c9810997', athenian_democratic_constitution__exclusionary_base, influences).
narrative_ontology:cs_reading_relation('9a364d4a-c197-4131-851a-7c17c9810997', athenian_democratic_constitution__sortition_and_rotation, coexists_with).
narrative_ontology:cs_axiom('9a364d4a-c197-4131-851a-7c17c9810997', foundational, supreme_decision_authority_resides_in_assembled_demos).
narrative_ontology:cs_axiom_status(supreme_decision_authority_resides_in_assembled_demos, holdable).
narrative_ontology:cs_axiom_grounding('9a364d4a-c197-4131-851a-7c17c9810997', supreme_decision_authority_resides_in_assembled_demos, conventional).
narrative_ontology:cs_axiom('9a364d4a-c197-4131-851a-7c17c9810997', foundational, no_executive_authority_above_assembly_veto).
narrative_ontology:cs_axiom_status(no_executive_authority_above_assembly_veto, holdable).
narrative_ontology:cs_axiom_grounding('9a364d4a-c197-4131-851a-7c17c9810997', no_executive_authority_above_assembly_veto, conventional).
narrative_ontology:cs_reference_frame('9a364d4a-c197-4131-851a-7c17c9810997', direct_assembled_democratic_decision).
narrative_ontology:cs_drift_state('9a364d4a-c197-4131-851a-7c17c9810997', late_classical_imperial_phase, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9a364d4a-c197-4131-851a-7c17c9810997', '').
narrative_ontology:cs_kernel_id(athenian_democratic_constitution__assembly_supremacy, athenian_democratic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(athenian_democratic_constitution__assembly_supremacy, voting_citizens_present).
narrative_ontology:constraint_beneficiary(athenian_democratic_constitution__assembly_supremacy, demos_in_assembly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ATTENDING CITIZEN (ROPE) — Citizens with mobility to attend assembly experience pure coordination. The constraint allocates decision authority to whoever shows up and votes. No extraction — the beneficiary IS the agent. Exits exist (a citizen can skip a session without legal penalty, though abstention has reputational cost). The classification reflects genuine coordination: the assembly solves the problem of 'who decides?' by answering 'whoever is present.'
constraint_indexing:constraint_classification(athenian_democratic_constitution__assembly_supremacy, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: THE EXCLUDED OR DISTANT CITIZEN (SNARE) — Citizens who cannot attend (slaves, metics, women, poor farmers at harvest time, those living far from the city) experience the assembly supremacy as pure suppression. They have no voice in the decisions that govern them. The constraint extracts compliance without benefit. Formal exit (renounce citizenship) is available but carries catastrophic social and economic cost. This perspective sees the constraint as snare because suppression is high and coordination benefit is zero for this agent.
constraint_indexing:constraint_classification(athenian_democratic_constitution__assembly_supremacy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE CITIZEN BODY COLLECTIVELY (ROPE) — From the generational perspective of the demos as an organized collective, the assembly supremacy is a coordination mechanism pure and simple. It solves the problem: 'How do we collectively decide on war, tribute, and law without a king or oligarchy controlling those decisions?' The answer — direct assembly voting — coordinates the demos' collective will. No extraction, no coercion (members can opt out), minimal theater (voting is straightforward, not ritual). Pure coordination. This is the constraint's native perspective.
constraint_indexing:constraint_classification(athenian_democratic_constitution__assembly_supremacy, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: THE MAGISTRATE OR ARCHON (TANGLED ROPE) — An office-holder experiences assembly supremacy as mixed coordination and extraction. The assembly confirms magistrates in power but also overturns their decisions and can ostracize them. The magistrate benefits from coordinated governance (the assembly validates their authority) but is extracted from via removal, prosecution, or reversal. The constraints on exit are high — a magistrate cannot simply walk away from office or criticism. This perspective sees coordination and asymmetric extraction intertwined.
constraint_indexing:constraint_classification(athenian_democratic_constitution__assembly_supremacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (ROPE) — From a universal analytical standpoint, the assembly supremacy is a coordination mechanism: it solves the problem of collective decision-making without delegation to a permanent executive. The extractiveness is low because the constraint does not concentrate power in a single authority — it distributes it to the present demos. The suppression is not inherent to the mechanism but is a separate structural fact (exclusion of non-citizens). The theater is minimal: voting is the function, not a performance. This reading sees rope as the constraint's essential type.
constraint_indexing:constraint_classification(athenian_democratic_constitution__assembly_supremacy, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(athenian_democratic_constitution__assembly_supremacy_tests).
:- end_tests(athenian_democratic_constitution__assembly_supremacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The assembly supremacy constraint does not concentrate power in a single extractor — it distributes decision authority to whoever attends. From the perspective of voting citizens, extractiveness is near zero (they are the beneficiaries). From the perspective of excluded non-citizens, extractiveness approaches 1.0 (pure suppression). The 0.38 value reflects a weighted average across perspectives: the demos represents perhaps 10-15% of the population, and their coordination benefit is genuine, but the suppression of the remaining 85-90% is severe. The extractiveness increases slightly over time (0.32 → 0.40) as empire expands, creating more non-citizens without voting rights and more distant citizens unable to attend. Suppression (0.42): Moderate. The assembly supremacy reading itself does not require suppression of the voting demos — those citizens are freely participating. But the constraint's operation depends on the suppression of non-citizens (slaves, metics, women) from voting. This is a structural fact of the reading: assembly supremacy for whom? The answer is: for citizens only, and citizenship is restricted. The suppression increases slightly (0.38 → 0.45) over time as enforcement of non-citizen exclusion becomes more explicit and more costly to violate. Theater ratio (0.35): Low-moderate. The assembly's decision process is functional, not performative — voting is straightforward, debate is real, outcomes are binding. The theater arises not from the assembly itself but from the framing: the claim that this is 'the constitution' naturalizes what is actually a specific institutional choice. Theater increases slightly (0.28 → 0.42) over time as democracy becomes more self-conscious and starts to perform 'being democratic' as a public good.
 *
 * PERSPECTIVAL GAP:
 *   The attending citizen and the excluded non-citizen experience fundamentally different constraints under the same institutional label. For the citizen, assembly supremacy is rope — pure coordination with no extraction. For the excluded non-citizen, assembly supremacy is snare — pure extraction with no coordination benefit. For the magistrate, it is tangled rope — the assembly both empowers and constrains. For the collective demos, it is rope at the generational scale. For the analytical observer, it is rope — the mechanism solves collective decision without permanent delegation. The gap reveals that 'assembly supremacy' as a constitutional claim requires sustained suppression of non-citizens to function as coordination for citizens. The tension is not resolved by choosing one perspective as 'correct' but by recognizing that the constraint's type is perspectival, not essential.
 *
 * DIRECTIONALITY LOGIC:
 *   From the assembly supremacy reading, directionality is determined by whether the agent is a voting citizen or not. Voting citizens (beneficiaries with mobile exit options) derive d ≈ 0.15-0.20, producing negative f(d) and negative effective extraction — they experience the constraint as pure benefit. Excluded non-citizens (victims with constrained exit due to slavery or severe legal disability) derive d ≈ 0.85-0.95, producing high f(d) and high effective extraction — they experience the constraint as pure suppression. Magistrates (institutional context with constrained exit due to office holding) derive d ≈ 0.50-0.60, experiencing mixed coordination and extraction. The analytical observer (analytical context) derives d ≈ 0.72, the canonical analytical directionality, viewing the constraint as a genuine coordination mechanism with inherited asymmetric burdens.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by being rooted in a genuine coordination function (the assembly truly does solve the problem of 'who decides?' without a permanent executive) and a genuine victim set (non-citizens are structurally excluded from the decision mechanism). The constraint is not mislabeling extraction as coordination — the extraction is real and visible to excluded non-citizens. But the extraction is not the primary function of the constraint from the perspective of voting citizens. The mandatrophy is resolved by accepting that the constraint is simultaneously rope (for citizens) and snare (for non-citizens), and these are not competing claims but different measurements of the same structural mechanism from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    assembly_sovereignty_vs_accountability_tension,
    'Is assembly supremacy fundamentally incompatible with post-decisional accountability, or do Athens'' audit mechanisms (euthyna, probole) represent a coherent integration of both?',
    'Historical analysis of conflicts between assembly decisions and accountants'' judgments; examination of whether ostracism operates as retroactive assembly override or as separate accountability institution',
    'If incompatible: assembly_supremacy and accountability_machinery readings foreclose each other. If integrated: they coexist as different aspects of a single constitutional kernel. This determines whether the kernel is genuinely contested or whether ''supremacy'' and ''accountability'' describe sequential rather than simultaneous authority structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assembly_sovereignty_vs_accountability_tension, empirical, 'Whether assembly supremacy is logically compatible with post-decisional accountability mechanisms').

omega_variable(
    sortition_as_supremacy_mechanism_vs_bypass,
    'Does the lottery for magistrates (sortition) implement assembly supremacy by reducing magistrate independence, or does it bypass assembly supremacy by creating a parallel authority structure not answerable to assembly preferences?',
    'Examination of assembly''s power over lottery process (could it revise lot-based selections?); analysis of whether magistrates drawn by lot treated as delegates of assembly will or as independent rotation officials',
    'If sortition implements supremacy: this reading coexists with sortition_and_rotation reading. If sortition bypasses it: the readings foreclose each other — the lottery creates space for magistrate action outside assembly control. This determines the coherence of Athens'' constitutional kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sortition_as_supremacy_mechanism_vs_bypass, conceptual, 'Whether sortition implements or bypasses assembly supremacy').

omega_variable(
    exclusion_as_external_vs_constitutive_fact,
    'Is the exclusion of non-citizens (women, slaves, metics) external to the assembly supremacy constraint, or is it constitutive — does assembly supremacy only function because a minority demos can make binding decisions for a subjugated majority?',
    'Counterfactual analysis: if metics and slaves were voting members, would assembly supremacy still be a coordination mechanism or would it become a snare? Historical comparison with inclusive democracies (modern representative systems with broader franchises); analysis of whether Athens'' assembly supremacy is replicable at different exclusion thresholds',
    'If external: exclusionary_base and assembly_supremacy are separate constraints (network.affects_constraints relationship). If constitutive: they foreclose each other — true assembly supremacy requires an exclusive demos, or true inclusion requires abandoning assembly supremacy. Determines whether this reading can genuinely coexist with the exclusionary_base reading in a single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_as_external_vs_constitutive_fact, conceptual, 'Whether exclusion is a separate constraint or constitutive to assembly supremacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(athenian_democratic_constitution__assembly_supremacy, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ath_assembly_theater_t0, athenian_democratic_constitution__assembly_supremacy, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ath_assembly_theater_t50, athenian_democratic_constitution__assembly_supremacy, theater_ratio, 50, 0.35).
narrative_ontology:measurement(ath_assembly_theater_t100, athenian_democratic_constitution__assembly_supremacy, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(ath_assembly_extract_t0_founding, athenian_democratic_constitution__assembly_supremacy, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ath_assembly_extract_t50_empire, athenian_democratic_constitution__assembly_supremacy, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(ath_assembly_extract_t100_degradation, athenian_democratic_constitution__assembly_supremacy, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(ath_assembly_suppress_t0, athenian_democratic_constitution__assembly_supremacy, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(ath_assembly_suppress_t50, athenian_democratic_constitution__assembly_supremacy, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(ath_assembly_suppress_t100, athenian_democratic_constitution__assembly_supremacy, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(athenian_democratic_constitution__assembly_supremacy, resource_allocation).
narrative_ontology:affects_constraint(athenian_democratic_constitution__assembly_supremacy, athenian_democratic_constitution__accountability_machinery).
narrative_ontology:affects_constraint(athenian_democratic_constitution__assembly_supremacy, athenian_democratic_constitution__exclusionary_base).
narrative_ontology:affects_constraint(athenian_democratic_constitution__assembly_supremacy, athenian_democratic_constitution__sortition_and_rotation).

% DUAL FORMULATION NOTE:
% The athenian_democratic_constitution kernel decomposes into four structurally distinct constraints, each with different ε values and different claimed types. The assembly_supremacy reading (ε=0.38, rope) treats direct citizen voting as the constitutional core. The accountability_machinery reading emphasizes post-decisional audits and personal liability of magistrates. The exclusionary_base reading emphasizes the restricted citizenship that makes the demos a minority of the resident population. The sortition_and_rotation reading emphasizes the lottery and rotation principle as anti-aristocratic mechanism. These are not four measurements of one constraint but four readings of one contested kernel, producing four separate constraint stories with different beneficiary/victim structures and different effective extractiveness values. They share affects_constraints relationships because the empirical success or failure of any one reading affects the plausibility of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
