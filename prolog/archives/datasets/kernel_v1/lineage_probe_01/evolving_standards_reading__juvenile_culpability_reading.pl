% ============================================================================
% CONSTRAINT STORY: evolving_standards_reading__juvenile_culpability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolving_standards_reading__juvenile_culpability_reading, []).

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
 *   constraint_id: evolving_standards_reading__juvenile_culpability_reading
 *   human_readable: Eighth Amendment Juvenile Culpability Reading (Miller/Graham Doctrine)
 *   domain: constitutional_law/criminal_procedure
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a contested Eighth Amendment
 *   kernel: the 'evolving standards of decency' clause and what it requires
 *   for juvenile offenders. Miller v. Alabama (2012) and Graham v. Florida
 *   (2010) held that mandatory life without parole (LWOP) for juveniles
 *   violates the Eighth Amendment because juveniles have diminished
 *   culpability (due to developmental neuroscience) and a reasonable prospect
 *   of eventual rehabilitation. This reading embeds development science
 *   directly into constitutional meaning: the Clause is read to require
 *   individualized sentencing that accounts for youth-based diminished
 *   capacity. The constraint suppresses the alternative: the regime of
 *   mandatory juvenile LWOP that preceded Miller/Graham. The beneficiary is
 *   the pool of juvenile offenders whose sentences are converted from
 *   irrevocable to reviewable (their future selves, capable of eventual
 *   release). The victims are the mandatory-sentencing schemes themselves —
 *   statutes that auto-imposed LWOP without judicial discretion. The
 *   constraint exhibits tangled_rope structure: it coordinates a principle
 *   (culpability-based proportionality applied to juveniles) while extracting
 *   a cost (mandatory schemes lose certainty, prosecutors lose charging
 *   leverage, states lose the incapacitation guarantee). Extractiveness has
 *   declined from the pre-Miller era (0.55) as resentencing machinery matures
 *   and the affected cohort ages, stabilizing at 0.38. Suppression has
 *   declined as judicial discretion replaced mandatory imposition, though
 *   significant barriers remain (prosecutorial workarounds, conservative
 *   resentencing judges). Theater ratio remains moderate (0.35) because the
 *   constraint is enforceable via actual resentencing, not merely ritual —
 *   the alternative (no resentencing, unchanged sentences) is structurally
 *   blocked by Miller/Graham.
 *
 * KEY AGENTS:
 *   - Juvenile Offenders at Sentencing (powerless/trapped): Primary victims of the pre-Miller mandatory regime; primary beneficiaries of the constraint. Bear maximum extraction pre-Miller; experience reduced extraction post-Miller as resentencing becomes mandatory.
 *   - State Sentencing Institutions (institutional/constrained): Mixed position — lose mandatory certainty but gain coordination framework and reduced federal review risk. Neither pure beneficiary nor pure victim.
 *   - Defense Advocacy & Juvenile Justice Reform (institutional/arbitrage): Primary beneficiary. The constraint provides legal and moral authority for organizing litigation and legislative reform.
 *   - Implementation & Resentencing Machinery (organized/mobile): See the constraint as temporary (scaffold logic). View the endpoint (all affected cohorts resentenced) as a natural sunset.
 *   - Prosecutorial Practice (institutional/constrained): Experience the constraint as piton-like — formal review obligation that permits charging workarounds. Suppression remains high because prosecutorial discretion survives doctrinal constraints.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing the doctrinal choice (that neuroscience determines constitutional meaning) as a fact of law rather than a contested reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolving_standards_reading__juvenile_culpability_reading, 0.38).
domain_priors:suppression_score(evolving_standards_reading__juvenile_culpability_reading, 0.62).
domain_priors:theater_ratio(evolving_standards_reading__juvenile_culpability_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolving_standards_reading__juvenile_culpability_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(evolving_standards_reading__juvenile_culpability_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(evolving_standards_reading__juvenile_culpability_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolving_standards_reading__juvenile_culpability_reading, tangled_rope).
narrative_ontology:human_readable(evolving_standards_reading__juvenile_culpability_reading, "Eighth Amendment Juvenile Culpability Reading (Miller/Graham Doctrine)").
narrative_ontology:topic_domain(evolving_standards_reading__juvenile_culpability_reading, "constitutional_law/criminal_procedure").

domain_priors:requires_active_enforcement(evolving_standards_reading__juvenile_culpability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(evolving_standards_reading__juvenile_culpability_reading, 'cd73421d-6f47-4daa-9fc1-4adf46128ece').
narrative_ontology:cs_kernel_codification('cd73421d-6f47-4daa-9fc1-4adf46128ece', formalized).
narrative_ontology:cs_authority_grounding('cd73421d-6f47-4daa-9fc1-4adf46128ece', lineage).
narrative_ontology:cs_interpretation_layer_present('cd73421d-6f47-4daa-9fc1-4adf46128ece').
narrative_ontology:cs_reading_relation('cd73421d-6f47-4daa-9fc1-4adf46128ece', evolving_standards_reading__death_penalty_narrowing_reading, influences).
narrative_ontology:cs_reading_relation('cd73421d-6f47-4daa-9fc1-4adf46128ece', evolving_standards_reading__conditions_confinement_reading, coexists_with).
narrative_ontology:cs_axiom('cd73421d-6f47-4daa-9fc1-4adf46128ece', foundational, juvenile_culpability_constitutionally_relevant).
narrative_ontology:cs_axiom_status(juvenile_culpability_constitutionally_relevant, holdable).
narrative_ontology:cs_axiom_grounding('cd73421d-6f47-4daa-9fc1-4adf46128ece', juvenile_culpability_constitutionally_relevant, empirically_contingent).
narrative_ontology:cs_axiom('cd73421d-6f47-4daa-9fc1-4adf46128ece', foundational, rehabilitative_capacity_future_oriented).
narrative_ontology:cs_axiom_status(rehabilitative_capacity_future_oriented, holdable).
narrative_ontology:cs_axiom_grounding('cd73421d-6f47-4daa-9fc1-4adf46128ece', rehabilitative_capacity_future_oriented, empirically_contingent).
narrative_ontology:cs_reference_frame('cd73421d-6f47-4daa-9fc1-4adf46128ece', individualized_dignity_based_sentencing).
narrative_ontology:cs_drift_state('cd73421d-6f47-4daa-9fc1-4adf46128ece', contemporary_prosecutorial_workarounds, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd73421d-6f47-4daa-9fc1-4adf46128ece', '').
narrative_ontology:cs_kernel_id(evolving_standards_reading__juvenile_culpability_reading, evolving_standards_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolving_standards_reading__juvenile_culpability_reading, juvenile_offenders_future_capacity).
narrative_ontology:constraint_victim(evolving_standards_reading__juvenile_culpability_reading, mandatory_sentencing_schemes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUVENILE OFFENDER AT SENTENCING (SNARE) — Trapped by both age and legal status. The mandatory-sentencing regime forecloses all exit paths: no discretion for mitigation, no recognition of developmental change, no future-oriented alternative. The offender bears maximum extraction — a life sentence at fifteen, with no legal pathway to challenge the sentence's proportionality to culpability. The constraint suppresses every alternative (judicial discretion, developmental evidence, rehabilitation potential) and permits only the coded outcome.
constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE SENTENCING INSTITUTIONS (TANGLED ROPE) — Constrained by the Miller/Graham doctrine but also benefit from a coordination function: the doctrine establishes a framework for considering individualized circumstances, which reduces the risk of excessive sentences that invite federal review and create institutional liability. The state is partly the beneficiary (maintains sentencing authority) and partly the victim (loses mandatory-sentencing certainty). Moderate extraction because the doctrine preserves state power while constraining its worst expressions.
constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE ADVOCACY & JUVENILE JUSTICE REFORM (ROPE) — Primary beneficiary. The Miller/Graham doctrine creates legal and moral authority for challenging mandatory-juvenile-life sentences. This constraint provides a coordination mechanism: advocates can invoke constitutional doctrine to organize litigation, legislative reform, and public support. The constraint operates as pure coordination from this perspective — it enables collective action without extracting value from the advocates themselves. Arbitrage exit: advocates can shift to state legislation, international bodies, or alternative doctrinal frameworks while maintaining leverage through the constitutional claim.
constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPLEMENTATION & RESENTENCING MACHINERY (SCAFFOLD) — Organized actors (courts, legislatures, sentencing commissions) see Miller/Graham as a temporary framework for addressing a known problem with a sunset: once all mandatory-juvenile-life sentences are converted to sentences with parole eligibility (the structural outcome of the doctrine), the active enforcement burden declines dramatically. The constraint has a natural sunset as the affected population ages out and resentencing concludes. Effective extraction is low because the organized actors see an exit path — a defined end state where the constraint becomes moot.
constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PROSECUTORIAL PRACTICE (PITON) — The Miller/Graham doctrine constrains but does not eliminate prosecutorial power: prosecutors retain charging discretion and can still pursue sentences with 50-year minimums for juveniles, knowing the sentence will later face resentencing review. The doctrine has become largely theatrical at the prosecutorial level — it produces a formal review that prosecutors work around. Theater ratio high because the constraint permits the same ultimate outcomes (long sentences) through a different procedural path (individualized sentencing review rather than mandatory imposition). Prosecutors experience the constraint as performative — a ritual that must be observed without materially limiting their discretionary power.
constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Miller/Graham doctrine appears to read a fact of developmental neuroscience into the Constitution: juveniles have reduced culpability due to brain development. This perspective sees the constraint as a natural-law expression of scientific fact — development is immutable, and law merely recognizes what is already true about juvenile capacity. However, this naturalizes a doctrinal choice (that development science grounds constitutional meaning) that other readings of the Clause reject. The engine's false summit detector will flag this as a fabricated natural law.
constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolving_standards_reading__juvenile_culpability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolving_standards_reading__juvenile_culpability_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolving_standards_reading__juvenile_culpability_reading, TR),
    TR >= 0.70.

:- end_tests(evolving_standards_reading__juvenile_culpability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The constraint reduces extraction significantly compared to the mandatory-LWOP regime (0.55 pre-Miller) by introducing judicial discretion and requiring developmental evidence. However, the reduction is not total — prosecutors retain charging power, many resentenced juveniles receive sentences of 30+ years with parole eligibility at 50+, and the constraint is vulnerable to judicial narrowing. The metric reflects the intermediate state: meaningful but incomplete suppression of the pre-Miller extraction. Suppression (0.62): Moderate-high. Significant barriers remain to full relief: prosecutorial discretion in charging and plea pressure, conservative resentencing judges, insufficient juvenile-specific mitigation evidence, resource barriers to expert testimony, and the doctrine's ceiling on sentences (allowing decades of incapacitation even for non-homicide crimes). Yet suppression is lower than the mandatory era (0.78) because judicial discretion and developmental science evidence are now mandatory components of sentencing. Theater ratio (0.35): Moderate-low. The constraint produces real resentencing proceedings, not merely ritual. However, some theater emerges as resentencing becomes proceduralized: courts develop pro forma mitigation frameworks, prosecutorial arguments follow predictable patterns, and the discretion is bounded by sentencing guidelines and precedent. The theater is lower than piton-range because the actual outcome (sentence length, parole eligibility) genuinely depends on the hearing, not on predetermined form.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival diversity across a single legal doctrine. Juvenile offenders at sentencing see snare (immutability from their position at age 15 facing LWOP). Defense advocacy sees rope (enabling coordination). Prosecutors see piton (formal constraint with substantive workarounds). Implementation machinery sees scaffold (temporary problem with defined endpoint). State institutions see tangled_rope (mixed benefit and cost). The analytical observer risks seeing mountain (naturalizing the neuroscience-as-constitutional-fact framing). The perspectival gap between the powerless and institutional actors is maximal: the same doctrine that appears as pure extraction to the juvenile appears as manageable constraint to the state, and as a coordination mechanism to reformers. This gap reveals that the constraint's type depends entirely on observational position — there is no context-free answer to 'what constraint is Miller/Graham?' The answer is: all six types, from six different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value is derived from its structural position relative to the extraction flow. Juvenile offenders at sentencing are pure victims (trapped, no alternatives) — d ≈ 0.95, producing high f(d) and high experienced extraction. Defense advocates are pure beneficiaries (arbitrage exit, institutional power) — d ≈ 0.10, producing low/negative f(d) and low experienced extraction. Prosecutors occupy an intermediate position (retain charging discretion but lose mandatory imposition) — d ≈ 0.55, producing moderate f(d) and moderate experienced extraction. The state sentencing institutions are closest to symmetric (benefit from coordination framework, lose mandatory certainty) — d ≈ 0.50, producing moderate f(d). These directionality values, combined with the 0.38 base extractiveness and σ(S) = 1.0 (national scope), produce the perspectival gap: the powerless agent sees snare (high chi), the beneficiary sees rope (low chi), the moderate state sees tangled_rope (mid-range chi). The constraint is not perceived identically across positions — the indexical tuple (P, T, E, S) produces genuinely different classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through committer-frame analysis: the constraint is one reading (juvenile_culpability_reading) of a contested kernel (Eighth Amendment 'cruel and unusual punishment'). The mandatrophy question is 'which reading of the kernel is correct?' not 'which constraint type is correct?' The juvenile_culpability_reading coexists with the conditions_confinement_reading (which emphasizes human dignity and conditions of confinement) and influences the death_penalty_narrowing_reading (which emphasizes retributive proportionality across categories of offenders). All three readings operate within the same constitutional framework but ground their authority differently: culpability (Miller/Graham), dignity (conditions), proportionality (capital). The tangled_rope classification at the analytical level reflects the constraint's hybrid nature: it coordinates a new principle (developmental proportionality) while extracting cost from established regimes (mandatory sentencing). This is precisely the mandatrophy resolution: the constraint is correctly classified as tangled_rope because it contains both coordination (establishing a framework for considering individual circumstances) and extraction (suppressing mandatory alternatives). The perspectival divergence is legitimate — each position experiences the constraint's hybrid character differently, not because one perspective is wrong, but because the constraint genuinely contains both elements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    development_science_as_constitutional_fact,
    'Is diminished juvenile culpability a constitutional fact (read from development neuroscience into the Clause) or a policy preference (that the Clause permits but does not require)?',
    'Comparison of Miller/Graham''s doctrinal language (''evolving standards of decency'') with its treatment of neuroscientific evidence. If the Court treats development science as determinative of constitutional meaning, the claim is constitutional fact. If the Court treats it as evidence supporting a policy judgment, the claim is preference. Historical and comparative analysis: do other common-law jurisdictions reach identical conclusions about juvenile culpability absent the same neuroscientific consensus?',
    'If constitutional fact: the reading is nearly immutable (mountain-adjacent). If policy preference: the reading is revisable through changing science or doctrinal reinterpretation (tangled_rope or scaffold ceiling). This determines whether the doctrine can be reversed or only narrowed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(development_science_as_constitutional_fact, conceptual, 'Whether development science grounds constitutional fact or policy preference').

omega_variable(
    mandatory_sentencing_regime_structural_function,
    'Are mandatory-juvenile-life sentences a mechanism for ensuring accountability, a mechanism for incapacitating dangerous offenders, or a mechanism for permanently excluding juveniles from civic membership?',
    'Legislative history analysis of mandatory-sentence enactment (what harms were the laws meant to address?). Empirical comparison: do jurisdictions with mandatory-juvenile-life differ in violent-crime rates from those without? Post-sentencing outcomes: what proportion of juveniles mandatorily sentenced to life ultimately pose danger at parole-review age? Do they recidivate at rates requiring continued incapacitation?',
    'If accountability: the Miller/Graham reading appropriately bars sentences disproportionate to culpability. If incapacitation: the reading suppresses legitimate public-safety concerns and may produce insufficient protection. If exclusion: the reading correctly identifies and blocks a form of permanent civil death incompatible with constitutional principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatory_sentencing_regime_structural_function, empirical, 'Structural function of mandatory-juvenile-life sentences').

omega_variable(
    eighth_amendment_kernel_contest_location,
    'Does the Eighth Amendment''s ''cruel and unusual punishment'' clause ground its meaning in retributive proportionality (culpability-based), in evolving standards of decency (democratic consensus), or in human dignity (categorical prohibitions)?',
    'Doctrinal history: how have Supreme Court decisions weighted these three groundings across different punishment contexts (capital, corporal, conditions of confinement)? Miller/Graham relies on evolving standards (generational consensus against mandatory juvenile life). The conditions_confinement_reading relies on human dignity (refusal to dehumanize). The death_penalty_narrowing_reading relies on retributive proportionality (crimes and capacities matched to punishments). Each reading foregrounds a different constitutional grounding.',
    'If retributive proportionality dominates: Miller/Graham is a strong reading (culpability is constitutional). If evolving standards dominates: Miller/Graham is a medium reading (consensus determines scope). If human dignity dominates: Miller/Graham is a weak reading (dignity arguments apply to all punishment, not just juvenile sentences). This determines whether the doctrine''s logic extends to other offenders or remains category-specific.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eighth_amendment_kernel_contest_location, conceptual, 'Location of Eighth Amendment kernel''s ground of meaning').

omega_variable(
    reading_reversal_stability,
    'Is the juvenile_culpability_reading a stable doctrinal direction or vulnerable to reversal through appointments or ideological shift?',
    'Comparison of Miller/Graham''s doctrinal foundation (narrow 5-4 majorities, reliance on evolving standards rather than per se rule) with doctrines that have proven stable (complete prohibition on capital punishment for juveniles — never reversed) versus those that have been reversed (juvenile LWOP prohibition — currently unsettled post-current appointee shifts). Empirical stability: do states continue honoring Miller/Graham resentencing obligations, or are there rollbacks?',
    'If stable: the reading is a constitutional fixture, properly classified as tangled_rope or rope. If vulnerable: the reading is contingent on judicial composition, potentially downgrading to piton (performative) or scaffold (temporary). This determines investment in implementation versus contingency planning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_reversal_stability, empirical, 'Doctrinal stability of the Miller/Graham juvenile_culpability_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolving_standards_reading__juvenile_culpability_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juv_theater_mandatory_era_low, evolving_standards_reading__juvenile_culpability_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(juv_theater_resentencing_ritual_emerges, evolving_standards_reading__juvenile_culpability_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(juv_theater_ratio_stabilized, evolving_standards_reading__juvenile_culpability_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(juv_extractiveness_pre_miller, evolving_standards_reading__juvenile_culpability_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(juv_extractiveness_post_miller_immediate, evolving_standards_reading__juvenile_culpability_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(juv_extractiveness_resentencing_cohort_aging, evolving_standards_reading__juvenile_culpability_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(juv_suppression_mandatory_era, evolving_standards_reading__juvenile_culpability_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(juv_suppression_post_miller_discretion_introduced, evolving_standards_reading__juvenile_culpability_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(juv_suppression_stabilized_resentencing_process, evolving_standards_reading__juvenile_culpability_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolving_standards_reading__juvenile_culpability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(evolving_standards_reading__juvenile_culpability_reading, death_penalty_narrowing_reading).
narrative_ontology:affects_constraint(evolving_standards_reading__juvenile_culpability_reading, conditions_confinement_reading).
narrative_ontology:affects_constraint(evolving_standards_reading__juvenile_culpability_reading, mandatory_minimum_sentencing_regime).
narrative_ontology:affects_constraint(evolving_standards_reading__juvenile_culpability_reading, juvenile_justice_discretion).

% DUAL FORMULATION NOTE:
% The juvenile_culpability_reading is one reading of the Eighth Amendment kernel. The death_penalty_narrowing_reading and conditions_confinement_reading are sibling readings of the same kernel. Each should be authored as a separate constraint story with its own ε, beneficiary/victim structure, and perspectives. Network edges indicate doctrinal influence: this reading influences death_penalty_narrowing (extends the logic of categorical culpability-based exemptions) and provides foundation for conditions_confinement (both require individualized dignity-based review). This constraint is downstream of the mandatory_minimum_sentencing_regime (which it partially reverses) and affects the operation of juvenile_justice_discretion (which it mandates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
