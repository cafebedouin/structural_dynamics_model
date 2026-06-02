% ============================================================================
% CONSTRAINT STORY: second_amendment_text__hybrid_civic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__hybrid_civic_reading, []).

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
 *   constraint_id: second_amendment_text__hybrid_civic_reading
 *   human_readable: Second Amendment: Hybrid Civic Participation Reading
 *   domain: constitutional_law/political_philosophy/firearms_policy
 *
 * SUMMARY:
 *   The hybrid civic reading of the Second Amendment interprets the
 *   constitutional text as protecting individual ownership of firearms
 *   conditional on civic participation and regulatory compliance. This
 *   reading occupies a middle position between the pure individual-rights
 *   reading (Second Amendment protects an essentially unconditioned right to
 *   bear arms) and the collective-rights reading (Second Amendment protects
 *   militia-related carrying only). The hybrid reading claims to resolve the
 *   tension by asserting that the right is real and individual but
 *   legitimately conditioned on demonstrated civic participation (training,
 *   registration, licensing) and continued compliance with safety
 *   regulations. This constraint demonstrates how a single constitutionaltext
 *   (the kernel) generates three distinct indexical classifications depending
 *   on the observer's structural relationship to the regulatory framework.
 *   The hybrid reading is itself a reading — one possible interpretation
 *   among several — and its analysis reveals how constitutional claims can
 *   instantiate different constraint types from different perspectives.
 *   Extractiveness has risen from 0.35 to 0.55 over the measurement interval
 *   as licensing and registration requirements have proliferated, and
 *   theater_ratio has increased from 0.42 to 0.63 as the performative
 *   elements of compliance (background checks, training documentation) have
 *   expanded relative to their functional verification of safe ownership.
 *
 * KEY AGENTS:
 *   - Trained, Registered Gun Owners: Primary beneficiaries (institutional/arbitrage) — gain clarity, liability protection, and predictable legal framework; incur compliance costs but can afford them
 *   - Excluded Marginalized Populations: Primary victims (powerless/trapped) — lack access due to training costs, licensing fees, lack of residency documentation, or prior criminal records; bear full suppression with no exit
 *   - State Regulatory Authorities: Secondary beneficiary (institutional/arbitrage) — gain enforcement authority and administrative control; frame regulation as coordination
 *   - Gun Rights Organizations: Organized actors (organized/constrained) — view conditioning as temporary scaffold, treating it as a development stage toward deconditioning
 *   - Gun Control Organizations: Organized actors (organized/constrained) — view conditioning as essential coordination for safety; concerned about equitable enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating the conditioning as natural or inherent rather than as contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__hybrid_civic_reading, 0.48).
domain_priors:suppression_score(second_amendment_text__hybrid_civic_reading, 0.62).
domain_priors:theater_ratio(second_amendment_text__hybrid_civic_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__hybrid_civic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(second_amendment_text__hybrid_civic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__hybrid_civic_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__hybrid_civic_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__hybrid_civic_reading, "Second Amendment: Hybrid Civic Participation Reading").
narrative_ontology:topic_domain(second_amendment_text__hybrid_civic_reading, "constitutional_law/political_philosophy/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__hybrid_civic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__hybrid_civic_reading, '7da7d2ed-f907-47b0-8df4-c590ee0a0f6a').
narrative_ontology:cs_kernel_codification('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', fixed_text).
narrative_ontology:cs_authority_grounding('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', lineage).
narrative_ontology:cs_interpretation_layer_present('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a').
narrative_ontology:cs_reading_relation('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', second_amendment_text__individual_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', second_amendment_text__collective_rights_reading, coexists_with).
narrative_ontology:cs_axiom('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', foundational, civic_participation_conditions_right).
narrative_ontology:cs_axiom_status(civic_participation_conditions_right, holdable).
narrative_ontology:cs_axiom_grounding('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', civic_participation_conditions_right, conventional).
narrative_ontology:cs_axiom('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', foundational, individual_ownership_is_protected).
narrative_ontology:cs_axiom_status(individual_ownership_is_protected, holdable).
narrative_ontology:cs_axiom_grounding('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', individual_ownership_is_protected, deontological).
narrative_ontology:cs_reference_frame('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', second_amendment_individual_right_with_civic_conditions).
narrative_ontology:cs_drift_state('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7da7d2ed-f907-47b0-8df4-c590ee0a0f6a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_text__hybrid_civic_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__hybrid_civic_reading, trained_registered_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__hybrid_civic_reading, state_regulatory_authorities).
narrative_ontology:constraint_victim(second_amendment_text__hybrid_civic_reading, unregistered_marginalized_populations).
narrative_ontology:constraint_victim(second_amendment_text__hybrid_civic_reading, communities_without_enforcement_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED POPULATIONS (SNARE) — Unregistered, undocumented, or economically marginalized individuals cannot access the claimed right. Training and licensing requirements create cost barriers; criminal records (including those from over-policing) create permanent exclusion. No meaningful exit from the exclusion; maximum experienced extraction and suppression. The right is available only to those who can afford compliance infrastructure.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLIANT GUN OWNERS (TANGLED ROPE) — Trained, registered owners experience the constraint as both coordination and extraction. Genuine coordination benefit: registration systems enable tracking of stolen weapons, liability frameworks protect owners through predictability, training standards create public safety coordination. But also extraction: licensing fees, training time barriers, ongoing compliance burden, targeting of this group for enforcement scrutiny. Constrained exit — can own, but only under increasingly administered conditions.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY AUTHORITIES (ROPE) — Sees the constraint as pure coordination mechanism. Registration enables theft tracking, training standards reduce accidents, licensing frameworks create liability clarity. Regulatory authorities benefit from clear rules and enforcement authority. The right is conditioned on compliance, which gives the authority structural advantage but the authority frames this as coordination rather than extraction. High arbitrage capacity — can adjust enforcement priorities without legal risk.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GUN RIGHTS ORGANIZATIONS (SCAFFOLD) — These organizations treat the hybrid reading as a temporary compromise position: conditioning rights on civic participation and compliance is acceptable in principle if viewed as a development stage toward full deconditioning. The sunset is implicit — the vision is removal of licensing and registration requirements as citizens are educated and trust is built. Currently constrained by legal challenges to regulations, but see a path toward structural simplification.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GUN CONTROL ORGANIZATIONS (TANGLED ROPE) — These organizations see genuine coordination benefits (registration enabling violence prevention, training reducing accidents, civic participation ensuring responsible ownership) alongside extraction concerns (whether conditioning creates barriers that privilege wealthy groups, whether enforcement is equitable across demographic lines). Constrained by political opposition but structurally embedded in the enforcement system.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL RIGHTS VIEW (MOUNTAIN) — From a civilizational perspective grounded in natural rights theory, this reading risks naturalizing what is actually a contingent political arrangement. The claim that the Second Amendment protects individual ownership 'conditional on civic participation' presents the conditioning as inherent to the right rather than as a regulatory overlay on the right. This perspective sees the hybrid reading as a false summit: the conditioning is institutional, not natural law.
constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__hybrid_civic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_text__hybrid_civic_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__hybrid_civic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__hybrid_civic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The conditioning on civic participation creates measurable barriers to accessing the right. Training and licensing requirements impose financial, temporal, and knowledge burdens. These burdens fall unequally across economic strata and demographic groups. However, the extractiveness is not maximal (0.70+) because the conditioning can be framed (and partly functions) as legitimate safety coordination. The hybrid reading explicitly claims that the conditioning serves genuine civic purposes (public safety, liability clarity, accountability). State regulatory authorities genuinely benefit from registration (theft tracking, forensic matching), so there is a real coordination function, not pure extraction. Suppression (0.62): High. The barriers to accessing the conditioned right are substantial. Excluded populations face structural prevention (undocumented status), economic barriers (training and licensing costs), legal barriers (criminal records triggering permanent exclusion), and informational barriers (lack of knowledge of how to comply). These barriers have increased over the measurement interval. Theater ratio (0.58): Moderate-high. Some aspects of compliance are genuinely functional (background checks reduce some categories of prohibited ownership; training reduces accident rates). But significant performative elements exist: the emotional labor of licensing/registration, the visibility-for-its-own-sake of some compliance documentation, and the demonstration-of-deference embedded in the licensing process. Theater has increased as the number of compliance steps has grown without proportional gains in public safety outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits divergent classifications across the observation sites. Excluded populations see a snare — barriers they cannot overcome, no exit, full suppression. Compliant owners see tangled rope — genuine coordination benefits (liability clarity, registration theft tracking) mixed with extraction (fees, time burden, ongoing compliance friction). State authorities see rope — pure coordination, no extraction from their perspective. Gun rights organizations see a temporary scaffold with a sunset trajectory. Gun control organizations see tangled rope with emphasis on the coordination function. The analytical observer's natural-rights perspective risks seeing a mountain (conditioning as inherent to how rights work) but the structural data reveals this as a false summit: the conditioning is a contingent institutional arrangement, not a natural law. The perspectival gap reveals that whether the Second Amendment protection is conditional or unconditional depends entirely on who you are — your economic status, documentation status, prior criminal record, geographic access to registrars, and knowledge of compliance procedures all determine whether the right is accessible to you.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the constraint. Excluded populations (powerless + trapped) experience d ≈ 0.95, producing high f(d) and high experienced extraction chi. Compliant owners (moderate + constrained) experience d ≈ 0.55, producing moderate f(d) and moderate chi. State authorities (institutional + arbitrage) experience d ≈ 0.10, producing low f(d) and negative chi (they benefit from the framework). Gun rights organizations (organized + constrained) experience d ≈ 0.40, producing moderate f(d). Gun control organizations (organized + constrained) experience d ≈ 0.35, experiencing coordination benefits. The analytical observer (analytical + analytical) experiences canonical d ≈ 0.72, but this observer position is itself captured by the natural-rights framing of the mountain perspective, instantiating the oracle gap: the analytical position cannot see the institutional structure from within the natural-law frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how a single constitutional text generates different constraint classifications depending on the reading applied and the observer's structural position. The mandatrophy question — 'Is this coordination or extraction?' — has a perspectival answer: for excluded populations, it is extraction; for state authorities, it is coordination; for compliant owners, it is both. The hybrid reading itself is an attempt to resolve the mandatrophy by claiming that conditioning rights on civic participation is a form of coordination, not extraction. However, the structural data reveals that the conditioning produces sharply differentiated outcomes across demographic and economic groups. The reading's coherence depends on whether civic participation requirements are equitably accessible. If they are inequitably distributed (high confidence from omega_3), the reading collapses toward snare for excluded populations, revealing the false-summit structure: 'conditioning on civic participation' naturalizes what is actually a mechanism for concentrating rights among economically privileged groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_participation_definition_ambiguity,
    'What constitutes ''civic participation'' for purposes of Second Amendment protection under this reading? Is it explicitly codified or emergent from regulatory practice?',
    'Analysis of case law defining civic participation; comparison across state regulatory frameworks; tracking whether courts narrow or expand the definition over time',
    'If narrowly codified: the right becomes conditional on specific, observable civic acts (training completion, licensing renewal), producing clear extraction benchmarks. If emergent/vague: regulatory authorities gain de facto discretion to determine who qualifies, increasing effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_participation_definition_ambiguity, conceptual, 'Definition and scope of required civic participation').

omega_variable(
    registration_as_rights_prerequisite_or_enforcement_tool,
    'Is registration a logical prerequisite for exercising the right (part of the right''s definition) or an enforcement mechanism applied after the right is granted (separate from the right)?',
    'Historical analysis of Second Amendment jurisprudence (Heller, McDonald, Bruen); conceptual analysis of whether registration precedes or follows right recognition; comparative constitutional law (other democracies'' approaches)',
    'If prerequisite: the right is inherently conditional, supporting tangled rope classification. If enforcement tool: registration is an administrative mechanism applied to a pre-existing right, which would suggest snare-from-excluded-populations but rope-from-compliant-owners.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(registration_as_rights_prerequisite_or_enforcement_tool, conceptual, 'Registration status: prerequisite or enforcement mechanism').

omega_variable(
    equity_of_compliance_burden_across_demographics,
    'Do registration, training, and licensing requirements create equal access across socioeconomic and demographic groups, or do they systematically exclude lower-income, undocumented, and over-policed populations?',
    'Empirical analysis of training availability and cost; licensing fee structure and income-relative burden; geographic access to registrars; correlation between demographic group and exclusion rates',
    'If equitable: victims classification narrows to true exclusions (undocumented persons). If inequitable: victims category expands to include systematically excluded demographics, raising snare magnitude and supporting false-summit detection (natural conditioning logic masks structural extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_of_compliance_burden_across_demographics, empirical, 'Equity of compliance burden across demographic and economic groups').

omega_variable(
    conditioning_as_reading_or_doctrinal_imposition,
    'Is the conditioning (rights protected conditional on civic participation and compliance) inherent to the hybrid reading''s textual interpretation of the Second Amendment, or is it imported from external policy preferences?',
    'Textual analysis of the Second Amendment in comparison to other conditional rights in the Constitution (e.g., voting); historical documentation of founding-era militia participation norms; analysis of how different textual readings produce different conditioning structures',
    'If inherent to text: the hybrid reading is a defensible constitutional claim with equal standing to individual-rights and collective-rights readings. If imported: the reading is actually one party''s policy preference wrapped in constitutional language, shifting the classification boundary and affecting sibling-reading relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditioning_as_reading_or_doctrinal_imposition, conceptual, 'Textual vs. doctrinal basis for conditioning requirement').

omega_variable(
    kernel_reading_identity_ambiguity,
    'Which kernel is this reading actually interpreting — the constitutional text of the Second Amendment itself, or the post-Heller doctrinal framework that interprets individual rights to own firearms?',
    'Comparison of this reading''s foundational axioms against pre-Heller jurisprudence; analysis of whether Heller''s individual-rights shift changed the kernel or changed the reading framework',
    'If the kernel is the constitutional text: all three readings (individual, collective, hybrid) are equally valid interpretations of the same source. If the kernel is the Heller doctrine: the hybrid reading is a constrained variant of the individual-rights kernel, not a co-equal sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_ambiguity, conceptual, 'Identity of the kernel: constitutional text vs. post-Heller doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__hybrid_civic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_hybrid_tr_t0, second_amendment_text__hybrid_civic_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sa_hybrid_tr_t15, second_amendment_text__hybrid_civic_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(sa_hybrid_tr_t30, second_amendment_text__hybrid_civic_reading, theater_ratio, 30, 0.63).

% Extraction over time
narrative_ontology:measurement(sa_hybrid_be_t0, second_amendment_text__hybrid_civic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_hybrid_be_t15, second_amendment_text__hybrid_civic_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(sa_hybrid_be_t30, second_amendment_text__hybrid_civic_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(sa_hybrid_su_t0, second_amendment_text__hybrid_civic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sa_hybrid_su_t15, second_amendment_text__hybrid_civic_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(sa_hybrid_su_t30, second_amendment_text__hybrid_civic_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__hybrid_civic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__hybrid_civic_reading, second_amendment_text__individual_rights_reading).
narrative_ontology:affects_constraint(second_amendment_text__hybrid_civic_reading, second_amendment_text__collective_rights_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel generates three distinct constraints, one for each interpretive reading. This constraint (hybrid_civic_reading) is the middle position, coexisting with both the unconditioned individual-rights reading and the militia-only collective-rights reading. All three stories are linked as siblings within the same kernel family. Each reading produces different beneficiary/victim structures and different ε values because each reading changes the structural relationship between the text, the regulatory frameworks, and the agents affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
