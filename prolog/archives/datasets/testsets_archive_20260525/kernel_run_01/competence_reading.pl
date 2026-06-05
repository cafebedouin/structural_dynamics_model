% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_reading
 *   human_readable: Competence Reading of Preparedness Retention: Live Exercised Knowledge
 *   domain: disaster_preparedness/institutional_memory/water_management
 *
 * SUMMARY:
 *   The competence reading of preparedness retention asserts that flood
 *   response capacity is maintained through continuous live exercised
 *   knowledge: drills test real capacity, knowledge must be embodied and
 *   continuously refreshed, and the constraint is a tangled rope mixing
 *   genuine coordination (competence maintenance) with asymmetric extraction
 *   (the cost to operational teams and institutional budgets). This reading
 *   is ONE of two structurally distinct interpretations of the preparedness
 *   retention kernel. The sibling reading (husk_reading) claims that
 *   preparedness is actually maintained through institutional documentation
 *   and formal training pipelines, and that live drills are largely
 *   theatrical — satisfying stakeholder expectations and organizational
 *   ritual while actual emergency response depends on ad-hoc improvisation.
 *   The competence reading sees drills as non-substitutable; the husk reading
 *   sees them as theater with diminishing returns. Both readings derive from
 *   the same observable phenomena but interpret the causal mechanism and
 *   structural necessity differently.
 *
 * KEY AGENTS:
 *   - Flood Response Teams: Field operators (powerless/trapped) — bear the cost of continuous drill participation; their competence exists only through active rehearsal; trapped in the exercise cycle.
 *   - Water Management Agency: Institutional actor (moderate/constrained) — benefits from actual response capacity but bears drill costs; experiences both coordination and extraction; constrained by the necessity of embodied knowledge.
 *   - Emergency Coordinator: Beneficiary (institutional/arbitrage) — derives professional value from drills; experiences pure coordination; has exit options (consulting, other jurisdictions) and uses drills strategically.
 *   - Documentation System: Institutional artifact (institutional/arbitrage) — persists through manuals, certifications, training pipelines; appears to maintain knowledge but may be decoupled from actual competence.
 *   - Preparedness Reform Coalition: Organized agents (organized/constrained) — pushing for digital knowledge systems and modular competence standards; see current model as temporary; have a sunset strategy.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the drill-intensive model as immutable law of competence; must confront empirical evidence that other systems achieve similar capacity with different architectures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.38).
domain_priors:suppression_score(competence_reading, 0.52).
domain_priors:theater_ratio(competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, tangled_rope).
narrative_ontology:human_readable(competence_reading, "Competence Reading of Preparedness Retention: Live Exercised Knowledge").
narrative_ontology:topic_domain(competence_reading, "disaster_preparedness/institutional_memory/water_management").

domain_priors:requires_active_enforcement(competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(competence_reading, distributed).
narrative_ontology:cs_authority_grounding(competence_reading, practice).
narrative_ontology:cs_kernel_id(competence_reading, preparedness_retention).
narrative_ontology:cs_reading_relation(competence_reading, husk_reading, coexists_with).
narrative_ontology:cs_axiom(competence_reading, foundational, embodied_knowledge_non_substitutable).
narrative_ontology:cs_axiom_status(embodied_knowledge_non_substitutable, holdable).
narrative_ontology:cs_axiom(competence_reading, foundational, drills_functionally_necessary).
narrative_ontology:cs_axiom_status(drills_functionally_necessary, holdable).
narrative_ontology:cs_reference_frame(competence_reading, active_exercised_competence).
narrative_ontology:cs_drift_state(competence_reading, contemporary_institutional_pressure, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, flood_response_capacity).
narrative_ontology:constraint_beneficiary(competence_reading, operational_teams).
narrative_ontology:constraint_victim(competence_reading, institutional_efficiency_budget).
narrative_ontology:constraint_victim(competence_reading, routine_operations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD OPERATOR (SNARE) — Trapped in continuous drill cycles; bears the cost of muscle memory maintenance without choosing it. Cannot exit the exercise regimen; their capacity exists only through constant rehearsal. The constraint extracts availability and cognitive load from them under the framing of 'readiness.' Suppression is high: operators cannot refuse drills or reduce participation without breaking the competence chain. Extraction is moderate-high because the drills themselves are the only mechanism that preserves their capability — paradoxically, the extraction mechanism IS what keeps them operationally alive.
constraint_indexing:constraint_classification(competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WATER MANAGEMENT AGENCY (TANGLED ROPE) — Experiences genuine coordination function (drills maintain team cohesion, test equipment, identify gaps) alongside asymmetric extraction (drills are expensive, disrupt normal operations, force budget reallocations). The agency benefits from having real flood response capacity but bears the cost of continuous maintenance. Exit options are constrained: they cannot simply hire static capacity or buy preparedness off-the-shelf — competence requires active exercise. The constraint asymmetrically favors actual response capability (benefits) over administrative efficiency (costs).
constraint_indexing:constraint_classification(competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMERGENCY COORDINATOR (ROPE) — Arbitrage exit option (can move to private consulting, NGO work, or other jurisdictions). Benefits from the drills: each exercise strengthens inter-agency relationships, reveals system weaknesses, and provides data for grant applications and emergency management training. Experiences the constraint as pure coordination — the exercise mechanism is the primary tool they use to build resilience. Net beneficiary; experiences low effective extraction.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: DOCUMENTATION SYSTEM (PITON) — Written disaster plans, training manuals, and certification processes persist as institutional artifacts even when disconnected from actual capacity. Theater ratio is high: organizations maintain elaborate documentation and formal training pipelines that appear to preserve knowledge but do not exercise it. The piton reading asks: is the documented knowledge (manuals, certifications, credential systems) actually maintaining competence, or is it theatrical maintenance? If living drills stop, the documents persist but the capacity evaporates — suggesting the theater is the decoupling mechanism.
constraint_indexing:constraint_classification(competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized actors (disaster recovery organizations, insurance bodies, environmental agencies) pushing for digitized knowledge systems, modular competence standards, and distributed team rotation. They see the current drill-intensive model as temporary (sunset clause: 15-20 years as technology enables more efficient competence maintenance). The constraint has low effective extraction from this perspective because the coalition has agency and a clear exit path: if digital systems can encode tacit knowledge and adaptive teaming can distribute cognitive load, live drills become less necessary. This is a real structural feature: preparedness systems ARE transitioning toward hybrid (documented + modularly exercised) models.
constraint_indexing:constraint_classification(competence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — Views the constraint as an immutable feature of how competence works: complex procedural knowledge in high-stakes domains cannot be preserved through documentation alone; it requires continuous embodied practice. From this angle, the drill-intensive model is not extractive but rather an unavoidable tax on maintaining collective capacity in domains where failures are catastrophic. The constraint appears as a mountain: the law of skill atrophy, the physics of institutional memory, the logic of complex procedural knowledge. However, this risks false-summit naturalization — the analytical view must confront the empirical reality that other jurisdictions have reduced drill frequency through digital knowledge systems and team rotation schedules without losing capacity. The mountain classification naturalizes what may be a contingent institutional choice.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_reading, TR),
    TR >= 0.70.

:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint requires continuous resource expenditure (drills are costly in time, equipment, and opportunity cost), and it asymmetrically benefits those who coordinate the system (coordinators, agencies) more than those who execute it (field teams). However, extractiveness is NOT high because the benefit is real — the extraction finances actual emergency response capacity. The cost is not pure rent-seeking; it purchases genuine coordination function. The rise from 0.28 to 0.38 over the 10-year interval reflects increasing formalization of drill requirements, professionalization of training pipelines, and accumulating certification mandates. Suppression (0.52): Moderate-high. Field operators have limited ability to opt out of drills — refusal breaks the competence chain they depend on. Institutional efficiency gains are suppressed: agencies cannot reallocate drill budgets to routine operations without risking response capacity. However, suppression is not total (0.70+) because documented knowledge and digital systems offer partial alternatives, and some jurisdictions have successfully reduced drill frequency. Theater ratio (0.35): Low. This reading minimizes theater — the drills are primarily functional (actually test decision-making, equipment, inter-team coordination) rather than performative. The low theater distinguishes the competence reading from the husk reading, which would claim theater_ratio > 0.70.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading produces substantial perspectival differentiation. Field operators trapped in drills perceive pure extraction (snare). The water agency perceives both coordination and extraction (tangled rope) — the constraint solves a real problem (maintaining competence) but imposes asymmetric costs. Emergency coordinators perceive pure coordination (rope) — drills are their primary tool for building resilience and inter-agency relationships. The documentation system appears as a decoupled artifact (piton) — manuals and certifications persist through institutional inertia even when disconnected from actual competence. The reform coalition perceives temporary coordination (scaffold) — digital and modular alternatives are emerging that could reduce drill burden. The analytical observer risks naturalizing all of this as immutable (mountain) — viewing embodied knowledge maintenance as a law of nature rather than a contingent institutional choice. The perspectival gaps are real: different positions in the constraint structure yield genuinely different classifications. This is not noise; it is the engine working correctly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (flood response capacity, operational teams) experience the constraint as coordination necessity — they derive existence from the drill system. The agency experiences asymmetric costs (must fund drills, divert budget) and asymmetric benefits (gains response capacity). Field operators experience high extraction despite being non-beneficiaries: they are trapped in the exercise cycle and bear the cognitive load. Directionality derivation: field operators (powerless/trapped) yield d ≈ 0.95, producing high f(d) ≈ 1.42, resulting in experienced χ high. Agency actors (moderate/constrained) yield d ≈ 0.55, producing f(d) ≈ 0.75, resulting in moderate χ. Coordinators (institutional/arbitrage) yield d ≈ 0.15, producing f(d) ≈ -0.01, resulting in low or negative χ. The perspectival gap reflects this: field operators see snare, agency sees tangled rope, coordinators see rope. No directionality overrides needed; the derivation chain captures the actual structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence reading resolves mandatrophy by grounding the tangled rope classification in a genuine structural duality: drills serve both a coordination function (maintaining real response capacity) and an extraction function (imposing costs on field operators and institutional budgets). The tangled rope is the correct classification because both functions are present and asymmetric. The piton perspective on the documentation system is a key diagnostic: if the theater_ratio of the overall constraint were high (0.70+), it would suggest that drills are primarily performative. But the competence reading maintains low theater (0.35) because drills actually exercise decision-making, equipment, and inter-team coordination. This distinguishes competence from husk: the husk reading would argue that theater is high and rising, that documentation is sufficient, and that drills are increasingly decoupled from actual capacity. The competence reading says the opposite: theater is low and stable because drills are functionally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_knowledge_threshold,
    'What proportion of flood response competence cannot be preserved through documentation and must be actively exercised?',
    'Comparative analysis: measure response performance (decision latency, error rates, coordination effectiveness) in jurisdictions with high-drill vs. low-drill regimes; correlate against documented knowledge quality, simulation-based training, and team rotation frequency. Isolate the variance attributable to live drill participation vs. other competence-maintenance mechanisms.',
    'If threshold > 80%: competence reading is correct; drills are non-substitutable, constraint is legitimately tangled rope. If threshold < 40%: husk reading gains traction; much of the drill extraction is theatrical, constraint is actually piton. If 40-80%: both readings coexist; the constraint is hybrid (competence + theater), and the reading_relations ''coexists_with'' is empirically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_knowledge_threshold, empirical, 'Proportion of flood response competence requiring active exercise vs. documentation').

omega_variable(
    digital_knowledge_encoding_feasibility,
    'Can digital knowledge systems (decision trees, scenario simulations, adaptive learning platforms) encode the tacit knowledge currently maintained through live drills?',
    'Technology audit of preparedness systems in high-digital environments (Netherlands, Singapore, Japan); measure coverage of decision space in digital vs. live-drill formats; identify decision types that digital systems handle well vs. those requiring embodied practice; cost-benefit analysis of hybrid (digital + minimal live drills) vs. current (extensive live drills) models.',
    'If digital systems achieve >70% coverage of critical decisions: scaffold sunset becomes structurally real; digital transition can reduce drill burden. If <40% coverage: digital systems are complementary tools, not substitutes; the constraint remains extraction-necessary. This resolution directly determines whether the scaffold perspective''s sunset clause is aspirational or structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(digital_knowledge_encoding_feasibility, empirical, 'Technical feasibility of digital knowledge systems replacing live drills').

omega_variable(
    kernel_reading_ambiguity,
    'Is preparedness retention fundamentally about maintaining real competence (competence reading) or about maintaining institutional confidence that competence exists (husk reading)?',
    'Post-disaster performance analysis: compare actual response quality to pre-disaster drill frequency and documentation completeness. Identify cases where high-drill jurisdictions failed and high-documentation jurisdictions succeeded (or vice versa). Interview disaster managers about what they actually rely on during response — drills or documented procedures. Measure belief vs. behavior: do decision-makers behave as though competence is embodied (keep funding drills) or documented (allow drill reduction)?',
    'If evidence favors competence: this reading is structurally sound, the husk reading is misdiagnosis. If evidence favors husk: this reading is false confidence in embodied knowledge, the constraint''s extraction is theater masquerading as necessity. If mixed evidence: both readings coexist, and the kernel is genuinely ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether preparedness requires real competence (this reading) or institutional confidence in documented competence').

omega_variable(
    interdisciplinary_knowledge_transfer,
    'Can preparedness competence be maintained through cross-disciplinary team rotation (bringing in fresh personnel systematically) combined with documented knowledge transfer, or does it require stable team continuity with live drills?',
    'Case study analysis of organizations with high personnel turnover (military units, NGO disaster teams) vs. stable teams. Measure performance trajectories before and after rotation events. Identify whether drills serve primarily to maintain individual skill or to integrate new team members into existing norms. If rotation + documentation is sufficient, the constraint''s extraction mechanism (continuous drill enforcement on existing teams) is contingent, not necessary.',
    'If rotation + documentation sufficient: the constraint is a choice to maintain stable teams, not an immutable requirement. Extraction is reduced in this framing, possibly pushing classification toward rope or even piton. If rotation fails without extensive drills: embodied team knowledge is non-transferable, competence reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interdisciplinary_knowledge_transfer, empirical, 'Whether cross-disciplinary team rotation plus documentation can substitute for live drill maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_theater_t0, competence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(comp_theater_t5, competence_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(comp_theater_t10, competence_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(comp_extr_t0, competence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_extr_t5, competence_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(comp_extr_t10, competence_reading, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_reading, institutional_memory_erosion).
narrative_ontology:affects_constraint(competence_reading, budget_allocation_under_uncertainty).

% DUAL FORMULATION NOTE:
% The competence reading is paired with husk_reading as two structurally distinct claims about the same kernel (preparedness_retention). The competence reading emphasizes real capacity maintenance (low theater, genuine coordination); the husk reading emphasizes institutional theater and documentation theater. Separate story files allow each reading to maintain ε-invariance: competence_reading has theater_ratio=0.35 (low), husk_reading will have theater_ratio>=0.70 (high). The network edge indicates that institutional_memory_erosion (whether institutional knowledge actually decays without active maintenance) determines which reading is empirically correct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
