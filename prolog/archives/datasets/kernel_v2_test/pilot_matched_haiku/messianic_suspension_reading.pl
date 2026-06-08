% ============================================================================
% CONSTRAINT STORY: messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_messianic_suspension_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: messianic_suspension_reading
 *   human_readable: Messianic Suspension of Sacrifice Obligation with Study-Based Operational Readiness
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   The messianic suspension reading interprets the obligation to bring
 *   sacrifices as divinely suspended (not transformed or eliminated) until
 *   the messianic restoration, when the Temple will be rebuilt and sacrifice
 *   will resume. During the suspension period, which has lasted approximately
 *   2,000 years since the Temple's destruction, the obligation remains
 *   binding in principle but cannot be fulfilled in practice. The reading
 *   resolves this tension through a coordination mechanism: study of the
 *   sacrificial laws maintains the community's operational readiness and
 *   preserves the knowledge required for restoration. This reading treats the
 *   suspension as temporary (with an explicit sunset clause: messianic
 *   restoration) and the study-based readiness as a legitimate substitute
 *   during the suspension period. The constraint exhibits low extractiveness
 *   (0.15) because the coordination function is transparent, the beneficiary
 *   (future generations and the tradition itself) is not predatory, and the
 *   obligation's binding force is preserved rather than exploited. The
 *   reading coexists with three sibling readings that interpret the same
 *   kernel (the obligation to bring sacrifices) differently: the
 *   study_as_exercise_reading treats study as an end in itself rather than
 *   instrumental readiness; the performance_only_reading treats the
 *   obligation as purely symbolic or performative; the
 *   symbolic_archive_reading treats the obligation as a historical artifact
 *   to be preserved but not actively maintained. This story instantiates the
 *   messianic_suspension_reading as a clean, ε-invariant constraint with its
 *   own beneficiary structure, authority grounding, and sunset clause.
 *
 * KEY AGENTS:
 *   - Observant Community: Primary actor (powerless/constrained) — maintains study-based readiness; experiences the suspension as legitimate coordination rather than extraction
 *   - Halakhic Authority Structure: Organized actor (organized/constrained) — rabbinic courts and yeshiva networks that maintain interpretive lineages and textual transmission; benefits from the suspension's legitimacy
 *   - Jewish Legal Tradition: Institutional beneficiary (institutional/arbitrage) — the tradition as a whole benefits from the suspension framework by preserving the obligation's binding force while adapting to historical circumstance
 *   - Future Generations: Primary beneficiary (powerless/mobile) — the suspension's explicit endpoint; the knowledge preserved through study is maintained for their benefit when restoration occurs
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a scaffold with a declared sunset clause (messianic restoration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(messianic_suspension_reading, 0.15).
domain_priors:suppression_score(messianic_suspension_reading, 0.25).
domain_priors:theater_ratio(messianic_suspension_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(messianic_suspension_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(messianic_suspension_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(messianic_suspension_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(messianic_suspension_reading, rope).
narrative_ontology:human_readable(messianic_suspension_reading, "Messianic Suspension of Sacrifice Obligation with Study-Based Operational Readiness").
narrative_ontology:topic_domain(messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system").

narrative_ontology:has_sunset_clause(messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(messianic_suspension_reading, '8aad102b-f19c-4bfc-bdb0-4c8c23ad134d').
narrative_ontology:cs_kernel_codification('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', fixed_text).
narrative_ontology:cs_authority_grounding('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', lineage).
narrative_ontology:cs_interpretation_layer_present('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d').
narrative_ontology:cs_reading_relation('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', messianic_suspension_reading__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', messianic_suspension_reading__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', messianic_suspension_reading__symbolic_archive_reading, influences).
narrative_ontology:cs_axiom('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', foundational, obligation_suspended_not_transformed).
narrative_ontology:cs_axiom_status(obligation_suspended_not_transformed, holdable).
narrative_ontology:cs_axiom_grounding('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', obligation_suspended_not_transformed, deontological).
narrative_ontology:cs_axiom('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', foundational, study_maintains_operational_readiness).
narrative_ontology:cs_axiom_status(study_maintains_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', study_maintains_operational_readiness, instrumental).
narrative_ontology:cs_reference_frame('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', obligation_suspended_until_restoration).
narrative_ontology:cs_drift_state('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8aad102b-f19c-4bfc-bdb0-4c8c23ad134d', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, future_generations).
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, jewish_legal_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(messianic_suspension_reading, halakhic_authority).
narrative_ontology:constraint_vindicates(messianic_suspension_reading, divine_authority_over_obligation_suspension).
narrative_ontology:constraint_vindicates(messianic_suspension_reading, study_as_legitimate_substitute_during_suspension).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The observant community maintains study-based readiness for the obligation's restoration. They set the agenda for how the suspension is understood and practiced through their engagement with the halakhic tradition. They bear the cost of maintaining study infrastructure (yeshivas, textual transmission) but experience this as legitimate coordination rather than extraction. Exit would require abandoning the obligation's binding force, which is constrained by religious identity and community membership.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, observant_community, agenda_setter,
    powerless, generational, constrained, global).

% Rabbinic courts and yeshiva networks set the agenda for interpreting the suspension and maintaining readiness. They benefit from the suspension's legitimacy (it grounds their interpretive role) but also bear the cost of maintaining institutional infrastructure. Their exit options are constrained by the need to maintain authority within the tradition.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, halakhic_authority, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(messianic_suspension_reading, halakhic_authority, beneficiary).

% The tradition as an institutional actor benefits from the suspension framework by preserving the obligation's binding force while adapting to historical circumstance. The tradition has arbitrage options (could reframe the obligation as symbolic-only, could declare it permanently transformed) but chooses the suspension reading because it maintains both the obligation's authority and the possibility of restoration.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, jewish_legal_tradition, beneficiary,
    institutional, civilizational, arbitrage, global).

% Future generations are the explicit beneficiary of the suspension framework. The knowledge preserved through study-based readiness is maintained for their benefit when messianic restoration occurs. They have mobile exit options (could choose not to engage with the tradition) but are positioned to benefit from the knowledge preserved during the suspension period.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, future_generations, beneficiary,
    powerless, generational, mobile, global).

% Messianic restoration is the explicit endpoint of the suspension. It is not an agent but a future event that grounds the constraint's sunset clause. The restoration is excluded from the present conversation (it is a future event) but is central to the constraint's legitimacy and function.
narrative_ontology:constraint_stakeholder(messianic_suspension_reading, messianic_restoration, excluded,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(messianic_suspension_reading, messianic_restoration).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates the preservation of knowledge about the sacrificial laws and the maintenance of institutional readiness for the obligation's restoration. The real coordination problem is: how does a religious community maintain the binding force of an obligation that cannot be fulfilled in practice? The suspension reading solves this by treating the obligation as suspended (not violated) and study-based readiness as a legitimate substitute during the suspension period.
% TRANSFER_FUNCTION: The constraint transfers knowledge and institutional responsibility from the present generation to future generations. Study-based readiness requires ongoing investment in yeshivas, textual transmission, and interpretive lineages. This investment flows from the observant community and halakhic authority to the tradition as a whole, with the benefit accruing to future generations when restoration occurs.
% ABSENT_VOICES: The performance_only_reading and symbolic_archive_reading represent absent voices in the present conversation. These readings would argue that the obligation has been transformed or that study is not instrumental readiness but an end in itself. These voices are excluded from the messianic_suspension_reading's framework but represent live alternative interpretations of the same kernel.
% DISAPPEARANCE_RATIONALE: If the messianic_suspension_reading disappeared, the observant community would need to adopt a different reading of the obligation (study_as_exercise, performance_only, or symbolic_archive). This would rearrange how the obligation is understood, practiced, and transmitted. The institutional infrastructure for maintaining readiness (yeshivas, textual transmission) would persist but would be justified differently. The constraint's disappearance would not eliminate the obligation itself but would change how the community relates to it.
% FOUNDING_PROBLEM: The founding problem is the destruction of the Second Temple in 70 CE, which made the obligation to bring sacrifices impossible to fulfill. The halakhic tradition needed to resolve the tension between the obligation's binding force (it is commanded in the Torah) and its practical impossibility (the Temple no longer exists). The messianic_suspension_reading solves this by treating the obligation as suspended until messianic restoration.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by historical fact (the Temple's destruction in 70 CE) and by the halakhic tradition's ongoing engagement with the question of how to relate to the obligation during the suspension period. The problem remains live because the Temple has not been rebuilt and messianic restoration has not occurred. The problem is attested by multiple sources: the Talmud (Menachot 110a, discussing the obligation's status after the Temple's destruction), Maimonides (Mishneh Torah, Hilkhot Korbanot, treating the obligation as suspended), and contemporary rabbinic authorities who continue to debate the obligation's status and the legitimacy of study-based readiness.
narrative_ontology:disappearance_verdict(messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(messianic_suspension_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT COMMUNITY (ROPE) — Constrained by the suspension obligation but experiences it as legitimate coordination rather than extraction. The community maintains study-based readiness as a genuine collective good: preserving knowledge for future restoration. No victim set during suspension — the obligation is in abeyance, not violated. Moderate constraint but low extractiveness because the coordination function (knowledge preservation) is transparent and the beneficiary (future generations) is not a predatory actor.
constraint_indexing:constraint_classification(messianic_suspension_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: HALAKHIC AUTHORITY (ROPE) — Organized actors (rabbinic courts, yeshiva networks) experience the suspension as a coordination mechanism: study-based readiness requires institutional infrastructure (yeshivas, textual transmission, interpretive lineages). The authority structure benefits from the suspension's legitimacy (it grounds their interpretive role) but also bears the cost of maintaining readiness. Coordination function is clear; extraction is minimal because the authority's benefit is tied to the constraint's legitimacy, not to suppressing alternatives.
constraint_indexing:constraint_classification(messianic_suspension_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: JEWISH LEGAL TRADITION (ROPE) — The tradition as an institutional actor benefits from the suspension framework: it preserves the obligation's binding force while adapting to historical circumstance (absence of Temple). The tradition has arbitrage options (could reframe the obligation as symbolic-only, could declare it permanently transformed), but chooses the suspension reading because it maintains both the obligation's authority and the possibility of restoration. Net beneficiary with low extractiveness because the benefit is tied to the constraint's legitimacy, not to coercion.
constraint_indexing:constraint_classification(messianic_suspension_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SCAFFOLD) — From a civilizational perspective, the suspension is explicitly temporary: the obligation remains binding until messianic restoration, at which point the Temple will be rebuilt and sacrifice will resume. This is a scaffold constraint with a declared sunset clause (messianic restoration). The constraint's function is transitional: maintaining knowledge and readiness during the suspension period. Low extractiveness because the endpoint is explicit and the constraint's justification is the transition, not the steady state.
constraint_indexing:constraint_classification(messianic_suspension_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(messianic_suspension_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(messianic_suspension_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(messianic_suspension_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The suspension reading treats the obligation as genuinely in abeyance, not violated or transformed. Study-based readiness is framed as instrumental (maintaining capacity for future restoration) rather than substitutive (replacing the obligation). No victim set exists during the suspension period because the obligation is not being extracted from or exploited — it is being preserved. The beneficiary (future generations) is not a predatory actor but a future community that will benefit from the knowledge preserved. The low extractiveness reflects that the coordination function (knowledge preservation) is transparent and the constraint's justification is the transition (maintaining readiness until restoration), not the steady state. Suppression (0.25): Low-moderate. The constraint requires ongoing study and institutional maintenance (yeshivas, textual transmission, interpretive lineages), which imposes costs on the observant community. However, suppression is not high because the community experiences the obligation as legitimate and the study as meaningful, not coercive. The suppression requirement has remained stable over the interval (500-1000 years) because the institutional infrastructure for maintaining readiness has been established and normalized. Theater ratio (0.35): Low-moderate. The study-based readiness is not purely performative — it serves a genuine function (preserving knowledge for restoration). However, some performative elements exist: the study is framed as maintaining readiness for an event (messianic restoration) whose timing is uncertain, which creates some theater around the constraint's endpoint. The theater ratio has remained stable because the reading's authority grounding (lineage, textual tradition) has remained consistent.
 *
 * PERSPECTIVAL GAP:
 *   The messianic suspension reading produces a perspectival gap between the observant community's experience (Rope: legitimate coordination with low extractiveness) and the performance_only_reading's experience (Piton: degraded ritual maintained through inertia). The gap reveals that the constraint's classification depends on whether the suspension is treated as temporary (with a real endpoint) or indefinite (with an uncertain endpoint). The suspension reading treats the endpoint as explicit (messianic restoration), which justifies the scaffold classification from the analytical perspective. The performance_only_reading treats the endpoint as uncertain or aspirational, which would justify a piton classification (indefinite maintenance of readiness). The study_as_exercise_reading treats study as an end in itself rather than instrumental, which would shift the beneficiary structure and potentially increase extractiveness. The symbolic_archive_reading treats the obligation as a historical artifact, which would eliminate the binding force and shift the constraint to a different type entirely. The perspectival gap is not a failure of the framework but a diagnostic signal: the constraint's classification depends on which reading of the kernel is adopted.
 *
 * DIRECTIONALITY LOGIC:
 *   The messianic suspension reading produces low directionality values (d ≈ 0.2-0.3) for the observant community because the constraint is framed as coordination (knowledge preservation) rather than extraction. The beneficiary (future generations) is not a predatory actor, and the community experiences the obligation as legitimate. The halakhic authority structure has moderate directionality (d ≈ 0.3-0.4) because it benefits from the suspension's legitimacy but also bears the cost of maintaining institutional infrastructure. The Jewish legal tradition has low directionality (d ≈ 0.1-0.2) because it benefits from the suspension framework without bearing significant costs — the tradition's benefit is tied to the constraint's legitimacy, not to suppressing alternatives. The analytical observer has zero directionality (d = 0.5, symmetric) because the observer is not a beneficiary or victim but a neutral analyst. The low directionality values across all perspectives reflect that the constraint is experienced as coordination rather than extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The messianic suspension reading resolves the mandatrophy by treating the obligation as genuinely suspended (not violated or transformed) and the study-based readiness as a legitimate substitute during the suspension period. The reading's mandate is to preserve the obligation's binding force while adapting to the historical circumstance of the Temple's absence. The constraint's function (maintaining readiness for restoration) is tied to its endpoint (messianic restoration), which prevents mandatrophy from occurring. If the endpoint were uncertain or aspirational, the constraint would risk becoming a piton (indefinite maintenance of readiness maintained through inertia). The suspension reading's explicit sunset clause (messianic restoration) prevents this degradation by keeping the constraint's function and endpoint aligned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_transformation_ambiguity,
    'Is the obligation genuinely suspended (in abeyance, awaiting restoration) or has it been transformed into a permanent study-based substitute?',
    'Textual analysis of authoritative sources (Talmud, Maimonides, Shulchan Aruch) distinguishing suspension language from transformation language; examination of whether restoration is treated as contingent (dependent on messianic arrival) or impossible (the obligation''s nature has changed)',
    'If suspension: this reading (Rope, low extractiveness, sunset clause) is correct. If transformation: the constraint becomes a different reading (study_as_exercise_reading or performance_only_reading), with different extractiveness and no sunset clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_vs_transformation_ambiguity, conceptual, 'Whether obligation is suspended or permanently transformed').

omega_variable(
    messianic_restoration_contingency,
    'What is the epistemic status of messianic restoration as a contingent future event? Is it treated as historically inevitable, theologically possible but uncertain, or metaphorically aspirational?',
    'Historical analysis of how different Jewish communities have treated the restoration expectation; examination of whether the suspension framework''s legitimacy depends on restoration being treated as inevitable vs. merely possible',
    'If inevitable: the sunset clause is real and the scaffold classification holds. If merely possible: the constraint''s endpoint is uncertain, and the classification shifts toward piton (indefinite maintenance of readiness). If aspirational: the constraint becomes performance-only (different reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_contingency, conceptual, 'Epistemic status of messianic restoration as constraint endpoint').

omega_variable(
    study_substitution_legitimacy,
    'Does study-based readiness constitute a legitimate substitute for the obligation during suspension, or is it a workaround that acknowledges the obligation cannot be fulfilled?',
    'Textual analysis of whether study is framed as equivalent to sacrifice (substitutive) or as instrumental preparation (maintaining capacity); examination of whether study is treated as fulfilling the obligation or as maintaining readiness for future fulfillment',
    'If substitutive: the constraint is a rope (coordination of knowledge preservation). If instrumental: the constraint is still a rope but with different beneficiary structure (future generations rather than present community). If workaround: the constraint may be a tangled rope (coordination + extraction of the obligation''s binding force).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_substitution_legitimacy, empirical, 'Whether study is substitutive or instrumental to the obligation').

omega_variable(
    reading_contest_among_siblings,
    'Which reading (messianic_suspension, study_as_exercise, performance_only, symbolic_archive) is currently dominant in Jewish legal practice and interpretation?',
    'Survey of contemporary rabbinic authorities'' positions; analysis of how different communities (Orthodox, Conservative, Reform) treat the obligation; examination of whether the suspension reading is actively defended or has become implicit/assumed',
    'If suspension is dominant: the reading''s legitimacy is high and the constraint''s classification is stable. If another reading is dominant: the suspension reading may be a minority position or a historical artifact, affecting its authority grounding and interpretation_layer_present status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_among_siblings, empirical, 'Dominance of messianic suspension reading among sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(messianic_suspension_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msr_tr_t0, messianic_suspension_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(msr_tr_t500, messianic_suspension_reading, theater_ratio, 500, 0.35).
narrative_ontology:measurement(msr_tr_t1000, messianic_suspension_reading, theater_ratio, 1000, 0.35).

% Extraction over time
narrative_ontology:measurement(msr_be_t0, messianic_suspension_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(msr_be_t500, messianic_suspension_reading, base_extractiveness, 500, 0.15).
narrative_ontology:measurement(msr_be_t1000, messianic_suspension_reading, base_extractiveness, 1000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(msr_su_t0, messianic_suspension_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(msr_su_t500, messianic_suspension_reading, suppression_requirement, 500, 0.25).
narrative_ontology:measurement(msr_su_t1000, messianic_suspension_reading, suppression_requirement, 1000, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(messianic_suspension_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The messianic_suspension_reading is one of four structurally distinct readings of the sacrifice_obligation_kernel. Each reading has a different ε value, beneficiary structure, and authority grounding. The readings form a constraint family linked by network.affects_constraints. The messianic_suspension_reading (this story) has low extractiveness (0.15) and treats the suspension as temporary with an explicit endpoint. The study_as_exercise_reading has higher extractiveness (estimated 0.35-0.45) and treats study as an end in itself. The performance_only_reading has moderate extractiveness (estimated 0.40-0.50) and treats the obligation as symbolic. The symbolic_archive_reading has low extractiveness (estimated 0.10-0.15) but treats the obligation as a historical artifact rather than binding. Each reading should be authored as a separate constraint story with its own perspectives, omegas, and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
