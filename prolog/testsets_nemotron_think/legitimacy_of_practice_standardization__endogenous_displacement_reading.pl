% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Displacement as Legitimacy Criterion for Practice Standardization
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint story models the endogenous displacement reading of the
 *   legitimacy kernel for practice standardization. The reading holds that
 *   practice change is legitimate only when it emerges from voluntary
 *   adoption driven by perceived utility or cultural evolution — not state
 *   decree, not international pressure, not colonial imposition.
 *   Historically, this predicts gradual adoption curves with regional
 *   variation, elite-to-mass diffusion, temporary friction as 'double life'
 *   transitional phases, and resistance that dissipates as utility becomes
 *   evident. The constraint is the endogenous displacement mechanism itself:
 *   a distributed, non-coercive process that coordinates practice evolution
 *   across generations. It claims Rope status — genuine coordination with
 *   minimal extraction. But the metrics reveal low-level extraction: early
 *   adopters bear disproportionate transition costs, state modernizers are
 *   delegitimized, and marginalized voices never enter the diffusion process.
 *   The claimed Rope type and the authored metrics are independent; the
 *   engine will compute whether the seat-level classifications match the
 *   claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Displacement as Legitimacy Criterion for Practice Standardization").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '476e8e6b-46d2-45ed-b9f2-1c87c95d8436').
narrative_ontology:cs_kernel_codification('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', distributed).
narrative_ontology:cs_authority_grounding('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', practice).
narrative_ontology:cs_reading_relation('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', foundational, voluntary_adoption_grounds_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_adoption_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', voluntary_adoption_grounds_legitimacy, conventional).
narrative_ontology:cs_axiom('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', secondary, cultural_evolution_is_self_correcting).
narrative_ontology:cs_axiom_status(cultural_evolution_is_self_correcting, holdable).
narrative_ontology:cs_axiom_grounding('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', cultural_evolution_is_self_correcting, empirically_contingent).
narrative_ontology:cs_reference_frame('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', endogenous_legitimacy_standard).
narrative_ontology:cs_drift_state('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', contemporary_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('476e8e6b-46d2-45ed-b9f2-1c87c95d8436', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_innovators).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_modernizers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rapid_coordination_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, ordinary_adopters).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, ordinary_adopters).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_adoption_grounds_legitimacy).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_is_self_correcting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their practices evolve through internal cultural logic; the endogenous displacement criterion treats their path as legitimate. They bear transition costs when practices shift but control the pace and direction. Exit means abandoning constitutive practices — identity-locked.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_communities, beneficiary,
    moderate, generational, identity_locked, local).

% Opinion leaders, intellectuals, local notables who initiate and model new practices. They gain status from driving adoption curves but can shift allegiance if a practice loses prestige. Mobile exit — they move between cultural fields.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_elites, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_elites, beneficiary).

% Bureaucrats, reformers, military officers who need rapid, uniform standardization (calendar, weights, language, dress). The endogenous legitimacy criterion blocks their decree-based approach, forcing slower negotiation or delegitimizing their efforts. Constrained exit — they hold state power but face legitimacy costs.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_modernizers, payer,
    institutional, biographical, constrained, national).

% Merchants, administrators, engineers who need interoperable practices now (common weights, shared calendar, standard gauge). They pay the cost of prolonged fragmentation while waiting for voluntary diffusion. Constrained exit — they can't force adoption but need coordination.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, rapid_coordination_seekers, payer,
    moderate, immediate, constrained, national).

% Experience the 'double life' transitional phase — using both old and new practices. Bear learning costs and social friction during diffusion. Gain reduced transaction costs once adoption completes. Constrained exit — embedded in local networks where practice signals belonging.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, ordinary_adopters, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, ordinary_adopters, beneficiary).

% Analyze the endogenous displacement pattern across cases (Gregorian calendar, metric system, dress reforms). They see the full structure: gradual curves, regional variation, elite-to-mass diffusion. No stake in any particular outcome.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates practice evolution across generations and communities without central authority, reducing friction of divergent practices while preserving local legitimacy. Solves the problem of how societies update shared practices when no single actor can decree the change.
% TRANSFER_FUNCTION: Moves legitimacy authority from central decree to distributed voluntary adoption. Early adopters (cultural elites) bear learning and status risk; benefits diffuse as adoption spreads. Late adopters avoid transition costs but lose agency. No monetary transfer — transfers legitimacy, status, and coordination risk.
% ABSENT_VOICES: Colonized peoples whose endogenous practices were overridden by imperial exogenous standardization; marginalized groups within communities (women, lower castes, minorities) whose practices never gain traction in the elite-to-mass diffusion because they lack cultural authority. Both are structurally excluded from the 'voluntary' process.
% DISAPPEARANCE_RATIONALE: If the endogenous displacement criterion vanished, standardization would default to power-based imposition (state decree, market dominance, imperial fiat). Practices that currently survive through gradual cultural evolution would be replaced by whatever the strongest actor imposes. The legitimacy landscape would shift from 'what spreads organically' to 'what power enforces.'
% FOUNDING_PROBLEM: How do societies coordinate practice change without tyranny? How to legitimate innovation and adaptation without granting any single actor the authority to impose practices on others? The endogenous displacement mechanism answers: legitimacy comes from the adoption process itself, not the imposer.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists (Norbert Elias on civilizing process, Charles Tilly on state formation), anthropologists of practice (Bourdieu on habitus, Scott on seeing like a state), and scholars of diffusion (Rogers) corroborate the pattern from outside the benefiting communities. The operator (state modernizers) disputes the status, arguing the problem is solved by bureaucratic rationality.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the mechanism primarily coordinates — it solves the problem of how practices update without central command. The costs are transition costs borne voluntarily, not rents extracted by a beneficiary class. Suppression is very low (0.1) — no enforcement machinery, only social pressure and legitimacy denial. Theater ratio is low (0.12) — the 'voluntary' framing is largely genuine, though elite-driven diffusion creates a veneer of voluntariness. Accessibility collapse is moderate (0.4) — exogenous alternatives exist but are delegitimized, not eliminated. Resistance is low (0.2) — friction is temporary and resolves as utility proves itself. Measurements show slight upward drift in extractiveness and theater from 1500-2000 as state power increasingly mimics endogenous forms (managed diffusion, nudges) while retaining exogenous ends.
 *
 * PERSPECTIVAL GAP:
 *   From the traditional community seat, the constraint is a protective Rope — it shields their practices from exogenous overwriting. From the state modernizer seat, the same constraint operates as a Snare — it blocks necessary coordination and delegitimizes their rationalizing efforts. From the ordinary adopter seat, it is a Tangled Rope — genuine coordination mixed with elite-driven transition costs. The engine computes this divergence from the structural data; the authored claim (Rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional communities and cultural innovators are beneficiaries (d near 0) — the mechanism legitimates their evolutionary path. Cultural elites are agenda_setters with beneficiary secondary role — they steer diffusion but also gain status. State modernizers and rapid coordination seekers are payers (d near 1) — they bear the cost of slow, uneven standardization. Ordinary adopters are dual payer/beneficiary — they pay transition costs but gain coordination benefits. The engine will compute directionality from these structural declarations. The identity_locked exit for traditional communities reflects that their practices constitute their identity; they cannot exit without ceasing to be that community.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating practice change without tyranny) remains live — modern states still struggle with the legitimacy of imposed standardization (metrication, language policy, calendar reform). The endogenous displacement mechanism has not atrophied; it operates alongside and in tension with exogenous override. No mandatrophy resolution — the arrangement persists because the problem persists. The dual_practice_equilibrium reading captures the stable coexistence in some domains (religious vs civil calendars), but this reading treats dual practice as transitional, not equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the endogenous displacement reading describe a genuine coordination mechanism, or does it rationalize the power of cultural elites who control the diffusion process?',
    'Compare adoption curves where elites genuinely prefer the new practice vs. cases where elites impose practices that serve their interests but diffuse ''voluntarily'' through status signaling. Historical network analysis of diffusion pathways.',
    'If elite-controlled, the low extractiveness metric masks asymmetric status extraction; the constraint reclassifies toward Tangled Rope. If genuine, Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether ''voluntary adoption'' masks elite-driven status competition.').

omega_variable(
    voluntary_coercion_boundary,
    'Where does social pressure for adoption become coercion? The reading treats resistance as ''temporary friction'' but identity-locked communities may experience exclusion as suppression.',
    'Measure post-adoption wellbeing of late adopters vs. never-adopters in historical cases. If never-adopters face material exclusion (market access, legal standing, marriage markets), the suppression metric understates structural coercion.',
    'If material exclusion occurs, suppression rises and the constraint shifts toward Tangled Rope or Snare for identity-locked groups.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_coercion_boundary, empirical, 'Whether social pressure in endogenous diffusion constitutes structural suppression for identity-locked agents.').

omega_variable(
    marginalized_voice_exclusion,
    'The reading''s coordination function assumes a unified ''community'' adopting voluntarily. But marginalized subgroups may never consent — their practices are displaced without voice.',
    'Subaltern studies methodology: recover practices of women, lower castes, ethnic minorities within ''traditional communities'' and trace whether their practices follow the same diffusion curves or are erased.',
    'If marginalized practices are systematically erased, the coordination function is partial and the beneficiary declaration ''traditional_communities'' is falsely unitary. Constraint may be Snare for those subgroups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_voice_exclusion, empirical, 'Whether the endogenous displacement mechanism coordinates for all community members or only the culturally authoritative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 1500, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1500, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(legi_tr_t1600, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement(legi_tr_t1700, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(legi_tr_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1800, 0.11).
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 2000, 0.12).

% Extraction over time
narrative_ontology:measurement(legi_be_t1500, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(legi_be_t1600, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1600, 0.1).
narrative_ontology:measurement(legi_be_t1700, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1700, 0.12).
narrative_ontology:measurement(legi_be_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1800, 0.14).
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1500, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(legi_su_t1600, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1600, 0.07).
narrative_ontology:measurement(legi_su_t1700, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1700, 0.08).
narrative_ontology:measurement(legi_su_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1800, 0.09).
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.08).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the legitimacy_of_practice_standardization constraint family. The endogenous displacement reading (this file) claims Rope status with low extraction. The exogenous override reading claims Scaffold or Tangled Rope (state coordination with sunset or extraction). The dual practice equilibrium reading claims Mountain or Piton (stable domain partition). Their ε values differ because they describe structurally distinct legitimacy criteria operating on different referents: voluntary diffusion vs. state decree vs. institutionalized partition. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
