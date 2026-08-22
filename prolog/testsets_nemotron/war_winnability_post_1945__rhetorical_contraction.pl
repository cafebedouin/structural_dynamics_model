% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Nuclear Winnability Rhetorical Taboo with Operational Persistence
 *   domain: strategic/nuclear/political
 *
 * SUMMARY:
 *   After 1945, the concept of 'winning' a nuclear war underwent a dual-layer
 *   transformation. Publicly, nuclear strategists, political leaders, and
 *   arms control advocates constructed a rhetorical taboo: winnability became
 *   unsayable in official discourse, treaties, and public debate. The Nuclear
 *   Non-Proliferation Treaty, SALT/START processes, and declaratory policies
 *   all treated nuclear war as categorically unwinnable. However, classified
 *   operational planning — targeting doctrines (counterforce, damage
 *   limitation), war plans (SIOP/OPLAN 8010), force posture requirements, and
 *   procurement specifications — continued to treat winnability as a
 *   constrained but reachable objective. The rhetorical contraction did not
 *   eliminate operational winnability planning; it moved it behind
 *   classification barriers. This created a structural gap: strategic
 *   planners and nuclear enterprise institutions retain operational
 *   flexibility without public accountability, while democratic oversight,
 *   civil society debate, and arms control diplomacy are excluded from the
 *   actual planning assumptions. The constraint is a tangled rope because it
 *   solves a genuine coordination problem (deterrence stability through
 *   mutual vulnerability acknowledgment) while simultaneously extracting
 *   democratic accountability through the same discursive structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Nuclear Winnability Rhetorical Taboo with Operational Persistence").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic/nuclear/political").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '6bbcc72a-a203-4c7b-a52c-8e9b536003ca').
narrative_ontology:cs_kernel_codification('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', distributed).
narrative_ontology:cs_authority_grounding('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', extraction).
narrative_ontology:cs_interpretation_layer_present('6bbcc72a-a203-4c7b-a52c-8e9b536003ca').
narrative_ontology:cs_reading_relation('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_axiom('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', foundational, rhetorical_taboo_serves_institutional_autonomy).
narrative_ontology:cs_axiom_status(rhetorical_taboo_serves_institutional_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', rhetorical_taboo_serves_institutional_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', foundational, operational_winnability_planning_persists_despite_declaratory_denial).
narrative_ontology:cs_axiom_status(operational_winnability_planning_persists_despite_declaratory_denial, holdable).
narrative_ontology:cs_axiom_grounding('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', operational_winnability_planning_persists_despite_declaratory_denial, empirically_contingent).
narrative_ontology:cs_reference_frame('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', mutual_vulnerability_declaratory_framework).
narrative_ontology:cs_drift_state('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', contemporary_modernization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6bbcc72a-a203-4c7b-a52c-8e9b536003ca', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_enterprise_institutions).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, civil_society_debate).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, arms_control_diplomacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Career military and civilian strategists within nuclear planning enterprises (STRATCOM, Pentagon, MOD, General Staff). They author and maintain classified war plans (OPLAN 8010, SIOP successors) that treat limited nuclear victory as an operational objective. Their professional identity, clearance level, and institutional role fuse them to the enterprise — exit means leaving the only structure where their expertise operates. They benefit from the taboo: it shields their planning assumptions from public challenge, protects budget lines for counterforce capabilities, and preserves operational flexibility.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, beneficiary,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter).

% The standing institutions of nuclear deterrence: STRATCOM, nuclear labs (LANL/LLNL/Sandia), procurement commands, allied nuclear planning groups. They set the agenda for force posture, targeting requirements, and declaratory policy. They benefit from the taboo as an institutional asset — it justifies classification regimes, protects procurement from oversight, and maintains the enterprise's bureaucratic centrality. Unlike individual planners, these institutions have arbitrage-grade exit: they can reorganize, rebrand, or shift missions while preserving their structural position.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_enterprise_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, nuclear_enterprise_institutions, beneficiary).

% Congressional defense committees, parliamentary oversight bodies, GAO/NAO equivalents. They are structurally excluded from the operational reality of nuclear planning by the classification barrier the taboo maintains. They receive briefings on declaratory policy (unthinkability) but not on operational plans (winnability). Their oversight tools (hearings, budget authority, reporting requirements) operate on the rhetorical layer, not the operational layer. Exit is constrained: they cannot easily acquire independent technical expertise, and challenging the taboo risks being labeled 'soft on deterrence.'
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight, payer,
    organized, biographical, constrained, national).

% Academics, NGOs, journalists, anti-nuclear movements, and informed public discourse. They operate entirely within the rhetorical taboo — the conceptual vocabulary available to them treats winnability as either categorically false (unthinkable) or categorically true (countervailing). The dual-layer reality (rhetorical taboo + operational persistence) is structurally invisible to them because the evidence is classified. They are trapped: they cannot access the operational layer, and their discourse is channeled by the taboo into binary positions that miss the structural extraction.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, civil_society_debate, excluded,
    moderate, generational, trapped, global).

% Negotiators, verification regimes, treaty inspection bodies (New START BCC, IAEA, CTBTO). They negotiate and verify agreements based on declaratory commitments (unthinkability, no-first-use, sole purpose) while the operational plans of the parties they monitor assume winnability. This creates a structural verification gap: treaties constrain declared postures, not operational capabilities. They pay the cost in failed verification, eroded trust, and agreements that drift from operational reality. Exit is constrained: the treaty regime is the only diplomatic structure available, and withdrawing from it carries higher political cost than staying within its fictions.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, arms_control_diplomacy, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, arms_control_diplomacy, payer).

% The analytical seat that sees the full dual-layer structure: the rhetorical taboo in public discourse, the operational persistence in classified planning, the beneficiary/victim asymmetry, and the extraction of democratic accountability. This seat has no stake in the constraint's persistence and full exit freedom — it exists to map the structure, not to inhabit it.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_analyst_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rhetorical taboo coordinates mutual vulnerability acknowledgment between nuclear-armed states, preventing destabilizing 'winnability' rhetoric that could trigger arms racing or preemptive incentives. It creates a shared declaratory framework that makes deterrence stable enough for crisis management.
% TRANSFER_FUNCTION: The constraint transfers democratic accountability (oversight of existential war planning) from the public and their representatives to the nuclear planning enterprise. It transfers epistemic authority (what nuclear war would actually look like) from open discourse to classified channels. It transfers risk (the consequences of plans going wrong) from planners to populations who cannot scrutinize those plans.
% ABSENT_VOICES: Populations of non-nuclear-weapon states under extended deterrence umbrellas (they bear targeting risks without even the nominal oversight nuclear-armed states' legislatures possess). Future generations (the taboo's persistence shapes the nuclear world they inherit, but they have no voice in its maintenance). Whistleblowers and dissident insiders (Daniel Ellsberg, Mordechai Vanunu, lesser-known leakers) who attempted to bridge the discursive/operational gap and were punished for it — their exclusion is what the suppression maintains.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo vanished overnight — officials spoke plainly about operational winnability plans, classification barriers to congressional oversight were lifted, declaratory policy matched operational reality — the nuclear enterprise would lose its primary shield against democratic accountability. Congress would demand plan reviews, budgets for counterforce systems would face scrutiny, arms control negotiations would shift from declaratory postures to operational capabilities, and the public would confront the gap between 'unthinkable' and 'planned.' The world would rearrange.
% FOUNDING_PROBLEM: Preventing nuclear use by establishing mutual vulnerability as the basis of deterrence stability, which required removing 'winnability' rhetoric from public discourse to prevent destabilizing arms races and preemptive war incentives (1945-1960s).
% FOUNDING_PROBLEM_CORROBORATION: The nuclear enterprise attests the problem is live: nuclear weapons exist, deterrence stability requires mutual vulnerability acknowledgment, and 'winnability' rhetoric remains destabilizing (2023 Posture Review, STRATCOM testimony). Arms control advocates and historians (Ellsberg 2017, Burr & Kimball 2022, Podvig 2020) attest the founding problem is substantially solved for declaratory purposes but the operational planning never accepted the taboo — the constraint persists as institutional protection, not deterrence necessity. No corroborating source outside the beneficiary set endorses the current dual-layer structure as necessary for deterrence.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the taboo extracts democratic oversight from nuclear planning decisions while preserving the planners' operational freedom. The extraction has accumulated over eight decades as classification regimes expanded and public discourse narrowed. Suppression (0.72) is high because the rhetorical taboo is actively maintained: officials who discuss winnability plainly face career penalties, classification systems prevent operational plans from public scrutiny, and think-tank discourse is channeled through approved frameworks. Theater ratio (0.55) is elevated because a growing share of deterrence discourse performs the 'unthinkability' ritual while operational planning proceeds on different assumptions — the gap between declaratory policy and operational reality has widened. Accessibility collapse (0.42) is moderate: alternatives (no-first-use, sole purpose declarations, disarmament pathways) exist discursively but are structurally marginalized by the taboo. Resistance (0.38) is present but fragmented: arms control advocates, some legislators, and whistleblowers resist, but the constraint's bipartisan/institutional character limits effective challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the strategic planner seat, the constraint appears as necessary coordination: the taboo prevents destabilizing rhetoric, reassures allies, and creates space for stable deterrence. From the democratic oversight seat, the same constraint appears as extraction: a classification regime that removes existential decisions from constitutional accountability. The engine computes this divergence from the declared beneficiaries/victims and their exit options — planners have institutional cover (identity_locked within the enterprise), oversight has only electoral cycles (constrained exit).
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and nuclear enterprise institutions are the primary beneficiaries (d near 0.0): they gain operational flexibility, budget protection, and institutional autonomy while avoiding public accountability. Democratic oversight, civil society debate, and arms control diplomacy are the primary victims (d near 1.0): they bear the cost of decisions made without their input, face existential risks from plans they cannot scrutinize, and negotiate treaties based on declaratory fictions. The directionality derives from the structural fact that the taboo shields planners from oversight while imposing ignorance on the public — not from any metric tuning.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear use through mutual vulnerability acknowledgment) remains live — nuclear weapons exist and deterrence stability is still required. However, the operational persistence of winnability planning behind the rhetorical taboo has outlived its coordination justification. The mandate has atrophied: the taboo no longer primarily serves deterrence stability but protects institutional prerogatives. This is not a scaffold (no sunset) nor a piton (active enforcement, concentrated beneficiaries). It is a tangled rope whose coordination function is real but whose extraction has become the dominant structural feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetorical_operational_gap_causality,
    'Does the rhetorical taboo *cause* the operational persistence (by shielding it from challenge), or does operational persistence *cause* the rhetorical taboo (as cover), or are they co-produced by a third structure (institutional inertia)?',
    'Counterfactual analysis: if the taboo were lifted (e.g., by a major leak or policy shift), would operational planning change, or would it persist unchanged? Historical tracing: which layer moved first at key inflection points (1960s flexible response, 1970s counterforce, 1990s post-Cold War, 2010s modernization)?',
    'If taboo causes persistence, lifting it reduces extraction. If persistence causes taboo, lifting it only exposes extraction without changing it. If co-produced, both must be addressed simultaneously. This determines whether the constraint is primarily extractive (snare-tending) or coordinative (rope-tending).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rhetorical_operational_gap_causality, conceptual, 'Causal direction between the constraint''s two layers.').

omega_variable(
    taboo_enforcement_mechanism,
    'Is the rhetorical taboo maintained primarily by formal classification/secrecy law, by informal professional norms, by career incentive structures, or by the self-censorship of the analytical community?',
    'Compare enforcement episodes: officials sanctioned for discussing winnability (formal), analysts denied access for asking (informal), career trajectories of those who challenge vs. conform (incentive), think-tank funding patterns (self-censorship).',
    'Formal enforcement implies the constraint is a legal/political artifact (more amenable to legislative remedy). Informal/self-censorship enforcement implies it is a cultural/epistemic artifact (harder to dislodge, more piton-like). The mix determines fixability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_enforcement_mechanism, empirical, 'Mechanism sustaining the rhetorical taboo.').

omega_variable(
    kernel_reading_ambiguity,
    'Is the ''war_winnability_post_1945'' kernel a single contested concept with three readings, or are these three distinct kernels that share vocabulary?',
    'Test ε-invariance: do the three readings have stable, distinct ε values when measured against their own referents? If deterrence_unthinkable has ε≈0.1, countervailing_thinkable has ε≈0.4, and rhetorical_contraction has ε≈0.68, they are distinct constraints. If measurement method changes ε within a reading, that reading needs further decomposition.',
    'If distinct kernels, the family linkage via affects_constraints is structural. If single kernel, the readings are observer-frames on one constraint — the current decomposition would be an authoring artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel decomposition reflects structural reality or authoring convenience.').

omega_variable(
    democratic_oversight_coalition_potential,
    'Could democratic oversight, civil society, and arms control diplomacy form a coalition with sufficient power to challenge the taboo, or are their exit options too constrained and their power too diffuse?',
    'Analyze historical moments of oversight assertion (Church Committee, Iran-Contra, post-Cold War drawdown, 2017 nuclear posture review hearings): did coalitions form? What was their structural leverage? Model the power distribution across the stakeholder seats.',
    'If coalition potential exists, the constraint''s persistence depends on active suppression (tangled_rope/snare). If structurally impossible, the constraint is more piton-like — inertia without active defense. Affects classification and mandatrophy assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_oversight_coalition_potential, empirical, 'Whether victim seats can structurally coordinate against the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t1955, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1955, 0.22).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t1965, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1965, 0.32).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t1975, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1975, 0.41).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t1985, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1985, 0.48).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t1995, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1995, 0.51).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t2005, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2005, 0.53).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t2015, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2015, 0.54).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_tr_t2025, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t1955, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1955, 0.35).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t1965, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t1975, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t1985, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t1995, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t2005, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t2015, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_be_t2025, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t1955, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1955, 0.42).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t1965, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1965, 0.55).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t1975, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t1985, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t1995, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t2005, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2005, 0.71).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t2015, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(war_winnability_post_1945__rhetorical_contraction_su_t2025, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, nuclear_deterrence_declaratory_policy).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, arms_control_verification_regimes).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, nuclear_command_control_secrecy).

% DUAL FORMULATION NOTE:
% This constraint decomposes the 'nuclear winnability' kernel into three structurally distinct claims. The deterrence_unthinkable reading (near-zero extraction, mountain-like) and countervailing_thinkable reading (moderate extraction, rope-like) have different ε values, different stakeholder structures, and different operational referents. They are linked through this constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, institutional, 0.15).
constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
