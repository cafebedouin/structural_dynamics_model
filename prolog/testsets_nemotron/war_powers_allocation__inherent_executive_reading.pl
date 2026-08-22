% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive Authority to Deploy Force Without Prior Congressional Authorization
 *   domain: constitutional/law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint story instantiates the inherent_executive_reading of the
 *   war_powers_allocation kernel. It claims that Article II's
 *   Commander-in-Chief clause grants the president inherent authority to
 *   deploy military force in defense of national interests without prior
 *   congressional authorization — congressional approval becomes courtesy,
 *   not requirement. The constraint's coordination function is executive
 *   agility in crisis; its transfer function moves war-initiation authority
 *   from the legislative to the executive branch, with post-hoc
 *   appropriations serving as ratification mechanism. The claimed type is
 *   tangled_rope: genuine coordination (rapid response capability) coexists
 *   with asymmetric extraction (Congress loses its constitutional gatekeeping
 *   role, public accountability is degraded). Active enforcement is required
 *   — the executive branch actively resists statutory constraints like the
 *   War Powers Resolution through signing statements, OLC opinions, and
 *   operational practice. This reading does not describe the same constraint
 *   as congressional_primacy_reading or functional_accommodation_reading; per
 *   ε-invariance, each reading has a distinct ε, distinct beneficiary/victim
 *   structure, and distinct classification.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary beneficiary (institutional/arbitrage) — gains unilateral deployment authority
 *   - national_security_establishment: Secondary beneficiary (institutional/arbitrage) — gains operational freedom and bureaucratic autonomy
 *   - congress_legislative_authority: Primary victim (institutional/constrained) — loses constitutional gatekeeping function
 *   - public_accountability_mechanisms: Secondary victim (organized/constrained) — loses transparent authorization record
 *   - judiciary: Observer (institutional/analytical) — invoked sporadically but avoids merits via political question doctrine
 *   - international_actors: Excluded (powerful/trapped) — affected by deployments but no structural voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.28).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.35).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive Authority to Deploy Force Without Prior Congressional Authorization").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional/law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '77841169-963f-487c-b221-9e722ad8f491').
narrative_ontology:cs_kernel_codification('77841169-963f-487c-b221-9e722ad8f491', fixed_text).
narrative_ontology:cs_authority_grounding('77841169-963f-487c-b221-9e722ad8f491', lineage).
narrative_ontology:cs_interpretation_layer_present('77841169-963f-487c-b221-9e722ad8f491').
narrative_ontology:cs_reading_relation('77841169-963f-487c-b221-9e722ad8f491', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('77841169-963f-487c-b221-9e722ad8f491', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('77841169-963f-487c-b221-9e722ad8f491', foundational, commander_in_chief_inherent_deployment_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_inherent_deployment_authority, holdable).
narrative_ontology:cs_axiom_grounding('77841169-963f-487c-b221-9e722ad8f491', commander_in_chief_inherent_deployment_authority, conventional).
narrative_ontology:cs_axiom('77841169-963f-487c-b221-9e722ad8f491', foundational, congressional_authorization_as_courtesy_not_requirement).
narrative_ontology:cs_axiom_status(congressional_authorization_as_courtesy_not_requirement, holdable).
narrative_ontology:cs_axiom_grounding('77841169-963f-487c-b221-9e722ad8f491', congressional_authorization_as_courtesy_not_requirement, conventional).
narrative_ontology:cs_axiom('77841169-963f-487c-b221-9e722ad8f491', secondary, appropriations_as_ratification_mechanism).
narrative_ontology:cs_axiom_status(appropriations_as_ratification_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('77841169-963f-487c-b221-9e722ad8f491', appropriations_as_ratification_mechanism, conventional).
narrative_ontology:cs_reference_frame('77841169-963f-487c-b221-9e722ad8f491', founding_era_executive_agility).
narrative_ontology:cs_drift_state('77841169-963f-487c-b221-9e722ad8f491', post_9_11_permanent_authorization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77841169-963f-487c-b221-9e722ad8f491', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_establishment).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress_legislative_authority).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, public_accountability_mechanisms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims inherent authority to deploy force globally without prior congressional approval. Collects the operational freedom to initiate military action on its own timeline and terms. Uses OLC opinions, signing statements, and practice to maintain the reading. Can shift between inherent_executive and functional_accommodation framings as politically convenient.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary,
    institutional, generational, arbitrage, global).

% The military-intelligence bureaucracy gains operational autonomy and reduced congressional oversight. Its planning and procurement cycles assume executive discretion. It advocates for the reading through institutional channels and leaks. Exit is arbitrage — it serves whichever reading the executive adopts.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Constitutionally designated to declare war and regulate captures, but structurally excluded from initiation decisions. Retains power of the purse but faces political impossibility of defunding deployed forces. Can pass War Powers Resolutions but lacks enforcement mechanism. Exit is constrained — institutionally cannot abandon war powers but politically cannot exercise them effectively.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress_legislative_authority, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, congress_legislative_authority, agenda_setter).

% The apparatus of democratic war control: public debate, authorization votes, casualty reporting, congressional hearings. These mechanisms are degraded when deployments bypass authorization. They persist formally but operate on a delayed, reactive basis. Exit is constrained — the mechanisms exist but their trigger (prior authorization) is removed.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, public_accountability_mechanisms, payer,
    organized, generational, constrained, national).

% Invoked by all sides but consistently avoids merits via political question doctrine, standing barriers, and equitable discretion. Provides no structural check on the constraint's operation. Its analytical seat sees the full structure but its institutional role prevents intervention.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Foreign governments and populations affected by U.S. military deployments have no structural voice in the authorization process. They bear the consequences of unilateral executive decisions but cannot access the constraint's decision mechanism. Their exclusion is structural — the constraint operates entirely within U.S. constitutional architecture.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, international_actors, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid military response to threats without the delay of congressional deliberation and voting — the genuine coordination problem of executive agility in crisis.
% TRANSFER_FUNCTION: Moves war-initiation authority from Congress (constitutional gatekeeper) to the Executive (operational decision-maker), with post-hoc appropriations serving as ratification mechanism that legitimizes but does not constrain.
% ABSENT_VOICES: Foreign populations affected by deployments, future generations bearing blowback costs, and the constitutional text itself (which is read differently by each reading). Congress as an institution is present but its constitutional role is hollowed out — its voice is structurally present but functionally absent.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the executive would lose its claim to unilateral deployment authority. Congress would resume its gatekeeping role (either via congressional_primacy or functional_accommodation). The War Powers Resolution would become binding. Deployment decisions would require prior authorization or fit within narrow emergency exceptions. The global deployment posture would contract significantly.
% FOUNDING_PROBLEM: The constitutional design required a single executive capable of responding to sudden attacks when Congress could not convene — the 'repel sudden attacks' function identified in the Federalist and early practice.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (executive agility against sudden attack) is attested as live by the executive branch and national security establishment. It is contested by congressional_primacy and functional_accommodation proponents who argue the problem is real but does not require the *breadth* of authority this reading claims. No disinterested third party corroborates that the *specific scope* of 'defense of national interests broadly defined' was the founding problem — the historical record supports only the narrower 'repel sudden attacks' formulation.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).
:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects that the executive gains substantial authority but the constraint is not pure extraction — rapid response to genuine threats is a real coordination function. Suppression (0.35) is moderate: the War Powers Resolution exists but is treated as advisory; Congress's power of the purse is structurally intact but politically unusable once forces are deployed. Theater ratio (0.22) captures the performative nature of 'consultation' rituals and post-hoc briefings that do not constrain action. Accessibility collapse (0.38) is low-moderate: alternatives (AUMFs, declarations, functional_accommodation frameworks) remain conceptually available but are politically disfavored. Resistance (0.42) reflects sustained congressional and judicial pushback that fails to alter structural operation. The measurement series shows extraction rising through Cold War and post-9/11 peaks, then modestly declining as functional_accommodation norms reassert.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat (beneficiary, institutional power, arbitrage exit), the constraint is genuine coordination — the only structure that permits timely defense. From the congressional seat (victim, institutional power, constrained exit), the same structure is extraction — their constitutional role is hollowed out while they retain nominal authority. From the public accountability seat (victim, organized power, constrained exit), the constraint degrades democratic control over war. The judiciary (observer) sees a political question it cannot resolve. These seats compute different types: the executive seat sees rope; the congressional seat sees snare; the engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and national security establishment are beneficiaries: they collect deployment authority and operational autonomy (d near 0.1-0.2). Congress as an institution is a victim: it bears the cost of lost gatekeeping and political accountability for wars it did not authorize (d near 0.7-0.8). Public accountability mechanisms are victims: transparency and democratic deliberation are degraded (d near 0.6-0.7). The executive's exit is arbitrage (can shift between readings as politically convenient); Congress's exit is constrained (institutional role prevents full exit but political cost of confrontation is high). The 'national interests' definition is self-certifying by the executive, making the constraint's scope effectively universal for deployment decisions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — executive agility against sudden attack — remains live (nuclear age, cyber threats, non-state actors). However, the constraint has expanded far beyond its founding scope: from 'repel sudden attacks' to 'deploy force in defense of national interests broadly defined.' The mandate has atrophied in the sense that the coordination function (speed) no longer requires the extraction component (total exclusion of Congress). The appropriations-as-ratification mechanism is a Mandyatrophy artifact: it preserves the form of congressional consent while inverting its substance. The constraint persists not because the founding problem demands this specific structure, but because the executive branch benefits from the expanded authority and no structural force compels reversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the inherent executive reading a distinct constraint with its own ε, or does it merely describe a different observable of the same war powers allocation constraint?',
    'Compare extraction profiles across readings: if congressional_primacy_reading shows near-zero extractiveness and functional_accommodation_reading shows moderate, context-dependent extractiveness while this reading shows sustained low-moderate extractiveness with different beneficiary/victim structure, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own constraint story and classification; if not, the framework must model observable-dependent ε within a single constraint (which the ε-invariance principle forbids).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three war powers readings instantiate separate constraints or one constraint measured differently.').

omega_variable(
    appropriations_ratification_mechanism,
    'Does post-hoc appropriations ratification constitute genuine congressional consent or a coerced ratification that masks extraction?',
    'Track legislative behavior: if Congress consistently funds operations it did not authorize but cannot politically defund once deployed, the mechanism is extractive ratification; if Congress genuinely endorses after the fact with power to decline, it functions as delayed coordination.',
    'If ratification is coerced, the constraint''s extraction is higher and its suppression is structural (Congress trapped); if genuine, the constraint operates as functional_accommodation with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_ratification_mechanism, empirical, 'Whether appropriations-as-ratification is a coordination mechanism or extraction trap.').

omega_variable(
    national_interests_definition,
    'Who defines ''national interests'' that trigger inherent authority — the executive alone, or does the constraint require inter-branch contestation over the definition?',
    'Analyze historical cases where Congress contested the scope of ''national interests'': if the executive''s definition prevails without judicial or legislative check, the constraint is self-certifying (higher extraction); if definition is genuinely contested, the constraint has built-in resistance.',
    'Self-certifying definition makes the constraint a snare (extraction with no structural check); contested definition makes it tangled_rope (coordination with asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_interests_definition, conceptual, 'Whether ''national interests'' is an executive monopoly or a contested boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__inherent_executive_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(war__tr_t1964, war_powers_allocation__inherent_executive_reading, theater_ratio, 1964, 0.12).
narrative_ontology:measurement(war__tr_t1973, war_powers_allocation__inherent_executive_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(war__tr_t1991, war_powers_allocation__inherent_executive_reading, theater_ratio, 1991, 0.15).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__inherent_executive_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(war__tr_t2011, war_powers_allocation__inherent_executive_reading, theater_ratio, 2011, 0.23).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__inherent_executive_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(war__be_t1964, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1964, 0.18).
narrative_ontology:measurement(war__be_t1973, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1973, 0.22).
narrative_ontology:measurement(war__be_t1991, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1991, 0.24).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2001, 0.31).
narrative_ontology:measurement(war__be_t2011, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2011, 0.29).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1945, 0.15).
narrative_ontology:measurement(war__su_t1964, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1964, 0.25).
narrative_ontology:measurement(war__su_t1973, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1973, 0.35).
narrative_ontology:measurement(war__su_t1991, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(war__su_t2011, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2011, 0.38).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__inherent_executive_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_resolution_1973).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, aumf_2001).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, aumf_2002).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel decomposes into three constraint stories with distinct ε and structural profiles. This reading (inherent_executive) claims the widest executive authority and shows the highest sustained extraction. The congressional_primacy_reading claims the narrowest executive authority and near-zero extraction. The functional_accommodation_reading sits between with context-dependent extraction. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, institutional, 0.15).
constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
