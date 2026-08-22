% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Norm Climb: Bottom-Up Adoption Preceding State Mandate
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous-climb reading of the
 *   imposition_mechanism_kernel: a contest over whether new norms achieve
 *   legitimacy through grassroots adoption that the state later formalizes,
 *   or through coercive state imposition that communities gradually
 *   internalize. Under this reading, communities independently discover and
 *   adopt norms that solve coordination problems arising from changed
 *   material conditions (technology, trade, demographic shifts, institutional
 *   evolution). The state, recognizing that a norm has already achieved
 *   widespread cultural acceptance, formalizes it into law or official
 *   mandate — acting as a coordinator and legitimacy certifier rather than an
 *   enforcer. Extraction and enforcement costs are minimal because legitimacy
 *   precedes mandate. This reading asserts that the causation runs from
 *   community to state, not the reverse. The constraint itself is the state's
 *   formalization; the coordination problem solving happens endogenously.
 *
 * KEY AGENTS:
 *   - coordinating_communities: Solve the coordination problem through peer modeling and internal incentive alignment; adopt the norm voluntarily because it solves a real problem
 *   - state_apparatus: Recognizes endogenous adoption and formalizes it; gains governance efficiency and legitimacy by appearing responsive rather than commanding
 *   - norm_pioneers: Early adopters whose example catalyzes broader diffusion; carry social risk in adoption but gain status from leading the coordination
 *   - holdout_populations: Resist adoption despite social and later legal pressure; incur sanctions and mandatory compliance costs; their resistance is treated as deviance
 *   - rival_institutional_authorities: Alternative framings and power centers that might have shaped the norm are retrospectively excluded once the state certifies the adopted norm
 *   - historical_observer: Examines whether the causal claim of endogenous climb is supported by evidence or whether state narrative has constructed the appearance of grassroots adoption
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Norm Climb: Bottom-Up Adoption Preceding State Mandate").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, 'b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799').
narrative_ontology:cs_kernel_codification('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', formalized).
narrative_ontology:cs_authority_grounding('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', lineage).
narrative_ontology:cs_interpretation_layer_present('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799').
narrative_ontology:cs_reading_relation('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', foundational, community_problem_solving_precedes_state_mandate).
narrative_ontology:cs_axiom_status(community_problem_solving_precedes_state_mandate, holdable).
narrative_ontology:cs_axiom_grounding('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', community_problem_solving_precedes_state_mandate, empirically_contingent).
narrative_ontology:cs_axiom('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', foundational, legitimacy_flows_from_endogenous_coordination).
narrative_ontology:cs_axiom_status(legitimacy_flows_from_endogenous_coordination, holdable).
narrative_ontology:cs_axiom_grounding('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', legitimacy_flows_from_endogenous_coordination, conventional).
narrative_ontology:cs_axiom('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', secondary, state_acts_as_coordinator_not_coercer).
narrative_ontology:cs_axiom_status(state_acts_as_coordinator_not_coercer, holdable).
narrative_ontology:cs_axiom_grounding('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', state_acts_as_coordinator_not_coercer, instrumental).
narrative_ontology:cs_reference_frame('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', endogenous_norm_adoption_prior_to_state_formalization).
narrative_ontology:cs_drift_state('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', contemporary_historical_scholarship, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b26b8d60-9b8c-463a-a5ba-9cbf5e6c6799', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, coordinating_communities).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, state_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_pioneers).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, holdout_populations).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, cultural_legitimacy_precedes_coercion).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, grassroots_coordination_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities adopt and propagate new norms through internal coordination and peer-to-peer modeling. They benefit from coordination equilibrium (everyone following the same norm reduces transaction costs and social friction). Their adoption is genuinely voluntary — exit is available but costly; they remain because the norm solves a real coordination problem they face.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, coordinating_communities, beneficiary,
    organized, generational, mobile, national).

% Recognizes that a norm has achieved widespread endogenous adoption and formalizes it into law or mandate after the cultural fact. Acts as coordinator and certifier rather than enforcer. Benefits from governance efficiency (legitimacy already established), reduced compliance costs, and institutional authority that is seen as responsive to popular will rather than imposed.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, state_apparatus, beneficiary).

% Early adopters who pioneer the new norm within their communities, often motivated by solving local problems or responding to new conditions. Carry social risk in early adoption but gain status and coordination advantage once the norm spreads. Their example and advocacy catalyze broader adoption.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_pioneers, beneficiary,
    moderate, biographical, mobile, regional).

% Communities or individuals who do not voluntarily adopt the norm despite endogenous pressure. Face social sanctions and eventual state mandate enforcement, though enforcement costs are low because the norm is already culturally legitimated. Their resistance is treated as deviance rather than justified disagreement.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, holdout_populations, payer,
    powerless, biographical, constrained, local).

% Alternative authority structures (religious institutions, traditional guilds, regional powers) that might have competed to define the norm are superseded by the state's formalization. Their voice in the norm-setting process is excluded retrospectively — the state certifies a norm that communities have already adopted, foreclosing alternative institutional framings.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, rival_institutional_authorities, excluded,
    institutional, generational, trapped, national).

% Examines the constraint from outside the historical moment: whether the causal story is actually endogenous climb (communities precede state) or whether state messaging and subtle coercion shaped the appearance of grassroots adoption. Asks whether the norm's 'legitimacy' was genuinely bottom-up or manufactured through elite narrative.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__endogenous_climb_reading, state_apparatus).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__endogenous_climb_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Communities face a new practical problem (economic, social, technological, or institutional) that an emerging norm solves efficiently. Endogenous adoption coordinates behavior across dispersed actors without centralized enforcement; the norm equilibrates through peer modeling and social reward, not legal sanction.
% TRANSFER_FUNCTION: Does not principally move resources from one group to another. The transfer is of legitimacy and institutional authority from community adoption to state formalization: the state gains governance efficiency and political capital by appearing to codify popular will. Holdout populations may incur social costs (sanctions, mandatory compliance) that flow toward the broader community as enforcement of norm conformity.
% ABSENT_VOICES: Rival institutional authorities (traditional power centers, religious institutions, regional hegemonies) that might have offered alternative norm framings are excluded from the retrospective norm-setting process. Their objections are treated as resistance to an already-settled cultural fact rather than as legitimate competing definitions. Communities that arrived at different solutions to the coordination problem are outside the conversation once the state selects one solution to mandate.
% DISAPPEARANCE_RATIONALE: If the endogenous norm climb and its state formalization disappeared, communities would either revert to the coordination problem the norm solved, or would continue following the norm informally (since the legitimacy precedes the mandate). The constraint itself is the *formalization*, not the coordination; without it, the norm might persist socially but lose official sanction and enforcement backing.
% FOUNDING_PROBLEM: New material or institutional conditions create a coordination problem: a technology emerges, trade patterns shift, religious contexts change, or legal structures evolve. Existing norms are inadequate. Communities independently discover and experiment with new norms that solve the problem. After widespread adoption, state recognizes and formalizes the norm.
% FOUNDING_PROBLEM_CORROBORATION: This reading attributes the founding problem to endogenous conditions and bottom-up problem-solving. Historical sources from within the communities (chronicles, letters, guild records, merchant accounts) that show norm experimentation and adoption preceding state mandates support this reading. Comparative historical evidence from other similar transitions (craft standards, commercial practices, hygiene norms) where bottom-up adoption clearly preceded mandates corroborates the mechanism. Rival readings (exogenous_override, hybrid_legitimation) cite state chroniclers and elite narratives that assert top-down causation; the corroboration question is exactly where the historical evidence diverges most sharply.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.18 (final interval value) because the endogenous-climb reading claims low extraction: the norm solves a real coordination problem, adoption is voluntary and motivated by problem-solving, and the state's role is certification rather than coercion. However, residual extraction remains (not zero) because: (1) the state gains institutional authority and governance efficiency by formalizing the norm, (2) holdout populations incur costs through mandatory compliance and social sanctions, and (3) alternative institutional authorities are excluded from the retrospective norm-setting process. Suppression is low (0.12) because the norm is already culturally legitimated before state mandate; enforcement machinery addresses only holdouts, not the broader population. Theater ratio is minimal (0.08) because the state's formalization is largely genuine coordination and certification; the performance component is limited to the theater of responsiveness (appearing to follow rather than lead). Accessibility_collapse is moderate-high (0.65) because once communities adopt the norm, alternative coordination solutions become less accessible — network effects lock populations into the equilibrium — but it is not maximal because mobility and exit remain available to holdout populations until state mandate closes those options. Resistance is low (0.15) because the reading asserts that the norm solves a genuine problem communities recognize; resistance comes mainly from holdouts who benefit from the old arrangement or who have incompatible worldviews, not from the broader population that has already adopted. Measurement series show shallow rise from t0 to t40 (endogenous adoption phase where extraction and suppression are minimal), then flattening from t40 onward (post-formalization phase where mandatory compliance increases suppression slightly but extraction stays low because legitimacy is established). Theater rises gradually as the state celebrates its role in norm stewardship but remains low throughout.
 *
 * PERSPECTIVAL GAP:
 *   From the community seat, the constraint is genuine coordination: a problem is solved, adoption is voluntary, and legitimacy is earned through social proof. From the state seat, the constraint offers institutional authority gain and governance efficiency without the cost of enforcement — a secondary benefit, not primary rent collection, but still an asymmetric gain. From the historical observer seat (analytical), the key question is whether the historical record supports the causal claim: did communities actually adopt independently, or did state messaging and subtle incentives shape the appearance of endogenous adoption? The engine should compute the community seat as rope (coordination, low extraction) and the state seat as tangled_rope (coordination + secondary extraction benefit), with the pivotal difference being whether the historical evidence supports prior community adoption. If rival readings (exogenous_override or hybrid_legitimation) can point to state mandates that temporally preceded documented community adoption, the observer seat might recompute the entire constraint as snare or tangled_rope (exogenous imposition masked as endogenous climb).
 *
 * DIRECTIONALITY LOGIC:
 *   Coordinating communities sit near the beneficiary end (d near 0.1–0.2) because they solve their own coordination problem and voluntarily adopt; the norm exit is costly but available. The state sits in a complex middle position: it benefits from governance efficiency and authority, but it is not extracting rents from unwilling parties at scale — the extraction is from holdout populations only. State directionality should compute around 0.35–0.45 (moderate asymmetry: coordinator role with secondary extraction benefit). Holdout populations sit near the target end (d near 0.85) because they do not voluntarily adopt but are subjected to mandatory compliance and social sanctions. Norm pioneers sit at the beneficiary end (d near 0.0–0.1) because they gain coordination benefit and social status. The reading's structural claim is that directionality divergence between communities and state should be shallow (both are coordinating, though the state captures secondary benefits) — this is the signal that distinguishes the endogenous-climb reading from exogenous_override (where state-to-target directionality would be very steep, near 1.0) and from hybrid_legitimation (where symbolic authority transfer would add a secondary extraction layer increasing state d above 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (new coordination problem from changed material conditions) is live, not dead — it persists throughout the interval. The state mandate is genuinely responsive to that problem (communities have already solved it endogenously), not a zombie of a solved problem. Mandatrophy is not present under this reading because the foundational coordination function remains active. However, the reading is vulnerable to the mandatrophy critique from the rival readings: if historical evidence shows that communities did NOT independently adopt the norm but rather state coercion preceded mass adoption, then the constraint is a Piton (formalization without foundational coordination), and mandatrophy is present (a fictional story of endogenous climb covering up exogenous imposition). The omega variables below address this exact contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_manufactured_adoption,
    'Did communities genuinely adopt the norm endogenously through independent problem-solving, or did the state''s messaging, incentives, and subtle coercion create the appearance of grassroots adoption?',
    'Archival evidence from community sources (guild records, merchant accounts, folk practices, oral tradition) predating state mandates; comparative timing analysis of community adoption vs. state formalization across multiple cases; ethnographic or anthropological evidence of community problem-solving preceding state intervention; control cases where communities rejected state-promoted norms despite pressure.',
    'If genuinely endogenous, the constraint is rope (coordination) with minimal extraction. If manufactured, the constraint reclassifies to snare (extraction masked as coordination). This is the pivotal axis for the reading: high confidence in endogenous adoption is the core claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_vs_manufactured_adoption, empirical, 'Whether documented norm adoption is causally prior to state mandate and independent of state pressure.').

omega_variable(
    causation_direction_kernel_contest,
    'At the kernel level: does the evidence support endogenous climb (community → state) or exogenous override (state → community) or hybrid (state + community co-produce)?',
    'Historical analysis of norm diffusion patterns, timing of state formalization relative to community adoption, state propaganda vs. community documentation, counterfactual: would the norm have spread without state mandate? Comparative cases across different types of norms (hygiene, craft standards, religious practice, commercial law) to identify patterns.',
    'The kernel contest is exactly this: three constraints, three readings, one incompatible set of causal claims. This omega documents that the three readings are not three perspectives on one reality but rather three competing factual claims about causation. One or more readings will be reclassified as misdiagnosed if evidence decisively establishes causation in one direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_direction_kernel_contest, empirical, 'The kernel-level contest over causal direction of norm adoption: bottom-up vs. top-down vs. co-produced.').

omega_variable(
    legitimacy_versus_compliance,
    'Is the state mandate''s enforcement cost low because the norm is genuinely legitimate (community acceptance precedes mandate) or because the norm is internalizable (communities accept the *outcome* even if imposed)?',
    'Behavioral evidence post-mandate: holdout resistance, compliance motivation (choice vs. fear), norm transmission to children and new populations (internalization), voluntary adoption in contexts where mandate cannot reach, cost of enforcement over time (does suppression_requirement stay low or increase as alternative pressure builds?).',
    'This omega addresses the extraction/suppression ambiguity: low suppression could indicate genuine legitimacy or could indicate successful internalization of an imposed norm. If communities accept the norm as legitimate (endogenous climb), suppression should stay low indefinitely. If communities accept it because they internalize external coercion, suppression_requirement may increase over time as the constraint loses performative authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_versus_compliance, empirical, 'Whether low enforcement costs reflect genuine legitimacy or successful normative internalization of external imposition.').

omega_variable(
    state_role_classification,
    'Is the state''s role in norm formalization coordinator (ratifying and certifying endogenous adoption) or architect (selecting among possible solutions or imposing one solution)?',
    'Historical analysis of state deliberation: did state actors debate norm alternatives or simply formalize what communities had already settled on? Did the state formalize the most-adopted norm or a competing option that gained state backing over others? Did the state create incentive structures that shaped adoption, or did it respond to adoption already in progress?',
    'If the state is genuinely a coordinator, the constraint is rope and the state seat directionality should be low-to-moderate (0.3–0.4). If the state is an architect selecting among options, the constraint slides toward tangled_rope and state directionality rises (0.45–0.6). This omega distinguishes the endogenous-climb reading from the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_role_classification, empirical, 'Whether the state acts as a coordinator ratifying endogenous adoption or as an architect selecting and incentivizing particular norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(impo_tr_t0, projected).
narrative_ontology:measurement(impo_tr_t20, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 20, 0.04).
narrative_ontology:measurement_basis(impo_tr_t20, projected).
narrative_ontology:measurement(impo_tr_t40, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement_basis(impo_tr_t40, observed).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(impo_tr_t60, observed).
narrative_ontology:measurement(impo_tr_t80, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 80, 0.08).
narrative_ontology:measurement_basis(impo_tr_t80, observed).
narrative_ontology:measurement(impo_tr_t100, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement_basis(impo_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(impo_be_t0, projected).
narrative_ontology:measurement(impo_be_t20, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(impo_be_t20, projected).
narrative_ontology:measurement(impo_be_t40, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(impo_be_t40, observed).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement_basis(impo_be_t60, observed).
narrative_ontology:measurement(impo_be_t80, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 80, 0.19).
narrative_ontology:measurement_basis(impo_be_t80, observed).
narrative_ontology:measurement(impo_be_t100, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(impo_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement_basis(impo_su_t0, projected).
narrative_ontology:measurement(impo_su_t20, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement_basis(impo_su_t20, projected).
narrative_ontology:measurement(impo_su_t40, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement_basis(impo_su_t40, observed).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement_basis(impo_su_t60, observed).
narrative_ontology:measurement(impo_su_t80, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 80, 0.12).
narrative_ontology:measurement_basis(impo_su_t80, observed).
narrative_ontology:measurement(impo_su_t100, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 100, 0.12).
narrative_ontology:measurement_basis(impo_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imposition_mechanism_kernel, a foundational kernel in historical sociology contested by three incompatible constraint stories: endogenous_climb_reading (bottom-up adoption precedes state mandate), exogenous_override_reading (state coercion precedes community adoption), and hybrid_legitimation_reading (state symbolic authority + community agency co-produce norm adoption). All three stories share the kernel (a norm is formed and spreads) and share the stakeholders but diverge sharply on causal direction, legitimacy grounding, and the state's structural role. The three constraints have overlapping but distinct ε values, different suppression profiles, and different predicted directionalities for the state seat. The reading_relations declare coexistence (all three remain live in the historical dispute) and mutual influence (evidence favoring one reading creates pressure on the others). The axioms name the foundational normative/empirical claims each reading holds. Historical evidence will eventually support one reading's causal claim over the others; until then, all three remain in the constraint corpus as competing instantiations of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__endogenous_climb_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
