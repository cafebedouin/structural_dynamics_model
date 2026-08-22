% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation via Imperial Example and Institutional Incentives
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the hybrid legitimation mechanism where a new norm
 *   achieves empire-wide adoption through the emperor's symbolic example
 *   (visible, ritualized adoption) combined with a structured incentive
 *   system (examination reform, office qualification, tax privileges, status
 *   markers). It is neither pure endogenous climb (bottom-up diffusion) nor
 *   pure exogenous override (coercive imposition). The mechanism operates in
 *   historical cases like the Tang adoption of Buddhism-inflected rites, the
 *   Song neo-Confucian orthodoxy, the Meiji Emperor's Western dress and
 *   calendar reform, and Atatürk's script and hat laws. The structural delta:
 *   moderate enforcement costs (lower than pure override), stratified
 *   adoption (elites first, masses generations later), legitimacy derived
 *   from imperial charisma rather than coercion or grassroots demand.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.38).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.28).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation via Imperial Example and Institutional Incentives").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, 'f7a5db68-54d2-433c-8d6b-048ad3c10ee9').
narrative_ontology:cs_kernel_codification('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', distributed).
narrative_ontology:cs_authority_grounding('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', lineage).
narrative_ontology:cs_interpretation_layer_present('f7a5db68-54d2-433c-8d6b-048ad3c10ee9').
narrative_ontology:cs_reading_relation('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', foundational, imperial_example_is_primary_legitimacy_source).
narrative_ontology:cs_axiom_status(imperial_example_is_primary_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', imperial_example_is_primary_legitimacy_source, conventional).
narrative_ontology:cs_axiom('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', foundational, institutional_incentives_are_necessary_for_rapid_alignment).
narrative_ontology:cs_axiom_status(institutional_incentives_are_necessary_for_rapid_alignment, holdable).
narrative_ontology:cs_axiom_grounding('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', institutional_incentives_are_necessary_for_rapid_alignment, instrumental).
narrative_ontology:cs_axiom('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', secondary, stratified_adoption_is_functional_not_incidental).
narrative_ontology:cs_axiom_status(stratified_adoption_is_functional_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', stratified_adoption_is_functional_not_incidental, instrumental).
narrative_ontology:cs_reference_frame('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', imperial_charismatic_legitimacy_cascade).
narrative_ontology:cs_drift_state('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', post_dynastic_collapse, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('f7a5db68-54d2-433c-8d6b-048ad3c10ee9', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, bureaucratic_elite).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, state_orthodoxy_institutions).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, regional_elites_resistant_to_change).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, traditional_cultural_practitioners).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, commoner_populations_bearing_adoption_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, bureaucratic_elite).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_example_as_legitimacy_source).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, institutional_incentives_accelerate_norm_adoption).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__hybrid_legitimation_reading, stratified_adoption_patterns_are_functional).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emperor and immediate court set the normative agenda through symbolic example — visible adoption of new rites, scripts, dress, or rituals — while controlling the institutional incentive structure (office access, tax relief, status markers) that rewards emulation. The court does not bear adoption costs; it defines what counts as legitimate and collects political capital from the resulting coordination.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, continental).

% High-ranking officials adopt the new norms first to signal loyalty and secure advancement. They gain status and career trajectory but must invest in re-education, ritual performance, and public display. Their exit is constrained: refusal means career termination, but the cost of adoption is offset by the rewards of compliance.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, bureaucratic_elite, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, bureaucratic_elite, payer).

% Academies, ritual boards, and examination systems codify the new norms into curricula and certification. They gain institutional relevance, funding, and expanded jurisdiction. Their exit is mobile — they could pivot to other orthodoxies — but the imperial mandate makes alignment the lowest-friction path.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, state_orthodoxy_institutions, beneficiary,
    organized, generational, mobile, continental).

% Local aristocrats and power-holders face a dilemma: adopt the imperial norms and lose distinctiveness/traditional authority, or resist and lose access to central patronage. They bear the cost of cultural displacement and the risk of alienating local constituencies. Exit is constrained — they cannot easily leave their regional base, and resistance carries escalating penalties.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, regional_elites_resistant_to_change, payer,
    powerful, biographical, constrained, regional).

% Craftspeople, ritual specialists, and keepers of oral traditions find their practices devalued or proscribed by the new orthodoxy. Their identity is fused to the displaced norms; exit means abandoning their self-concept and community recognition. They bear extraction without meaningful compensation, and their resistance is framed as backwardness.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, traditional_cultural_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Peasants, urban artisans, and laborers adopt new naming conventions, dress codes, or ritual calendars because local enforcers (village heads, tax collectors) demand it. They bear time, material, and opportunity costs with no status gain. Exit is effectively trapped — geographic mobility is low, and non-compliance invites corporal or fiscal punishment.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, commoner_populations_bearing_adoption_costs, payer,
    powerless, immediate, trapped, local).

% Observes the full structural pattern: symbolic authority transfer from the emperor creates a legitimacy cascade; institutional incentives convert that legitimacy into adoption; the hybrid mechanism is neither pure grassroots diffusion nor pure coercive imposition. The analyst sees how the same constraint reads differently from each seat.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, comparative_historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of rapid, empire-wide normative alignment without continuous military occupation: the emperor's example provides a Schelling point for coordination, and institutional incentives lower the transaction cost of adoption across diverse populations.
% TRANSFER_FUNCTION: Moves legitimacy capital from the imperial center to adopting elites (status, office, tax relief) and moves adoption costs (re-education, ritual performance, cultural displacement) onto regional elites, traditional practitioners, and commoners. The net transfer is legitimacy downward, cost downward, with the center capturing coordination rents.
% ABSENT_VOICES: Indigenous cultural communities at the empire's margins who were never offered the incentive structure — only the demand. They would object to the erasure of their normative worlds but were structurally excluded from the legitimation cascade. Also absent: the dead — ancestors whose rites are disrupted by the new norms, invoked by traditional practitioners but unrepresented in any seat.
% DISAPPEARANCE_RATIONALE: If the hybrid mechanism vanished overnight, the normative alignment would fracture: regional elites would revert to local traditions, bureaucratic elites would lose their coordination signal, commoners would stop performing the new rites, and the imperial center would lose its primary non-coercive integration tool. The empire would either escalate to pure coercion (exogenous override) or fragment into normative pluralism.
% FOUNDING_PROBLEM: How to integrate a culturally heterogeneous empire under a single legitimate order without the fiscal and military burden of permanent garrison occupation — and without waiting generations for organic diffusion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (integration of a specific historical empire) is dead — that empire dissolved centuries ago. The mechanism persists as a template cited in later state-formation episodes (e.g., Meiji restoration, Atatürk reforms, post-colonial nation-building). Corroboration comes from comparative historical sociology (Eisenstadt, Tilly, Scott) analyzing the template's recurrence, not from the original beneficiaries.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).
:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38) reflects real but moderate extraction: the center captures coordination rents (legitimacy, integration) while distributing some gains to adopting elites. Suppression (0.28) is present but not dominant — the mechanism relies more on incentive alignment than force, though resistance from regional elites and traditional practitioners requires targeted enforcement. Theater ratio (0.22) rises over time as the performative aspect of adoption (wearing the hat, using the script) outlives the functional integration purpose. Accessibility collapse (0.55) is moderate: alternatives (local traditions, prior norms) remain cognitively available but become socially and institutionally inaccessible. Resistance (0.42) is significant but channeled — it manifests as foot-dragging, selective adoption, and hidden transcript maintenance rather than open rebellion.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial court and bureaucratic elite seats, the constraint reads as a rope (genuine coordination solving imperial integration). From regional resistant elites and traditional practitioners, it reads as a snare (extraction of cultural autonomy). From commoners, it reads as a snare with trapped exit. The tangled_rope classification captures the hybrid: the coordination function is real (empire-wide normative alignment without garrison occupation) AND the extraction is asymmetric (costs borne by those with least voice). The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court sits at d ≈ 0.05 (full beneficiary: defines the game, collects integration rents, bears near-zero adoption cost). Bureaucratic elites sit at d ≈ 0.35 (net beneficiary but paying adoption costs — constrained exit, career-dependent). State orthodoxy institutions sit at d ≈ 0.25 (beneficiary with mobile exit). Regional resistant elites sit at d ≈ 0.7 (payer: bear cultural displacement cost, constrained exit). Traditional practitioners sit at d ≈ 0.9 (payer: identity-locked, extraction without compensation). Commoners sit at d ≈ 0.95 (payer: trapped, bear costs with no status gain). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating a specific historical empire) is dead — the empires that used this mechanism have dissolved. Yet the mechanism persists as a cited template in later state-formation episodes. This is not piton (theatrical maintenance of an atrophied function within the same institution) but rather a 'traveling template' — the constraint's structural logic migrates across historical cases. The mandatrophy is resolved at the original case level but the pattern recurs. The hybrid mechanism avoids pure extraction labeling because the coordination function (rapid alignment at lower enforcement cost) was genuinely functional for the adopting states, even as it extracted from subordinate populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural classification change if the kernel''s other readings (endogenous_climb, exogenous_override) are validated as empirically dominant in specific historical cases?',
    'Case-by-case historical triangulation: for each empire-formation episode, determine whether the hybrid mechanism, endogenous climb, or exogenous override best fits the adoption curve, enforcement record, and legitimacy discourse. The reading''s ε and type are indexed to the hybrid interpretation; if empirical work establishes a different mechanism as primary for a given case, that case maps to a different reading''s constraint story.',
    'If the hybrid mechanism is shown to be a post-hoc rationalization for what was actually exogenous override in key cases, this reading''s ε would be underestimated and its claimed_type (tangled_rope) would misrepresent the structural reality. Conversely, if endogenous climb is dominant in cases previously coded as hybrid, the coordination function is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, empirical, 'Committer-frame uncertainty: this constraint is one reading of a contested kernel; its structural parameters are reading-indexed.').

omega_variable(
    symbolic_authority_vs_coercion_boundary,
    'Where is the structural boundary between legitimacy derived from imperial charisma (symbolic authority transfer) and legitimacy derived from the threat of force (coercive backing)?',
    'Measure enforcement intensity conditional on adoption stage: if early adopters (elites) face near-zero coercion while late adopters (commoners) face high coercion, the mechanism is hybrid in a stratified sense. If coercion is uniform across strata, the symbolic component is decorative.',
    'If the symbolic authority component is decorative — i.e., the emperor''s example is merely the public face of a coercive apparatus — then the constraint''s claimed coordination function is overstated and its extractiveness is underestimated. The hybrid classification would collapse toward exogenous_override.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_authority_vs_coercion_boundary, conceptual, 'Whether the ''symbolic authority transfer'' is a genuine coordination mechanism or a legitimacy theater for coercion.').

omega_variable(
    stratified_adoption_as_design_or_emergence,
    'Is the stratified adoption pattern (elites first, masses later) a designed feature of the incentive structure or an emergent property of differential exit options?',
    'Analyze imperial edicts and institutional designs: do they explicitly sequence adoption by rank (design), or do they declare universal adoption while the incentive structure de facto stratifies (emergence)?',
    'If designed, the stratification is part of the constraint''s extraction architecture — elites are bought off first to create a legitimacy cascade. If emergent, the stratification reflects pre-existing power gradients that the constraint exploits but does not engineer. Changes the mandatrophy analysis: designed stratification implies the mechanism was always extractive toward the masses; emergent stratification implies the coordination function was genuine but differentially accessible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratified_adoption_as_design_or_emergence, conceptual, 'Whether stratified adoption is engineered extraction or differential access to a genuine coordination good.').

omega_variable(
    institutional_incentive_persistence_post_founding,
    'Do the institutional incentives (examination reform, office qualification) persist after the founding integration problem is solved, and if so, do they mutate into extraction mechanisms?',
    'Trace the incentive structure across the interval: if exam content and office criteria freeze around the founding norms while the functional integration need disappears, the incentives become extraction (rent-seeking on credential rents). If they evolve with functional needs, they remain coordination.',
    'The theater_ratio trajectory (rising from 0.08 to 0.22) suggests mutation toward extraction. If confirmed, the constraint''s late-interval classification shifts toward snare or piton for the payer seats, even as the agenda-setter seat still experiences coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_persistence_post_founding, empirical, 'Whether the incentive structure''s persistence after founding-problem death constitutes mandatrophy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(impo_tr_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(impo_tr_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(impo_tr_t45, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 45, 0.15).
narrative_ontology:measurement(impo_tr_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(impo_tr_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 75, 0.2).
narrative_ontology:measurement(impo_tr_t90, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 90, 0.21).
narrative_ontology:measurement(impo_tr_t105, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 105, 0.22).
narrative_ontology:measurement(impo_tr_t120, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 120, 0.22).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(impo_be_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(impo_be_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement(impo_be_t45, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 45, 0.31).
narrative_ontology:measurement(impo_be_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 60, 0.34).
narrative_ontology:measurement(impo_be_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 75, 0.36).
narrative_ontology:measurement(impo_be_t90, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 90, 0.37).
narrative_ontology:measurement(impo_be_t105, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 105, 0.38).
narrative_ontology:measurement(impo_be_t120, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 120, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(impo_su_t15, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(impo_su_t30, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(impo_su_t45, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 45, 0.23).
narrative_ontology:measurement(impo_su_t60, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 60, 0.25).
narrative_ontology:measurement(impo_su_t75, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 75, 0.27).
narrative_ontology:measurement(impo_su_t90, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 90, 0.28).
narrative_ontology:measurement(impo_su_t105, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 105, 0.28).
narrative_ontology:measurement(impo_su_t120, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 120, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__hybrid_legitimation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.08).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_legitimation_reading of the imposition_mechanism_kernel. The kernel decomposes into three constraint stories with distinct ε values and stakeholder structures. This reading's ε = 0.38 (moderate extraction, hybrid mechanism). The endogenous_climb_reading would have ε ≈ 0.12 (near-rope, low extraction). The exogenous_override_reading would have ε ≈ 0.65 (snare-range, high extraction). All three are linked via affects_constraints to enable contamination analysis across the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_mechanism_kernel__hybrid_legitimation_reading, powerful, 0.7).
constraint_indexing:directionality_override(imposition_mechanism_kernel__hybrid_legitimation_reading, moderate, 0.9).
constraint_indexing:directionality_override(imposition_mechanism_kernel__hybrid_legitimation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
