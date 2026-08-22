% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Winnability Rhetorical Taboo / Operational Planning Constraint (Post-1945)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   In the post-1945 nuclear era, a structural contradiction emerged between
 *   public discourse and operational planning. Public rhetoric, policy
 *   statements, and campaign language treat great-power nuclear war as
 *   categorically unwinnable and winnability as an unspeakable concept—a
 *   rhetorical taboo. Yet classified military planning, war games, targeting
 *   doctrine, and force structure decisions remain embedded with winnability
 *   assumptions: the operational space still contains plans for limited
 *   victory through counterforce targeting, escalation control, damage
 *   limitation, and first-strike disarming capabilities. This story models
 *   the constraint that maintains this dual-layer structure: winnability
 *   became unsayable in public space while remaining operationally planned.
 *   The constraint extracts strategic autonomy from democratic oversight by
 *   hiding planning assumptions behind both the rhetorical taboo and
 *   classification barriers.
 *
 * KEY AGENTS:
 *   - Strategic planning establishment (agenda-setter): retains winnability-embedded plans while enforcing taboo
 *   - Democratic publics and legislative bodies (payers): excluded from planning discourse, bear risk of plans made without their input on assumptions they cannot challenge
 *   - Deterrence theorists (observers): provide intellectual legitimation for the dual structure
 *   - Classification system (mechanism): maintains boundary between discourse and planning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.79).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Winnability Rhetorical Taboo / Operational Planning Constraint (Post-1945)").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '0a35146e-50d2-4b01-a887-bbe3d5f758e4').
narrative_ontology:cs_kernel_codification('0a35146e-50d2-4b01-a887-bbe3d5f758e4', fixed_text).
narrative_ontology:cs_authority_grounding('0a35146e-50d2-4b01-a887-bbe3d5f758e4', extraction).
narrative_ontology:cs_interpretation_layer_present('0a35146e-50d2-4b01-a887-bbe3d5f758e4').
narrative_ontology:cs_reading_relation('0a35146e-50d2-4b01-a887-bbe3d5f758e4', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('0a35146e-50d2-4b01-a887-bbe3d5f758e4', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('0a35146e-50d2-4b01-a887-bbe3d5f758e4', foundational, winnability_operationally_necessary).
narrative_ontology:cs_axiom_status(winnability_operationally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0a35146e-50d2-4b01-a887-bbe3d5f758e4', winnability_operationally_necessary, instrumental).
narrative_ontology:cs_axiom('0a35146e-50d2-4b01-a887-bbe3d5f758e4', foundational, rhetorical_unsayability_enforced).
narrative_ontology:cs_axiom_status(rhetorical_unsayability_enforced, holdable).
narrative_ontology:cs_axiom_grounding('0a35146e-50d2-4b01-a887-bbe3d5f758e4', rhetorical_unsayability_enforced, conventional).
narrative_ontology:cs_reference_frame('0a35146e-50d2-4b01-a887-bbe3d5f758e4', nuclear_deterrence_framework).
narrative_ontology:cs_drift_state('0a35146e-50d2-4b01-a887-bbe3d5f758e4', contemporary_post_cold_war, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0a35146e-50d2-4b01-a887-bbe3d5f758e4', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_publics).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, mutually_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, extended_deterrence_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Military planners, defense strategists, and classified war-fighting doctrine authors retain operational planning for nuclear war scenarios that remain classified as 'winnability-constrained' (limited victory through force application). They enforce the rhetorical taboo publicly while maintaining war plans that assume victory conditions are structurally reachable through counterforce targeting, escalation control, or damage limitation. They control the classification boundary that separates public discourse from operational planning.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment, agenda_setter,
    institutional, generational, arbitrage, continental).

% Citizens of nuclear-armed states who are excluded from the planning discourse via the rhetorical taboo: winnability language is treated as unspeakable in public debate, campaign rhetoric, and legislative discourse. They bear the risk of wars planned under operational assumptions they are not permitted to contest. Their exit option is limited to voting for representatives who also operate under the same taboo.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_publics, payer,
    organized, biographical, constrained, continental).

% Nominally empowered to audit war plans and military strategy; in practice excluded from reviewing or challenging the winnability assumptions embedded in classified planning because those assumptions are protected by the rhetorical taboo and classification barriers. They can vote on defense budgets but cannot articulate opposition to the strategic doctrine funding them because the very language is taboo.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies, excluded).

% Academic and policy intellectuals who maintain theoretical frameworks legitimizing the strategic establishment's operational planning (mutual deterrence, extended deterrence, second-strike credibility). They provide analytical cover for the constraint: they argue that deterrence credibility requires keeping some winnability narrative alive operationally, even as public discourse must treat it as taboo to maintain societal stability.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, deterrence_theorists, observer,
    institutional, biographical, analytical, global).

% Strategic theorists, arms control advocates, and adversary-nation planners who would argue for alternative nuclear doctrines (no-first-use, minimum deterrence, mutual vulnerability) are systematically excluded from the planning conversation. Their exclusion is maintained by the rhetorical taboo itself: to articulate these alternatives requires using the winnability language the taboo forbids.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, rival_strategic_doctrines, excluded,
    moderate, biographical, trapped, global).

% The institutional machinery of document classification, security clearance compartmentalization, and operational security that maintains the boundary between public discourse (where winnability is unsayable) and classified planning (where it remains operationally assumed). Not a party to the constraint but the technical mechanism that enforces the dual-layer structure.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, classification_system, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(war_winnability_post_1945__rhetorical_contraction, classification_system).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planning_establishment).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains deterrence credibility by separating public discourse (winnability is unthinkable, deterrence is absolute) from operational planning (winnability remains constrained-but-reachable, deterrence is backed by war-fighting capability). This dual-layer structure allows strategic planners to retain flexible war-fighting options while keeping public and adversary belief in 'no-win' scenarios that discourage escalation.
% TRANSFER_FUNCTION: Transfers operational control and strategic flexibility from democratic publics to classified military planning establishments. Winnability language remains operationally active in force planning, doctrine, targeting, and deterrence infrastructure but is extracted from public discourse, legislative articulation, and electoral accountability. The public pays (bears the strategic risk of plans made without consent on assumptions they cannot challenge), while planners collect (retain freedom to plan for scenarios they cannot defend in public).
% ABSENT_VOICES: Arms control advocates, pacifist or minimalist deterrence theorists, adversary-state strategic thinkers, and those who would argue that winnability language should be suppressed operationally as well as rhetorically. These voices are structurally excluded not only by classification but by the rhetorical taboo itself: to articulate them requires using language the taboo forbids. Democratic publics are nominally present but their participation is foreclosed by the unsayability constraint.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, war-fighting winnability language would emerge into public discourse, legislative debate, and campaign rhetoric. Strategic plans would become contestable by democratic processes. Force planning assumptions would need to be defended in open forums. The rhetorical and operational spaces would converge, forcing either operational planning to shift toward minimum-deterrence frameworks or public discourse to normalize winnability language—either way, the strategic autonomy of the planning establishment would be constrained.
% FOUNDING_PROBLEM: Post-1945 nuclear strategy faced an incoherence: nuclear war's destructive scale made total victory strategically indefensible as public doctrine, but military planners required operational planning for victory scenarios to maintain credible deterrence against adversaries (keep them uncertain whether escalation would be controllable). The solution was institutionalizing a split: public speech treats winnability as unthinkable while classified planning treats it as operationally necessary.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners and defense intellectuals attest the founding problem remains live: deterrence credibility requires that adversaries believe the US retains war-fighting capability even if public rhetoric denies winnability. Arms control advocates and some legislative critics attest the founding problem is a constructed rationalization: that the perceived need for winnability in planning reflects institutional preference for strategic flexibility, not genuine deterrence requirements. Declassified Pentagon documents from the Cold War and contemporary strategic reviews show planning embedded with winnability assumptions while public policy language denies them; this split between archive and rhetoric is corroborated by multiple external sources.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68 at interval end) captures the strategic planners' extraction of operational flexibility and immunity from democratic challenge. Suppression (0.79) is high because the constraint's persistence depends on actively enforcing the unsayability taboo: the moment winnability language becomes sayable in public discourse, the planning assumptions become contestable. Theater ratio (0.62 at interval end and plateau) reflects the constraint's core structure: roughly 62% of the public rhetoric around deterrence is performative (maintaining the unthinkability narrative while operations assume winnability is constrained-but-reachable), while 38% reflects genuine coordination (keeping adversaries uncertain, maintaining deterrence credibility). The measurement series shows theater_ratio rising steeply through the Cold War and stabilizing post-1990, suggesting the rhetorical taboo became more theatrically entrenched even as the operational need arguably declined with Soviet collapse. Extraction plateaus at t=60 onward (post-Cold War), indicating the constraint persists without requiring escalation—it has become institutionalized. Suppression_requirement stays high because the taboo must be actively maintained through: (1) media management and political socialization (public figures who use winnability language are treated as dangerous), (2) classification barriers, (3) academic framing that normalizes the taboo as prudent. All measurements share the same time grid (t=0,10,20,40,60,80).
 *
 * PERSPECTIVAL GAP:
 *   Planners experience the constraint as enabling (it lets them retain flexibility while maintaining deterrence credibility), while democratic publics experience it as coercive (they are excluded from decisions about plans made under assumptions they cannot articulate). Legislators nominally have powerful position atoms but are identity-locked into the taboo—they are socialized into the unspeakability norm and face political punishment for violating it. The observer seat (deterrence theorists) has institutional power but analytical exit_options: they can theorize about the constraint but do not face its suppressive force directly. The engine computes these divergences from the structural data: planners have arbitrage-level exit (they can move to classified forums where winnability language is operational), while democratic publics have constrained exit (they can vote, but the taboo forecloses the language they would use to articulate their objection).
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners are structurally beneficiaries (d ≈ 0.15): they collect operational flexibility, strategic autonomy, immunity from public challenge. Their exit options are arbitrage-level because they operate in classified space where the taboo does not apply. Democratic publics are structurally victims (d ≈ 0.85): they are excluded from the planning conversation, pay the risk of plans made without their knowledge, and are suppressed by the unsayability norm that prevents them from articulating objections. Their exit options are constrained—they cannot leave the nation, cannot refuse to bear the strategic risk, and the taboo prevents them from using the language that would mobilize collective action. Legislative bodies occupy an ambiguous position: they have institutional power but are identity-locked into the taboo (legislator identity includes the constraint to not speak winnability language in public), which reduces their effective directionality toward the target end (d ≈ 0.70). Classification system (non-agent) is the mechanism maintaining the structural difference, not a seat with directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining deterrence credibility while keeping winnability language unthinkable) remains contested: planners argue it is live and necessary, arms control advocates argue it is a constructed rationalization for strategic flexibility. The disappearance verdict (world_rearranges) and founding_problem_status (contested) create a mismatch consistent with a constraint that has become theatrically maintenance-intensive (high theater_ratio plateau) without a concentrated beneficiary defending its necessity. The constraint looks like a piton candidate: the planning establishment administers it and benefits from it, but cannot defend the winnability assumptions in public, which suggests the benefit is diffuse (strategic autonomy) rather than concentrated (specific rents). However, it is coded as tangled_rope because: (1) there is genuine coordination function (deterrence credibility does require keeping adversary uncertainty about war-fighting capability), (2) there are identifiable asymmetric winners and losers (planners vs. publics), (3) requires active enforcement (the taboo must be continuously reinforced through media management, academic framing, political socialization). The piton / tangled_rope boundary here depends on whether the coordination function is separable from the extraction. If winnability-embedded planning is structurally necessary for deterrence credibility (the planning establishment's core claim), then the extraction is inseparable from coordination and it remains tangled_rope. If winnability-embedded planning could be replaced with minimum-deterrence doctrine without loss of credibility (the arms control claim), then the coordination function is separable and the constraint is more snare-like. This ambiguity is captured in omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_necessity_in_planning,
    'Is winnability-embedded planning structurally necessary for deterrence credibility, or is minimum-deterrence doctrine (no-first-use, mutual vulnerability) capable of maintaining equivalent deterrence at lower strategic extraction cost?',
    'Comparative strategic analysis of minimum-deterrence doctrine across states (China, India) and historical counterfactuals of US/NATO doctrine under different assumptions. Requires declassification of war plans and targeting doctrine to enable direct comparison.',
    'If minimum-deterrence doctrine maintains deterrence credibly, the winnability planning is pure strategic extraction hiding behind a coordination cover story (snare reclassification). If winnability planning is structurally necessary, the extraction is inseparable from the coordination function and the constraint remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(winnability_necessity_in_planning, conceptual, 'Whether winnability-embedded planning is a separable extraction layer or inseparable from deterrence coordination.').

omega_variable(
    rhetorical_taboo_internalization,
    'Is the unsayability of winnability language a structural suppression (external barriers—classification, media management, institutional gatekeeping) or an internalized norm (legislators and publics have been socialized to find winnability language cognitively unthinkable, not merely unspeakable)?',
    'Experimental evidence from decision-makers removed from institutional constraints (retired officials, academics in private settings): can they articulate winnability reasoning when classification and political consequences are removed? Historical comparison to pre-taboo discourse (1945-1950s strategic language).',
    'If suppression is internalized, exit options for democratic publics are identity_locked rather than merely constrained—they carry the taboo with them even if institutional barriers are removed. If structural, removal of classification and media gatekeeping would enable the suppressed discourse. Internalized suppression implies higher effective suppression (people suppress themselves), structural suppression implies lower effective suppression (it depends on enforcement machinery).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_taboo_internalization, empirical, 'Whether the winnability taboo is structurally enforced or internalized.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the axioms of this reading (winnability_operationally_necessary and rhetorical_unsayability_enforced) logically foreclose the deterrence_unthinkable reading, or do they coexist as different institutional framings of the same nuclear-deterrence kernel?',
    'Logical analysis: deterrence_unthinkable asserts winnability is categorically impossible post-nuclear; rhetorical_contraction asserts winnability is possible operationally but suppressed rhetorically. These are contradictory claims about the same referent (whether nuclear war is winnable). However, they could coexist if framed as: (1) different seats'' sincere beliefs (planners believe winnability is possible, public discourse treats it as impossible), (2) different time-indexed readings (winnability was possible 1945-1965, became impossible 1965-onward), (3) different theater/reality distinction (public believes truly, planners merely perform the belief for deterrence purposes). Does coexistence require epistemic reconciliation the constraint itself prevents?',
    'If readings foreclose each other, the constraint enforces selection of one reading and suppresses the other (snare-like feature). If readings coexist, the constraint creates a holding space for multiple incompatible institutional readings without requiring resolution (piton-like inertia). This affects whether the constraint is primarily extractive (foreclosure + selection) or primarily theatrical (multiple readings held in suspension).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether this reading logically forecloses or coexists with sibling readings.').

omega_variable(
    classification_boundary_stability,
    'Will the classification boundary that separates public discourse (winnability unsayable) from operational planning (winnability assumed) remain stable if adversary nations adopt minimum-deterrence doctrine or if nuclear weapons proliferate to non-state actors?',
    'Strategic futures analysis; comparative examination of how constraint operates in multi-nuclear-armed scenarios (India-Pakistan, Israel-Iran) where the winnability taboo is less institutionalized.',
    'If the boundary destabilizes, the constraint''s suppression_requirement would increase sharply (more active enforcement needed) or the theater_ratio would spike (the performance becomes obviously theatrical). If the boundary is robust, the constraint can persist through changing strategic environments. Boundary destabilization would be associated with increasing resistance from democratic publics and arms control advocates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(classification_boundary_stability, empirical, 'Stability of the classification boundary under changing strategic environments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t10, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(war__tr_t10, observed).
narrative_ontology:measurement(war__tr_t20, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(war__tr_t20, observed).
narrative_ontology:measurement(war__tr_t40, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(war__tr_t40, observed).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 60, 0.62).
narrative_ontology:measurement_basis(war__tr_t60, observed).
narrative_ontology:measurement(war__tr_t80, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 80, 0.62).
narrative_ontology:measurement_basis(war__tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t10, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(war__be_t10, observed).
narrative_ontology:measurement(war__be_t20, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(war__be_t20, observed).
narrative_ontology:measurement(war__be_t40, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(war__be_t40, observed).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(war__be_t60, observed).
narrative_ontology:measurement(war__be_t80, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(war__be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t10, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(war__su_t10, observed).
narrative_ontology:measurement(war__su_t20, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(war__su_t20, observed).
narrative_ontology:measurement(war__su_t40, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 40, 0.77).
narrative_ontology:measurement_basis(war__su_t40, observed).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 60, 0.79).
narrative_ontology:measurement_basis(war__su_t60, observed).
narrative_ontology:measurement(war__su_t80, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 80, 0.79).
narrative_ontology:measurement_basis(war__su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__rhetorical_contraction, 0.25).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, nuclear_first_strike_doctrine_credibility).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, classified_defense_planning_secrecy).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the war_winnability_post_1945 kernel. The rhetorical_contraction reading models a dual-layer structure: public discourse (winnability unsayable) vs. operational planning (winnability operationally active). This reading instantiates the extractive asymmetry between strategic planners (who retain operational flexibility) and democratic publics (who are excluded from planning discourse). The deterrence_unthinkable sibling reading asserts winnability is categorically impossible post-nuclear. The countervailing_thinkable reading asserts winnability remains structurally possible through limited counterforce options. Each reading has its own ε (extractiveness), beneficiary/victim structure, and type classification. They are linked via network.affects_constraints to enable analysis of how the kernel's interpretive contest shapes the strategic landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__rhetorical_contraction, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
