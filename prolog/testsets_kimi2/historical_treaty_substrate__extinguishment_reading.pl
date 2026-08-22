% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Treaty Extinguishment Reading â Completed Property Transaction
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the extinguishment reading of the contested
 *   kernel historical_treaty_substrate. Under this reading, treaties between
 *   Indigenous nations and settler states are treated as completed property
 *   transactions in which Indigenous parties ceded territorial sovereignty in
 *   exchange for defined reserves, annuities, and narrow treaty rights. The
 *   settler state thereby becomes the sole legitimate authority over ceded
 *   territory. Sibling readings â nation_to_nation_reading and
 *   stewardship_reading â instantiate structurally distinct claims with
 *   different beneficiary/victim structures and epsilon values. The
 *   claim/metric independence is maintained: the constraint is claimed as
 *   tangled_rope while the authored metrics reflect a heavily extractive,
 *   actively enforced arrangement with rising performative maintenance.
 *
 * KEY AGENTS:
 *   - settler_state (institutional/analytical): agenda-setter and primary beneficiary â administers extinguishment doctrine, gains sole territorial authority
 *   - indigenous_nations (organized/constrained): dual-positioned payer/beneficiary â cede territorial sovereignty, receive narrow treaty rights
 *   - indigenous_jurisdiction_advocates (moderate/constrained): excluded â assert unextinguished sovereignty, foreclosed by the transaction framework
 *   - legal_historians (analytical): observer â document divergence between transaction framing and historical practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.82).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.76).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Treaty Extinguishment Reading â Completed Property Transaction").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '25cad553-ce09-4cf5-ba06-1b57da485e24').
narrative_ontology:cs_kernel_codification('25cad553-ce09-4cf5-ba06-1b57da485e24', fixed_text).
narrative_ontology:cs_authority_grounding('25cad553-ce09-4cf5-ba06-1b57da485e24', lineage).
narrative_ontology:cs_interpretation_layer_present('25cad553-ce09-4cf5-ba06-1b57da485e24').
narrative_ontology:cs_reading_relation('25cad553-ce09-4cf5-ba06-1b57da485e24', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_reading_relation('25cad553-ce09-4cf5-ba06-1b57da485e24', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('25cad553-ce09-4cf5-ba06-1b57da485e24', foundational, sovereignty_ceded_by_treaty_transaction).
narrative_ontology:cs_axiom_status(sovereignty_ceded_by_treaty_transaction, holdable).
narrative_ontology:cs_axiom_grounding('25cad553-ce09-4cf5-ba06-1b57da485e24', sovereignty_ceded_by_treaty_transaction, conventional).
narrative_ontology:cs_axiom('25cad553-ce09-4cf5-ba06-1b57da485e24', foundational, indigenous_title_fully_extinguishable).
narrative_ontology:cs_axiom_status(indigenous_title_fully_extinguishable, holdable).
narrative_ontology:cs_axiom_grounding('25cad553-ce09-4cf5-ba06-1b57da485e24', indigenous_title_fully_extinguishable, conventional).
narrative_ontology:cs_reference_frame('25cad553-ce09-4cf5-ba06-1b57da485e24', extinguished_sovereignty_framework).
narrative_ontology:cs_drift_state('25cad553-ce09-4cf5-ba06-1b57da485e24', contemporary_indigenous_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('25cad553-ce09-4cf5-ba06-1b57da485e24', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal doctrine that historical treaties were completed property transactions extinguishing Indigenous title. Gains sole legitimate authority and jurisdiction over ceded territory, enforced through courts, land title registries, and legislation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, settler_state, beneficiary).

% Under this reading, they are treaty parties who ceded territorial sovereignty in exchange for defined reserves, annuities, and narrow treaty rights. They are structurally the source of the transferred sovereignty and the recipients of limited, circumscribed benefits. Exit is constrained by the doctrine's assertion that sovereignty was fully and finally extinguished.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_nations, beneficiary).

% Advance claims of unextinguished Indigenous sovereignty and nation-to-nation treaty relationships. Structurally excluded from the extinguishment framework, which treats their claims as legally foreclosed by the completed transaction.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_jurisdiction_advocates, excluded,
    moderate, generational, constrained, national).

% Analyze the historical treaty-making process and its legal interpretations from an analytical seat outside the immediate beneficiary structure.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, legal_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a definitive, marketable land title regime for settler jurisdiction by resolving competing territorial claims through a formalized property transaction.
% TRANSFER_FUNCTION: Transfers territorial sovereignty and comprehensive jurisdiction from Indigenous nations to the settler state, in exchange for defined reserves, annuity payments, and narrow, non-sovereign treaty rights.
% ABSENT_VOICES: Indigenous nations asserting unextinguished sovereignty, proponents of nation-to-nation ongoing consent frameworks, and shared stewardship advocates are structurally excluded; the extinguishment reading treats their claims as already answered by the completed transaction.
% DISAPPEARANCE_RATIONALE: The constraint underpins the entire settler-state property and jurisdiction system; its disappearance would destabilize land titles, reactivate Indigenous territorial claims, and force a fundamental rearrangement of sovereignty allocation.
% FOUNDING_PROBLEM: The expansion of settler populations and economies required a legal mechanism to resolve competing claims to territorial sovereignty and to create clear, alienable title for settler land use and governance.
% FOUNDING_PROBLEM_CORROBORATION: Settler-state legal historians and courts assert the problem was resolved by treaty. Indigenous historians, international human rights bodies, and critical legal scholars attest from outside the primary benefiting party that the problem was never legitimately resolved and that the arrangement persists as a coercive extraction mechanism rather than a genuine solution.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the arrangement transfers comprehensive territorial sovereignty for narrow, circumscribed rights and payments that do not track the value of the jurisdiction lost. Suppression is high (0.76) because the constraint's persistence depends on actively excluding Indigenous jurisdictional claims through courts and legislation. Theater_ratio is elevated (0.68) and rising: as Indigenous rights discourse and historical scrutiny have intensified, the settler state has increased performative gestures of treaty honour and reconciliation while maintaining the underlying extinguishment framework. Accessibility_collapse is high (0.80) because once the extinguishment doctrine is accepted in the legal system, alternatives such as unextinguished title or shared sovereignty collapse as legally cognizable. Resistance is substantial (0.72) because Indigenous nations and advocates actively contest the framework through litigation, land defence, and international forums.
 *
 * PERSPECTIVAL GAP:
 *   The settler_state seat computes as beneficiary/agenda-setter: it authored the framework, administers its enforcement, and collects the transferred sovereignty. The indigenous_nations seat computes as net target despite receiving treaty rights, because the sovereignty cession vastly outweighs the compensatory bundle; the engine captures this through the directionality_override on the organized power atom. The excluded indigenous_jurisdiction_advocates seat experiences the constraint as a foreclosure of their entire normative framework. These divergences are structural, not perspectival illusions.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler_state is a clear beneficiary of the constraint (d near 0.0) because it receives sole legitimate authority and jurisdiction. Indigenous_nations are declared in both the beneficiary array (for narrow treaty rights) and the victim array (for territorial sovereignty cession). Automatic derivation would place them near symmetric (d â 0.5), but the structural reality is that the extracted sovereignty is comprehensive while the returned benefits are minimal and circumscribed. A directionality_override pushes the organized power atom toward the target end (d = 0.82) to reflect net extraction. Indigenous_jurisdiction_advocates are excluded rather than coordinated; their exclusion is the boundary the enforcement machinery defends.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â establishing clear, alienable title for settler expansion â is dead from the settler-state perspective: the title system is mature and the territorial dispossession is largely accomplished. Yet the arrangement persists because it continues to underwrite the entire property and sovereignty order. This is mandatrophy: the coordination rationale has atrophied into a rent-preservation function. The rising theater_ratio measurements document this trajectory, showing that an increasing share of constraint activity is performative maintenance of a settled structure rather than genuine coordination. The R5 mismatch (founding_problem_status = dead, disappearance_verdict = world_rearranges) flags the constraint as a zombie arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_consent_coercion_ambiguity,
    'Were historical treaties formed through genuine consent or through coercion, fraud, and structural duress?',
    'Comparative archival research and oral history documentation establishing the conditions of treaty negotiation, including bargaining power asymmetries andç¿»è¯ failures.',
    'If coercion is established, the transaction framing collapses and the constraint''s extractiveness increases toward snare territory; if consent is verified, the coordination framing strengthens and the tangled_rope classification tightens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_consent_coercion_ambiguity, empirical, 'Empirical ambiguity about treaty formation conditions').

omega_variable(
    extinguishment_doctrine_alternatives,
    'Could the settler property system have achieved coordination without the extinguishment of Indigenous title, through sui generis recognition or shared jurisdiction?',
    'Comparative constitutional analysis of jurisdictions that recognize Aboriginal title without extinguishment, and assessment of whether land markets functioned under alternative tenure regimes.',
    'If coordination was possible without extinguishment, the doctrine''s extraction component was unnecessary overhead, supporting a snare classification; if extinguishment was structurally necessary for the coordination function, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinguishment_doctrine_alternatives, conceptual, 'Conceptual ambiguity about legal necessity of extinguishment').

omega_variable(
    kernel_reading_contest_location,
    'Does the disagreement between the extinguishment reading and its siblings turn on empirical historical facts, normative legal principles, or the ontological status of treaty texts?',
    'Meta-legal analysis of the kernel''s divergence points to determine whether the readings are empirically decidable or represent incommensurable normative frameworks.',
    'If the divergence is empirical, one reading could be structurally falsified; if normative or ontological, the kernel is a permanent site of contest requiring constraint-family decomposition rather than resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Location of disagreement between kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 170).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__extinguishment_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__extinguishment_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement(hist_tr_t135, historical_treaty_substrate__extinguishment_reading, theater_ratio, 135, 0.62).
narrative_ontology:measurement(hist_tr_t170, historical_treaty_substrate__extinguishment_reading, theater_ratio, 170, 0.68).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 25, 0.9).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.87).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 75, 0.84).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.81).
narrative_ontology:measurement(hist_be_t135, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 135, 0.8).
narrative_ontology:measurement(hist_be_t170, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 170, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 75, 0.82).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement(hist_su_t135, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 135, 0.75).
narrative_ontology:measurement(hist_su_t170, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 170, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel historical_treaty_substrate. The extinguishment reading treats treaties as completed property transactions with sovereign title passing fully to the settler state. Its siblings â nation_to_nation_reading and stewardship_reading â instantiate structurally distinct claims with different beneficiary/victim structures and epsilon values. Decomposition follows the epsilon-invariance principle: the natural-language label 'treaty' conflates multiple structurally distinct constraints that must be modeled separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
