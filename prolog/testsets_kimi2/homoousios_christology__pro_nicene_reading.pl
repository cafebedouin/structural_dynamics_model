% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Pro-Nicene Homoousios Enforcement (Imperial-Ecclesiastical)
 *   domain: historical_theology/ecclesiastical_politics
 *
 * SUMMARY:
 *   This constraint story captures the pro-Nicene reading of the homoousios
 *   Christology kernel: the institutional enforcement of the claim that
 *   Christ is consubstantial (homoousios) with the Father. From the Council
 *   of Nicaea (325) through the First Council of Constantinople (381), this
 *   reading was progressively enforced through conciliar anathemas, episcopal
 *   deposition, and imperial edict. The constraint operated at the
 *   intersection of theological commitment and imperial political strategy,
 *   consolidating a unified Nicene episcopal hierarchy aligned with Roman
 *   state power. It is one reading of a contested theological kernel; sibling
 *   readings (arian_reading, semi_arian_reading) instantiate mutually
 *   exclusive ontological commitments. This JSON instantiates ONLY the
 *   pro-Nicene reading as an epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - nicene_episcopal_hierarchy: Primary agenda-setter (institutional/constrained) â administers doctrine, anathemas, and conciliar enforcement; collects concentrated ecclesiastical authority
 *   - imperial_authority: Primary beneficiary (institutional/arbitrage) â gains religious unity and political legitimation from a unified church; can shift theological allegiance for political convenience until Theodosius
 *   - arian_communities: Primary target (powerless/identity_locked) â bear costs of anathema, exclusion, and legal proscription; trapped by theological identity within the empire
 *   - semi_arian_communities: Secondary target (moderate/constrained) â occupy a foreclosed compromise position; progressively marginalized as conciliar definitions harden
 *   - dissenting_bishops: Tertiary target (moderate/trapped) â face deposition and exile for non-conformity; personal authority tied to subscription
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.78).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.85).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Enforcement (Imperial-Ecclesiastical)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "historical_theology/ecclesiastical_politics").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, 'feb4fdf8-d57c-4748-bb91-01c22d707cdb').
narrative_ontology:cs_kernel_codification('feb4fdf8-d57c-4748-bb91-01c22d707cdb', fixed_text).
narrative_ontology:cs_authority_grounding('feb4fdf8-d57c-4748-bb91-01c22d707cdb', lineage).
narrative_ontology:cs_interpretation_layer_present('feb4fdf8-d57c-4748-bb91-01c22d707cdb').
narrative_ontology:cs_reading_relation('feb4fdf8-d57c-4748-bb91-01c22d707cdb', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('feb4fdf8-d57c-4748-bb91-01c22d707cdb', homoousios_christology__semi_arian_reading, forecloses).
narrative_ontology:cs_axiom('feb4fdf8-d57c-4748-bb91-01c22d707cdb', foundational, divine_substance_identity).
narrative_ontology:cs_axiom_status(divine_substance_identity, holdable).
narrative_ontology:cs_axiom_grounding('feb4fdf8-d57c-4748-bb91-01c22d707cdb', divine_substance_identity, theological).
narrative_ontology:cs_axiom('feb4fdf8-d57c-4748-bb91-01c22d707cdb', secondary, trinitarian_monarchy).
narrative_ontology:cs_axiom_status(trinitarian_monarchy, holdable).
narrative_ontology:cs_axiom_grounding('feb4fdf8-d57c-4748-bb91-01c22d707cdb', trinitarian_monarchy, theological).
narrative_ontology:cs_reference_frame('feb4fdf8-d57c-4748-bb91-01c22d707cdb', nicaean_theological_unity).
narrative_ontology:cs_drift_state('feb4fdf8-d57c-4748-bb91-01c22d707cdb', theodosian_imperial_enforcement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('feb4fdf8-d57c-4748-bb91-01c22d707cdb', '').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_authority).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_communities).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_communities).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, dissenting_bishops).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the homoousios doctrine through conciliar decrees, episcopal ordination, and anathemas. Defines orthodox boundaries and excludes dissenters. Benefits from the concentration of theological authority in the episcopal office and the alliance with imperial enforcement.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, civilizational, constrained, continental).

% Gains religious unity across the empire and legitimation for imperial rule through alliance with the unified Nicene church. Enforces doctrinal conformity via edicts and military support for conciliar decisions. Benefits from a church that is structurally dependent on state enforcement.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).

% Bear the costs of anathema, exclusion from church property, loss of legal standing, and eventual dispersion. Their theological identity marks them as heretical under imperial law. Exit means renouncing their theological commitments or leaving the empire.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_communities, payer,
    powerless, generational, identity_locked, continental).

% Occupy a theological middle ground that the pro-Nicene framework progressively forecloses. Their compromise language (homoiousios) is rejected as inadequate. Bear costs of marginalization as conciliar definitions harden around homoousios.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_communities, payer,
    moderate, generational, constrained, continental).

% Individual bishops who refuse to subscribe to the Nicene formula face deposition, exile, and loss of see. Their personal authority and livelihood depend on conformity. Exit options are limited by the network of episcopal accountability and imperial enforcement.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, dissenting_bishops, payer,
    moderate, biographical, trapped, regional).

% Theologians whose alternative Trinitarian formulations fall outside the Nicene boundary are structurally excluded from conciliar deliberations, imperial church patronage, and academic transmission within the empire.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, excluded_theologians, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the diverse Christian communities of the Roman Empire under a single Trinitarian theological framework, preventing schism over the Son's relationship to the Father by establishing a shared creedal boundary.
% TRANSFER_FUNCTION: Moves authority from local and dissenting theological communities to the centralized conciliar and episcopal hierarchy, and transfers political legitimacy from the imperial office to the Nicene church in exchange for theological uniformity.
% ABSENT_VOICES: Arian bishops and communities, especially in the Gothic and Germanic missions, were excluded from conciliar deliberations after 325; their theological objections were recorded only in the polemics of their opponents. Homoiousian theologians seeking compromise were also progressively excluded as the pro-Nicene framework hardened.
% DISAPPEARANCE_RATIONALE: The Theodosian settlement and the unified imperial church depended on this doctrinal boundary. Without it, Arianism would have remained structurally viable within the empire, the episcopal hierarchy would have lacked a unifying theological criterion, and the Roman state would have lost its primary mechanism for religious legitimation.
% FOUNDING_PROBLEM: Controversy over the Son's ontological status relative to the Father threatened to fracture the imperial church into irreconcilable factions, undermining both ecclesiastical unity and the imperial policy of using a unified church as a social stabilizer.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians and the non-Nicene historiographical tradition (e.g., Philostorgius) attest that the theological controversy was real, but corroborate from outside the beneficiary set that the imperial enforcement machinery outlasted the immediate crisis and became a permanent governance structure.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint extracts theological compliance, concentrates authority in the conciliar hierarchy, and suppresses alternative Trinitarian formulations through active enforcement. Suppression is higher (0.85) due to anathemas, imperial edicts, depositions, and the structural exclusion of dissent. Theater_ratio is moderate (0.45): conciliar proceedings and creedal recitations have substantial performative dimension, but the enforcement machinery (exile, deposition) produces real material costs. Accessibility_collapse is high (0.80) because within the empire, Arian and Semi-Arian alternatives became structurally inaccessible after Theodosian enforcement. Resistance is moderate (0.60): Arianism persisted among Germanic tribes and within the empire for decades, generating sustained pushback. The measurement series show a cyclical pattern: extraction and suppression dip during the Arian resurgence under Constantius (approx. T=16-24) and recover sharply under Theodosius (T=40-56), reflecting the constraint's dependence on imperial enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   The Nicene episcopal hierarchy experiences the constraint as necessary guardianship of apostolic truth and legitimate coordination against schism. The imperial authority experiences it as a useful political instrument for religious unity. Arian and Semi-Arian communities experience the identical structure as coercive theological imposition. The engine computes this divergence from the structural data: agenda-setters and beneficiaries with constrained or arbitrage exits derive low directionality, while payers with identity-locked or trapped exits derive high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (imperial_authority, nicene_episcopal_hierarchy) derive low directionality: the constraint subsidizes their authority and legitimation, and their exits (arbitrage, constrained) prevent full target status. Victims (arian_communities, semi_arian_communities, dissenting_bishops) derive high directionality: they bear the costs of exclusion and suppression, and their exits are identity-locked or trapped, amplifying effective extraction. The excluded theologians sit at the high-target boundary of the excluded category. No directionality overrides are necessary; the derivation chain captures the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Trinitarian controversy threatening imperial church schism â was substantially addressed by 381. The constraint persisted well beyond the immediate crisis as permanent institutional machinery. The mismatch between contested founding_problem_status and world_rearranges disappearance_verdict signals that the arrangement developed zombie tendencies: it retained live theological boundary functions (preventing new heresies) but also carried substantial inertial enforcement beyond the original coordination need. This prevents mislabeling the constraint as pure coordination (it accumulated extraction after solving the crisis) or pure extraction (it did solve a genuine unification problem). The cyclical measurements reinforce this: enforcement intensity tracked imperial politics more closely than ongoing theological crisis, suggesting mandatrophy drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_truth_vs_institutional_enforcement,
    'Does the pro-Nicene reading claim mountain-like status (divine necessity) for a constraint whose persistence depends on active institutional enforcement and political alignment?',
    'Comparative analysis of enforcement decay: if the constraint persisted through theological tradition without state enforcement (as in post-Roman contexts), the mountain claim strengthens; if it collapsed when imperial enforcement withdrew, the claim weakens.',
    'A purely enforced persistence supports the tangled_rope classification; self-sustaining persistence would suggest the constraint contains genuine mountain elements (truth claims) within a tangled enforcement structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_truth_vs_institutional_enforcement, conceptual, 'Tension between theological necessity claim and institutional enforcement reality.').

omega_variable(
    semi_arian_exclusion_logic,
    'Was the exclusion of homoiousios (Semi-Arianism) structurally necessary to preserve the homoousios claim, or was it a political purification of compromise positions?',
    'Formal logical analysis of the ontological commitments: if homoiousios is genuinely incompatible with homoousios in Trinitarian ontology, the exclusion is doctrinally compelled; if compatible under reinterpretation (as the Cappadocians later achieved with some homoiousians), the exclusion was politically driven.',
    'If doctrinally compelled, the victimization of semi-arian communities is structurally embedded in the reading; if politically driven, it represents extractive surplus suppression beyond the coordination need.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semi_arian_exclusion_logic, conceptual, 'Whether semi-arian exclusion was doctrinally necessary or political surplus.').

omega_variable(
    enforcement_oscillation_source,
    'Does the oscillation in Nicene enforcement reflect genuine theological dispute resolution, or does the intermittent reinforcement itself function as an extraction mechanism?',
    'Pattern analysis: if enforcement cycles correlate with imperial succession crises, the oscillation is political; if they correlate with conciliar theological development, it is doctrinal.',
    'Political correlation would strengthen the tangled_rope reading by showing state interests drive enforcement intensity; doctrinal correlation would suggest genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_oscillation_source, empirical, 'Source of enforcement cyclicality in the Nicene-Arian conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_christology__pro_nicene_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(homo_tr_t8, homoousios_christology__pro_nicene_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(homo_tr_t16, homoousios_christology__pro_nicene_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(homo_tr_t24, homoousios_christology__pro_nicene_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(homo_tr_t32, homoousios_christology__pro_nicene_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(homo_tr_t40, homoousios_christology__pro_nicene_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(homo_tr_t48, homoousios_christology__pro_nicene_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement(homo_tr_t56, homoousios_christology__pro_nicene_reading, theater_ratio, 56, 0.45).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_christology__pro_nicene_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(homo_be_t8, homoousios_christology__pro_nicene_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(homo_be_t16, homoousios_christology__pro_nicene_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(homo_be_t24, homoousios_christology__pro_nicene_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(homo_be_t32, homoousios_christology__pro_nicene_reading, base_extractiveness, 32, 0.3).
narrative_ontology:measurement(homo_be_t40, homoousios_christology__pro_nicene_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(homo_be_t48, homoousios_christology__pro_nicene_reading, base_extractiveness, 48, 0.72).
narrative_ontology:measurement(homo_be_t56, homoousios_christology__pro_nicene_reading, base_extractiveness, 56, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_christology__pro_nicene_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(homo_su_t8, homoousios_christology__pro_nicene_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(homo_su_t16, homoousios_christology__pro_nicene_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(homo_su_t24, homoousios_christology__pro_nicene_reading, suppression_requirement, 24, 0.25).
narrative_ontology:measurement(homo_su_t32, homoousios_christology__pro_nicene_reading, suppression_requirement, 32, 0.35).
narrative_ontology:measurement(homo_su_t40, homoousios_christology__pro_nicene_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(homo_su_t48, homoousios_christology__pro_nicene_reading, suppression_requirement, 48, 0.82).
narrative_ontology:measurement(homo_su_t56, homoousios_christology__pro_nicene_reading, suppression_requirement, 56, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, semi_arian_reading).

% DUAL FORMULATION NOTE:
% This constraint is the pro-Nicene reading of the homoousios kernel; sibling readings instantiate mutually exclusive ontological commitments from the same theological controversy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
