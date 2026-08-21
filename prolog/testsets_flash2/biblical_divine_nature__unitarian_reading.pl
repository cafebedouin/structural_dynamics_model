% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Divine Nature: Father Alone is God
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint represents the Unitarian reading of divine nature,
 *   asserting the numerical singularity of God, with the Father alone as God,
 *   and the Son/Spirit subordinate or created. It is one reading of the
 *   'biblical_divine_nature' kernel, which is also interpreted by Trinitarian
 *   and Modalist readings. This reading challenges established institutional
 *   hierarchies and credal orthodoxies, leading to high resistance and
 *   suppression from those systems. The claimed type is 'snare' because its
 *   persistence relies on actively undermining and extracting legitimacy from
 *   competing theological frameworks, with identifiable victims in
 *   institutional structures and traditional doctrines.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.65).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.7).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, snare).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Divine Nature: Father Alone is God").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '61691198-cb5d-46ba-819b-c1ae9afd54bb').
narrative_ontology:cs_kernel_codification('61691198-cb5d-46ba-819b-c1ae9afd54bb', fixed_text).
narrative_ontology:cs_authority_grounding('61691198-cb5d-46ba-819b-c1ae9afd54bb', distributed).
narrative_ontology:cs_reading_relation('61691198-cb5d-46ba-819b-c1ae9afd54bb', biblical_divine_nature__trinitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('61691198-cb5d-46ba-819b-c1ae9afd54bb', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('61691198-cb5d-46ba-819b-c1ae9afd54bb', foundational, god_is_numerically_one_person).
narrative_ontology:cs_axiom_status(god_is_numerically_one_person, holdable).
narrative_ontology:cs_axiom_grounding('61691198-cb5d-46ba-819b-c1ae9afd54bb', god_is_numerically_one_person, deontological).
narrative_ontology:cs_axiom('61691198-cb5d-46ba-819b-c1ae9afd54bb', foundational, son_and_spirit_are_subordinate_or_created).
narrative_ontology:cs_axiom_status(son_and_spirit_are_subordinate_or_created, holdable).
narrative_ontology:cs_axiom_grounding('61691198-cb5d-46ba-819b-c1ae9afd54bb', son_and_spirit_are_subordinate_or_created, empirically_contingent).
narrative_ontology:cs_reference_frame('61691198-cb5d-46ba-819b-c1ae9afd54bb', early_christian_scriptural_interpretation).
narrative_ontology:cs_drift_state('61691198-cb5d-46ba-819b-c1ae9afd54bb', post_nicene_creed_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('61691198-cb5d-46ba-819b-c1ae9afd54bb', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_adherents).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, individual_conscience).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Find theological clarity and simplicity in the singular nature of God, aligning with their interpretation of scripture. They benefit from a flattened ecclesiastical structure and direct access to divine truth without complex mediation.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_adherents, beneficiary,
    moderate, biographical, mobile, local).

% Experiences a challenge to its authority and doctrinal control, as this reading undermines the necessity of complex credal formulations and the interpretive power of established church structures. It bears the cost of dissent and potential schism.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_hierarchy, payer,
    institutional, generational, constrained, global).

% Is directly contradicted by this reading, which rejects the core tenets of Trinitarian or Modalist creeds. Its persistence depends on suppressing this alternative, making it a victim of the unitarian challenge to its foundational claims.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy, payer,
    institutional, civilizational, identity_locked, global).

% Their professional careers and theological frameworks are built upon Trinitarian doctrine. This reading directly challenges their intellectual and institutional standing, forcing them to defend their positions against a perceived heresy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_theologians, payer,
    organized, biographical, constrained, global).

% Benefits from the freedom to interpret scripture and divine nature directly, without coercion from established dogma. This reading empowers individual theological autonomy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, individual_conscience, beneficiary,
    powerless, immediate, mobile, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a theological understanding of God's nature that emphasizes divine unity and simplicity, providing a clear, non-complex framework for worship and belief for its adherents.
% TRANSFER_FUNCTION: Transfers theological authority from institutional hierarchies and complex credal statements to individual interpretation and a simplified understanding of God, from established church structures to individual believers.
% ABSENT_VOICES: Early church councils and patristic theologians who formulated Trinitarian doctrine are absent from the contemporary unitarian discourse, as their authority is implicitly rejected. They would argue for the necessity of complex theological distinctions to preserve both monotheism and the divinity of Christ.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the theological landscape would shift significantly. Unitarian denominations would lose their foundational doctrine, and Trinitarian orthodoxy would face less internal resistance, potentially consolidating its authority. The dynamics of religious dissent and institutional control would rearrange.
% FOUNDING_PROBLEM: The perceived complexity and internal contradictions of Trinitarian doctrine, and the perceived imposition of human-made creeds over clear biblical monotheism.
% FOUNDING_PROBLEM_CORROBORATION: Unitarian scholars and adherents attest that the problem of Trinitarian complexity and perceived biblical inconsistency remains live. Critics from Trinitarian traditions acknowledge the historical and ongoing theological debate, corroborating the persistence of the 'problem' as a point of contention, even if they disagree with the unitarian solution.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading fundamentally reallocates theological authority and challenges the 'rents' collected by traditional interpretive bodies. Suppression (0.70) is also high, reflecting the historical and ongoing efforts by orthodox institutions to suppress unitarian views, often through excommunication or marginalization. Resistance (0.75) is significant, as this reading directly opposes deeply entrenched doctrines and institutional power. The theater ratio is low (0.20) because the challenge is direct and substantive, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of unitarian adherents, this reading is a liberating truth (rope-like or even mountain-like in its perceived naturalness). From the perspective of institutional hierarchy and credal orthodoxy, it is a destructive heresy (snare-like), requiring active suppression to maintain their own coherence and power. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian adherents and individual conscience are beneficiaries, gaining theological clarity and autonomy (low d). Institutional hierarchy, credal orthodoxy, and Trinitarian theologians are victims, as their authority and doctrines are directly challenged and undermined by this reading (high d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_suppression_efficacy,
    'How effective has institutional suppression been in preventing the spread and acceptance of unitarian views over time?',
    'Historical sociological analysis of unitarian movements, their growth, periods of decline, and the impact of persecution or tolerance on their persistence.',
    'If suppression has been highly effective, it reinforces the ''snare'' classification by demonstrating the constraint''s reliance on coercion. If unitarian views have persisted despite suppression, it suggests a deeper resilience or a less effective suppressive mechanism than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_suppression_efficacy, empirical, 'Measures the real-world impact of suppression on the unitarian reading''s propagation.').

omega_variable(
    theological_coherence_vs_simplicity,
    'Is the theological ''simplicity'' offered by the unitarian reading a genuine reduction of complexity, or does it merely shift complexity to other areas (e.g., explaining biblical passages that imply divinity of Son/Spirit)?',
    'Comparative theological analysis of how different readings address perceived biblical contradictions or theological challenges, assessing whether complexity is truly reduced or merely relocated.',
    'If complexity is merely relocated, the ''beneficiary'' status of unitarian adherents (who value simplicity) might be overstated, potentially increasing the effective extraction from them by requiring new interpretive work. If genuine, it reinforces the coordination function for its adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_coherence_vs_simplicity, conceptual, 'Examines whether the claimed simplicity of the unitarian reading is structurally genuine.').

omega_variable(
    authority_grounding_ambiguity,
    'Is the authority grounding for this reading primarily ''scriptural interpretation'' (expertise) or ''individual conscience'' (diffuse_epistemic)?',
    'Analysis of unitarian theological discourse and historical movements to identify the dominant mode of legitimizing their claims.',
    'If primarily ''expertise'' in scriptural interpretation, it suggests a more structured, albeit decentralized, authority. If ''individual conscience'', it points to a more diffuse and potentially less stable grounding, impacting its resilience against institutional pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, conceptual, 'Clarifies the primary source of authority for the unitarian reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t500, biblical_divine_nature__unitarian_reading, theater_ratio, 500, 0.15).
narrative_ontology:measurement(bibl_tr_t1000, biblical_divine_nature__unitarian_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__unitarian_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(bibl_tr_t2000, biblical_divine_nature__unitarian_reading, theater_ratio, 2000, 0.2).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bibl_be_t500, biblical_divine_nature__unitarian_reading, base_extractiveness, 500, 0.6).
narrative_ontology:measurement(bibl_be_t1000, biblical_divine_nature__unitarian_reading, base_extractiveness, 1000, 0.65).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__unitarian_reading, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement(bibl_be_t2000, biblical_divine_nature__unitarian_reading, base_extractiveness, 2000, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bibl_su_t500, biblical_divine_nature__unitarian_reading, suppression_requirement, 500, 0.7).
narrative_ontology:measurement(bibl_su_t1000, biblical_divine_nature__unitarian_reading, suppression_requirement, 1000, 0.75).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__unitarian_reading, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(bibl_su_t2000, biblical_divine_nature__unitarian_reading, suppression_requirement, 2000, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'biblical_divine_nature' kernel. This unitarian reading directly challenges the Trinitarian and Modalist readings, influencing their legitimacy and requiring their adherents to actively defend their positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
