% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Self-Defense Reading of Article 51
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates the expansive preventive reading of UN
 *   Charter Article 51: the claim that self-defense extends to preemptive or
 *   preventive uses of force against non-state actors or emerging threats
 *   when the acting state judges necessity to be demonstrated. It is one
 *   reading of a contested kernel that also includes a narrow armed-attack
 *   reading and an unable/unwilling doctrine reading. The expansive reading
 *   is structurally contested because it shifts the authority to judge
 *   necessity from multilateral institutions to individual states,
 *   concentrating security autonomy in militarily capable actors while
 *   externalizing costs onto target populations and collective security
 *   institutions.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: Primary agenda-setter (institutional/arbitrage) â asserts and enforces the expansive interpretation through state practice and military doctrine
 *   - defense_sectors: Primary beneficiary (powerful/mobile) â gains operational latitude and resource flows from expansive threat definitions
 *   - target_region_populations: Primary target (powerless/trapped) â bears the direct costs of preventive strikes without voice in necessity determinations
 *   - multilateral_security_institutions: Secondary target (institutional/constrained) â loses authority and functional relevance when states bypass Chapter VII
 *   - international_legal_community: Analytical observer (analytical/analytical) â produces the contesting interpretive frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.78).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense Reading of Article 51").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '2f75a89e-4da3-4f61-a51b-9561d3ba7868').
narrative_ontology:cs_kernel_codification('2f75a89e-4da3-4f61-a51b-9561d3ba7868', fixed_text).
narrative_ontology:cs_authority_grounding('2f75a89e-4da3-4f61-a51b-9561d3ba7868', lineage).
narrative_ontology:cs_interpretation_layer_present('2f75a89e-4da3-4f61-a51b-9561d3ba7868').
narrative_ontology:cs_reading_relation('2f75a89e-4da3-4f61-a51b-9561d3ba7868', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('2f75a89e-4da3-4f61-a51b-9561d3ba7868', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('2f75a89e-4da3-4f61-a51b-9561d3ba7868', foundational, inherent_right_not_conditioned_on_armed_attack).
narrative_ontology:cs_axiom_status(inherent_right_not_conditioned_on_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('2f75a89e-4da3-4f61-a51b-9561d3ba7868', inherent_right_not_conditioned_on_armed_attack, deontological).
narrative_ontology:cs_axiom('2f75a89e-4da3-4f61-a51b-9561d3ba7868', foundational, necessity_determination_belongs_to_threatened_state).
narrative_ontology:cs_axiom_status(necessity_determination_belongs_to_threatened_state, holdable).
narrative_ontology:cs_axiom_grounding('2f75a89e-4da3-4f61-a51b-9561d3ba7868', necessity_determination_belongs_to_threatened_state, instrumental).
narrative_ontology:cs_reference_frame('2f75a89e-4da3-4f61-a51b-9561d3ba7868', inherent_self_defense_prerogative).
narrative_ontology:cs_drift_state('2f75a89e-4da3-4f61-a51b-9561d3ba7868', contemporary_multilateral_resistance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2f75a89e-4da3-4f61-a51b-9561d3ba7868', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sectors).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_security_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce an expansive interpretation of Article 51 that permits unilateral preventive force against non-state actors and emerging threats. They self-judge necessity, conduct strikes, and shape international legal discourse through state practice and doctrinal publications. They can exit the constraint by accepting narrower multilateral authorization but choose not to.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Benefit from budget expansion, doctrinal relevance, and operational latitude when self-defense is interpreted expansively. They provide the analytical frameworks that classify threats as necessitating preventive action, and their institutional survival is tied to maintaining a threat environment that justifies readiness.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sectors, beneficiary,
    powerful, biographical, mobile, global).

% Bear the direct costs of preventive and preemptive strikes conducted under the expansive reading. They have no voice in the necessity determination, no exit from the territory designated as a threat source, and no recourse against the acting state's legal claims.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% The UN Security Council and collective security architecture lose authority when states bypass Chapter VII authorization by claiming unilateral self-defense. Their enforcement capacity and legitimacy are eroded, but they cannot easily exit the system because the Charter framework depends on state consent.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_security_institutions, payer,
    institutional, generational, constrained, global).

% Govern the territory designated as the source of a threat. They bear sovereignty costs when foreign states conduct preventive strikes on their soil without consent, yet they often lack the power to prevent such strikes or to challenge the legal interpretation effectively in enforceable forums.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_state_governments, payer,
    moderate, immediate, constrained, national).

% Academic, judicial, and practitioner experts who analyze and contest the boundaries of Article 51. They produce the interpretive frameworks that either legitimize or delegitimize expansive claims, but they do not control state military decisions or institutional enforcement.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_legal_community, observer,
    analytical, civilizational, analytical, global).

% Represent the majority of UN member states but are structurally marginalized in the formation of self-defense doctrine. Their repeated assertions of Charter fidelity and opposition to unilateral preventive force are overridden by the practice of militarily capable states and the Security Council's permanent member dynamics.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, global_south_states, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism for states to defend themselves against non-state actors and emergent threats without waiting for multilateral authorization when delay could result in catastrophic harm.
% TRANSFER_FUNCTION: Moves the authority to judge necessity and legitimacy of preventive force from collective security institutions to individual militarily capable states, and transfers the physical and sovereignty costs onto target-region populations and the host states where strikes occur.
% ABSENT_VOICES: Target-region civilian populations and their governments are excluded from the necessity determination; Global South states and restrictive international legal scholars are present in forums but overridden by the practice of powerful states and the interpretive frameworks generated by benefiting parties.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished, militarily capable states would lose the primary legal cover for unilateral preventive drone strikes and special operations; they would need to seek Security Council authorization or accept narrower imminence standards; defense planning, basing agreements, and threat-assessment doctrines would reorganize around multilateral authorization timelines.
% FOUNDING_PROBLEM: The UN Charter's collective security mechanism was designed for interstate wars and assumes the Security Council can act promptly; non-state actor threats and emergent capabilities created situations where states argued that waiting for multilateral authorization would be suicidal.
% FOUNDING_PROBLEM_CORROBORATION: Militarily capable states and their defense establishments attest the problem is live and justify the expansive reading. The International Court of Justice, the majority of UN General Assembly resolutions, and independent international law scholars outside the benefiting parties attest that the Charter text does not support preventive force and that the gap should be addressed through institutional reform or amendment rather than unilateral reinterpretation.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint allows states to externalize the costs of security onto foreign populations and bypass institutional authorization. Suppression is higher (0.78) because the reading must actively override the Charter's textual limits, ICJ advisory opinions, and the multilateral veto process. Theater is moderate (0.45): military operations are real, but the necessity determination is frequently performative â threat assessments are classified, retrospective, and shaped by the same actors who benefit. Accessibility collapse (0.68) reflects that multilateral alternatives exist formally but are structurally sidelined by permanent member dynamics and self-judging claims. Resistance (0.70) is substantial because Global South states, international lawyers, and target governments consistently contest the reading. The temporal series show monotonic increases across the post-9/11 interval as the reading was asserted, normalized, and entrenched against pushback.
 *
 * PERSPECTIVAL GAP:
 *   The militarily capable state seat experiences this constraint as essential security coordination â a necessary adaptation of international law to asymmetric threats. The target population seat experiences the same structure as unilateral violence without recourse. The multilateral institution seat experiences it as erosion of the post-1945 security architecture. The engine computes these divergent classifications from the same structural data; the claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are structural beneficiaries (d near 0.0): they collect security autonomy and operational latitude. Defense sectors are secondary beneficiaries (d low). Target region populations are full targets (d near 1.0): they bear the kinetic and sovereignty costs without exit. Multilateral security institutions are targets (d high): they bear authority costs. The legal community sits near symmetric (d ~0.5) because they gain professional relevance from the contestation while bearing the cost of legal coherence loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading is not a pure snare because the underlying coordination problem â non-state actor threats that outpace multilateral decision-making â is genuine and acknowledged even by critics. A pure snare would lack this coordination substrate. It is also not a rope because the distribution of costs and authority is sharply asymmetric: not all states can equally self-judge necessity, and the victims are not participants in the arrangement. The Tangled Rope classification captures the hybrid: a real security coordination function layered with extraction that requires active enforcement (legal argumentation, Security Council vetoes, and military normalization) to hold against the Charter's narrower text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_power_reading,
    'Is the expansive reading a natural evolution of the inherent right of self-defense, or a constructed interpretation that serves the interests of militarily dominant states?',
    'Historical corpus analysis of pre-Charter state practice versus post-1945 textual interpretation; comparative assessment of whether non-capable states assert the same reading against capable ones.',
    'If constructed, the constraint is a false summit mountain or tangled rope; if natural law evolution, it approaches a genuine coordination evolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_power_reading, conceptual, 'Whether the expansive reading reflects natural legal evolution or constructed power interest.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of this reading due to structural suppression of alternatives (veto power over international institutions) or internalized acceptance by the international community?',
    'Measure state voting patterns and legal briefs: if Global South states consistently object but are overridden, suppression is structural; if they acquiesce in practice, it is partially internalized.',
    'Structural suppression supports a snare or tangled rope classification; internalized acceptance would suggest a rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of multilateral alternatives.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine security coordination function (protecting states from non-state threats) be separated from the unilateral extraction (self-judged necessity bypassing multilateral authority)?',
    'Institutional design analysis: whether a multilateral rapid-response authorization mechanism could deliver equivalent security outcomes without unilateral self-judgment.',
    'If separable, the expansive reading is largely extraction on a coordination frame; if inseparable, the extraction is partly the cost of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Separability of security coordination from unilateral authority extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t4, article_51_self_defense__expansive_preventive_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(arti_tr_t8, article_51_self_defense__expansive_preventive_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__expansive_preventive_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(arti_tr_t16, article_51_self_defense__expansive_preventive_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__expansive_preventive_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(arti_be_t4, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(arti_be_t8, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(arti_be_t16, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 20, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(arti_su_t4, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(arti_su_t8, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 12, 0.73).
narrative_ontology:measurement(arti_su_t16, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% The article_51_self_defense kernel decomposes into three structurally distinct constraints: the narrow reading treats Article 51 as a fixed coordination rule with low extraction; the expansive reading treated here layers high extraction onto the same text; the unable/unwilling reading creates a hybrid attribution framework. Each has a different epsilon, stakeholder structure, and classification. They are linked as a constraint family because they compete to occupy the same legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
