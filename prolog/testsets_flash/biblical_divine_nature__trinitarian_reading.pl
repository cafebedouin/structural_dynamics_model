% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Doctrine of Divine Nature
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The Trinitarian doctrine asserts that God exists as three co-equal,
 *   co-eternal persons (Father, Son, and Holy Spirit) in one divine essence
 *   (ousia). This constraint defines the nature of God within mainstream
 *   Christianity, aiming to preserve monotheism while affirming the divinity
 *   of Christ and the Holy Spirit. Its enforcement has historically involved
 *   anathemas, excommunication, and persecution against those holding
 *   non-Trinitarian views.
 *
 * KEY AGENTS:
 *   - trinitarian_clergy: Agenda setter (institutional/arbitrage) — defines and enforces orthodoxy
 *   - trinitarian_institutions: Beneficiary (institutional/arbitrage) — derives authority and legitimacy from the doctrine
 *   - non_trinitarian_theologians: Payer (powerful/constrained) — bears intellectual and professional costs for dissent
 *   - non_trinitarian_congregations: Payer (organized/constrained) — faces social and theological exclusion
 *   - historical_councils: Agenda setter (institutional/analytical) — formalized and enforced the doctrine
 *   - analytical_theologians: Observer (analytical/analytical) — studies the doctrine's historical and structural impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.65).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.75).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Doctrine of Divine Nature").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '7732d48d-1ad4-477d-ade3-856f6b310d12').
narrative_ontology:cs_kernel_codification('7732d48d-1ad4-477d-ade3-856f6b310d12', formalized).
narrative_ontology:cs_authority_grounding('7732d48d-1ad4-477d-ade3-856f6b310d12', lineage).
narrative_ontology:cs_interpretation_layer_present('7732d48d-1ad4-477d-ade3-856f6b310d12').
narrative_ontology:cs_reading_relation('7732d48d-1ad4-477d-ade3-856f6b310d12', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('7732d48d-1ad4-477d-ade3-856f6b310d12', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('7732d48d-1ad4-477d-ade3-856f6b310d12', foundational, three_persons_one_essence).
narrative_ontology:cs_axiom_status(three_persons_one_essence, holdable).
narrative_ontology:cs_axiom_grounding('7732d48d-1ad4-477d-ade3-856f6b310d12', three_persons_one_essence, deontological).
narrative_ontology:cs_axiom('7732d48d-1ad4-477d-ade3-856f6b310d12', foundational, divinity_of_christ_and_spirit).
narrative_ontology:cs_axiom_status(divinity_of_christ_and_spirit, holdable).
narrative_ontology:cs_axiom_grounding('7732d48d-1ad4-477d-ade3-856f6b310d12', divinity_of_christ_and_spirit, theological).
narrative_ontology:cs_reference_frame('7732d48d-1ad4-477d-ade3-856f6b310d12', nicene_creed_orthodoxy).
narrative_ontology:cs_drift_state('7732d48d-1ad4-477d-ade3-856f6b310d12', contemporary_pluralistic_theology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7732d48d-1ad4-477d-ade3-856f6b310d12', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_clergy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_institutions).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, non_trinitarian_theologians).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, non_trinitarian_congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, interprets, and enforces the Trinitarian doctrine within their respective denominations. Their authority and professional identity are deeply intertwined with its acceptance and defense. They benefit from the stability and coherence it provides to their theological system.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_clergy, agenda_setter,
    institutional, generational, arbitrage, global).

% Churches, seminaries, and theological bodies whose legitimacy and funding depend on adherence to Trinitarian orthodoxy. They benefit from the doctrinal unity and the exclusion of competing theological frameworks.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_institutions, beneficiary,
    institutional, civilizational, arbitrage, global).

% Academics and scholars who hold or advocate for Unitarian, Modalist, or other non-Trinitarian views. They face professional marginalization, difficulty in securing academic positions in mainstream institutions, and theological censure. Their 'exit' means abandoning their intellectual convictions or operating outside established theological frameworks.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, non_trinitarian_theologians, payer,
    powerful, biographical, constrained, global).

% Religious communities (e.g., Unitarian Universalists, Oneness Pentecostals, Christadelphians) whose theological positions are deemed heterodox by Trinitarian bodies. They experience social stigma, exclusion from ecumenical dialogues, and sometimes active proselytization against their beliefs. Their 'exit' means conforming to Trinitarianism or remaining a minority tradition.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, non_trinitarian_congregations, payer,
    organized, generational, constrained, global).

% Ecumenical councils (e.g., Nicaea, Constantinople) that historically defined and codified the Trinitarian doctrine, establishing its normative force and imposing anathemas on dissenters. They represent the foundational institutional act of enforcement.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, historical_councils, agenda_setter,
    institutional, civilizational, analytical, global).

% Scholars who study the historical development, philosophical underpinnings, and social impact of the Trinitarian doctrine from a critical, non-confessional stance. They analyze its structural properties and consequences without being bound by its theological claims.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, analytical_theologians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__trinitarian_reading, trinitarian_institutions).
narrative_ontology:fixing_cost_class(biblical_divine_nature__trinitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, unified theological framework for understanding the nature of God within Christianity, reconciling the divinity of Christ and the Holy Spirit with monotheism, thereby coordinating worship, liturgy, and doctrinal teaching across diverse communities.
% TRANSFER_FUNCTION: Transfers theological authority and legitimacy to Trinitarian institutions and clergy, while transferring costs of exclusion, marginalization, and anathema to non-Trinitarian theologians and congregations.
% ABSENT_VOICES: Early Christian groups holding adoptionist or subordinationist Christologies, and later Unitarian and Modalist movements, were systematically excluded from the councils that defined Trinitarian orthodoxy. Their voices, if present and given equal weight, would have fundamentally altered the doctrine's formulation and enforcement.
% DISAPPEARANCE_RATIONALE: If the Trinitarian doctrine vanished overnight, the theological landscape of Christianity would undergo a profound rearrangement. Major denominations would lose their central organizing principle, leading to fragmentation, redefinition of Christology and Pneumatology, and a re-evaluation of historical anathemas. The authority of many religious institutions would be severely undermined.
% FOUNDING_PROBLEM: The early Christian church faced the challenge of reconciling the monotheistic tradition of Judaism with the emerging belief in the divinity of Jesus Christ and the Holy Spirit, while avoiding polytheism or reducing Christ to a mere creature.
% FOUNDING_PROBLEM_CORROBORATION: Trinitarian institutions and clergy attest that the problem of coherently defining God's nature remains live, citing ongoing theological debates and the need to maintain doctrinal unity. Non-Trinitarian theologians, however, argue that the problem was 'solved' in a way that created new problems of exclusion and philosophical complexity, and that alternative solutions exist. Historical scholarship from outside the benefiting parties corroborates the existence of the original theological dilemma but also documents the coercive aspects of its resolution.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a complex theological understanding of God (preserving monotheism while affirming three divine persons) but does so with significant asymmetric extraction and suppression. Extractiveness (0.65) is high due to the costs imposed on dissenting theological traditions (e.g., career limitations, social ostracization). Suppression (0.75) is also high, reflecting the historical and ongoing enforcement through anathemas and exclusion. The theater ratio (0.20) is low, as the doctrine is actively defended and its theological function is central to the beneficiary institutions, though some performative aspects exist in maintaining a unified front.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Trinitarian clergy and institutions, the doctrine is a foundational truth (a 'mountain') essential for Christian identity and worship, with any 'extraction' seen as the necessary cost of maintaining orthodoxy. From the perspective of non-Trinitarian theologians and congregations, it is an imposed dogma (a 'snare') that extracts conformity and suppresses alternative interpretations, often through coercive means. The engine's computation of 'tangled_rope' reflects this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Trinitarian clergy and institutions are clear beneficiaries (d=0.0-0.2) as their authority and legitimacy are directly tied to the doctrine's acceptance. Non-Trinitarian theologians and congregations are targets (d=0.8-1.0), bearing the costs of exclusion and suppression. Historical councils acted as agenda setters, formalizing and enforcing the doctrine. Analytical theologians serve as observers, studying its structural impact without direct benefit or cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as Tangled Rope prevents mislabeling the doctrine as a pure 'mountain' (as its proponents claim) by highlighting its active enforcement and extractive elements. It also avoids mislabeling it as a pure 'snare' by acknowledging its genuine coordination function in defining a complex theological concept for a large religious tradition. The 'contested' status of the founding problem further supports the Tangled Rope classification, indicating that while a problem was solved, the solution's persistence now involves significant extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine theological truth, or a constructed doctrine that benefits identifiable institutional actors?',
    'Historical analysis of doctrinal development, examination of power dynamics in ecumenical councils, and theological arguments from non-Trinitarian perspectives.',
    'If a constructed doctrine, its classification shifts from a theological ''mountain'' (as claimed by beneficiaries) to a ''snare'' or ''tangled_rope'' (as computed by the engine), highlighting its extractive and suppressive functions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between theological truth and institutional construct.').

omega_variable(
    trinitarian_vs_unitarian_modalist,
    'This constraint is the Trinitarian reading of the ''biblical_divine_nature'' kernel. How would the classification change if a Unitarian or Modalist reading were adopted?',
    'Analyzing the structural properties (beneficiaries, victims, enforcement) of the Unitarian and Modalist readings as separate constraints.',
    'A Unitarian reading would likely have different beneficiaries (e.g., those emphasizing divine simplicity) and victims, potentially reducing extractiveness and suppression. A Modalist reading would shift the nature of ''persons'' and ''essence'', altering the coordination function and potentially the enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trinitarian_vs_unitarian_modalist, conceptual, 'Impact of alternative readings on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__trinitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t100, biblical_divine_nature__trinitarian_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(bibl_tr_t200, biblical_divine_nature__trinitarian_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(bibl_tr_t300, biblical_divine_nature__trinitarian_reading, theater_ratio, 300, 0.2).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__trinitarian_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bibl_be_t100, biblical_divine_nature__trinitarian_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(bibl_be_t200, biblical_divine_nature__trinitarian_reading, base_extractiveness, 200, 0.6).
narrative_ontology:measurement(bibl_be_t300, biblical_divine_nature__trinitarian_reading, base_extractiveness, 300, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__trinitarian_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(bibl_su_t100, biblical_divine_nature__trinitarian_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(bibl_su_t200, biblical_divine_nature__trinitarian_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(bibl_su_t300, biblical_divine_nature__trinitarian_reading, suppression_requirement, 300, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, christological_orthodoxy).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, pneumatological_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is the Trinitarian reading of the 'biblical_divine_nature' kernel. Sibling readings (Unitarian, Modalist) are modeled as separate constraints due to differing epsilon values and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
