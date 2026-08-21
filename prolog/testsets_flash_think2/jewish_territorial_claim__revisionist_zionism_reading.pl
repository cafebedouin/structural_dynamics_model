% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__revisionist_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__revisionist_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__revisionist_zionism_reading
 *   human_readable: Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)
 *   domain: Political History/Settler Colonialism/Nationalism Studies
 *
 * SUMMARY:
 *   This constraint represents the Revisionist Zionist reading of the Jewish
 *   territorial claim, which advocates for immediate Jewish sovereignty over
 *   a maximalist territory (both banks of the Jordan River) and explicitly
 *   rejects the need for Arab consent, instead relying on an 'Iron Wall' of
 *   military force to compel acceptance. This reading emerged in the 1920s
 *   and shaped a significant, often confrontational, strand of Zionist
 *   thought and action. This story instantiates one specific reading of the
 *   broader 'jewish_territorial_claim' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, 0.92).
domain_priors:suppression_score(jewish_territorial_claim__revisionist_zionism_reading, 0.95).
domain_priors:theater_ratio(jewish_territorial_claim__revisionist_zionism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(jewish_territorial_claim__revisionist_zionism_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__revisionist_zionism_reading, snare).
narrative_ontology:human_readable(jewish_territorial_claim__revisionist_zionism_reading, "Revisionist Zionist Maximalist Territorial Claim (Both Banks of Jordan)").
narrative_ontology:topic_domain(jewish_territorial_claim__revisionist_zionism_reading, "Political History/Settler Colonialism/Nationalism Studies").

domain_priors:requires_active_enforcement(jewish_territorial_claim__revisionist_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__revisionist_zionism_reading, '2c10d75d-3f5e-4c17-8bdb-e392e587aa61').
narrative_ontology:cs_kernel_codification('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', formalized).
narrative_ontology:cs_authority_grounding('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', lineage).
narrative_ontology:cs_interpretation_layer_present('2c10d75d-3f5e-4c17-8bdb-e392e587aa61').
narrative_ontology:cs_reading_relation('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', jewish_territorial_claim__labor_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_axiom('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', foundational, jewish_sovereignty_over_greater_israel_non_negotiable).
narrative_ontology:cs_axiom_status(jewish_sovereignty_over_greater_israel_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', jewish_sovereignty_over_greater_israel_non_negotiable, deontological).
narrative_ontology:cs_axiom('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', foundational, arab_consent_irrelevant_to_jewish_statehood).
narrative_ontology:cs_axiom_status(arab_consent_irrelevant_to_jewish_statehood, holdable).
narrative_ontology:cs_axiom_grounding('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', arab_consent_irrelevant_to_jewish_statehood, conventional).
narrative_ontology:cs_reference_frame('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', historic_jewish_homeland_maximalist_borders).
narrative_ontology:cs_drift_state('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2c10d75d-3f5e-4c17-8bdb-e392e587aa61', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__revisionist_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ideological and political force advocating for and implementing the maximalist territorial claim, explicitly rejecting Arab consent and relying on military force ('Iron Wall') to establish and maintain sovereignty over both banks of the Jordan River. They are the primary beneficiaries of the claim's success.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals and communities who directly benefit from the territorial expansion and the security provided by the military enforcement of the claim. Their presence on the land is a direct outcome and reinforcement of the maximalist vision.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, jewish_settlers, beneficiary,
    powerful, generational, constrained, regional).

% The indigenous population whose land, resources, and sovereignty are directly targeted and expropriated by the maximalist claim. They face military subjugation, displacement, and the systematic denial of their national and individual rights. Their resistance is explicitly anticipated and suppressed.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, palestinian_arabs, payer,
    powerless, generational, trapped, local).

% States bordering the claimed territory that face military threats, refugee crises, and regional instability as a direct consequence of the maximalist claim and its enforcement. They bear political, economic, and social costs.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, neighboring_arab_states, payer,
    organized, generational, constrained, regional).

% International bodies and states that observe, debate, and sometimes condemn or legitimize aspects of the claim. While they can exert diplomatic pressure or impose sanctions, they often lack the direct enforcement power to alter the constraint's operation without significant political will.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, international_community, observer,
    institutional, generational, analytical, global).

% While also advocating for Jewish statehood, this movement's more pragmatic approach (seeking international recognition, potentially smaller borders, and a Jewish majority) is structurally sidelined by the Revisionist reading's non-negotiable maximalism and rejection of external consent. They are excluded from the *implementation* of this specific reading.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__revisionist_zionism_reading, political_zionist_movement, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__revisionist_zionism_reading, revisionist_zionist_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__revisionist_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify Jewish national aspirations around a maximalist territorial vision for a Jewish state, extending 'from the Mediterranean Sea to the Jordan River' and beyond, and to coordinate the political and military strategy required to achieve and maintain it.
% TRANSFER_FUNCTION: Transfers land, resources, and sovereignty from Palestinian Arabs and potentially neighboring states to the Jewish state and its settlers, enforced through military power and the systematic denial of indigenous rights.
% ABSENT_VOICES: Palestinian Arabs and their representatives are explicitly excluded from any negotiation or consent regarding the territorial claim; their voices are suppressed by the military force and political doctrine that underpins the 'Iron Wall' strategy.
% DISAPPEARANCE_RATIONALE: If the maximalist claim and its military enforcement vanished overnight, the entire political, demographic, and military structure of the region would fundamentally reorganize. Land ownership, national borders, and the balance of power would be radically reconfigured, leading to a new, albeit potentially contested, equilibrium.
% FOUNDING_PROBLEM: The perceived existential vulnerability of the Jewish people due to historical antisemitism and the lack of a secure, sovereign homeland, necessitating a strong, self-reliant Jewish state with defensible borders.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem and its ongoing 'live' status are primarily attested by the Revisionist Zionist movement itself and its ideological adherents. External corroboration is highly contested, with international bodies and historians often pointing to the dispossession of Palestinians as a consequence, rather than a solution, to the problem, and questioning the necessity of maximalist claims for security.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__revisionist_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__revisionist_zionism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__revisionist_zionism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_territorial_claim__revisionist_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__revisionist_zionism_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__revisionist_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__revisionist_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) due to the explicit aim of acquiring land and sovereignty without consent, directly dispossessing another population. Suppression is extremely high (0.95) as the 'Iron Wall' doctrine mandates overwhelming military force to overcome anticipated Arab resistance and deny alternatives. Theater ratio is very low (0.05) because the military and political actions are direct, functional, and explicitly coercive, not performative. Accessibility collapse is high (0.88) as the goal is to eliminate any viable alternative for Palestinian Arabs. Resistance is also high (0.75) because the claim inherently generates strong opposition, which the constraint then actively suppresses.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Revisionist Zionist movement, this claim is a necessary act of national liberation and self-defense, ensuring Jewish security and historical rights. From the perspective of Palestinian Arabs, it is an act of settler-colonial dispossession and military occupation. The engine's classification as a Snare reflects the structural reality of extraction and suppression, independent of the proponents' self-justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Revisionist Zionist movement and Jewish settlers are clear beneficiaries, gaining land, sovereignty, and security from the claim's enforcement. Palestinian Arabs and neighboring Arab states are direct targets, bearing the costs of dispossession, military subjugation, and regional instability. The international community acts as an observer, with limited direct influence on the constraint's internal dynamics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_expansion_motivation,
    'Is the ''Iron Wall'' doctrine primarily a defensive measure for Jewish security, or is it a means to enable maximalist territorial expansion?',
    'Historical analysis of primary sources, policy decisions, and military actions, particularly regarding the proportionality of force and the pursuit of territorial gains beyond immediate defensive needs.',
    'If primarily for expansion, the constraint''s extractiveness is confirmed as inherent to its design; if purely defensive, a portion of the extractiveness might be reclassified as a coordination cost for security, though still borne by victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_expansion_motivation, empirical, 'Ambiguity regarding the primary motivation behind the ''Iron Wall'' doctrine.').

omega_variable(
    legitimacy_of_force_for_self_determination,
    'Is the use of military force to compel acceptance of a national territorial claim, which dispossesses another people, a legitimate exercise of self-determination?',
    'Conceptual analysis based on international law, human rights frameworks, and ethical principles of national self-determination, particularly concerning the rights of indigenous populations.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness are further condemned as unjust; if deemed legitimate under specific historical circumstances, the moral valence of the constraint shifts, though its structural properties remain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_force_for_self_determination, conceptual, 'Conceptual ambiguity regarding the ethical and legal legitimacy of the constraint''s core mechanism.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint truly a distinct reading of the ''jewish_territorial_claim'' kernel, or is it merely a more extreme variant of ''political_zionism_reading''?',
    'Detailed comparison of foundational texts and policy proposals from both Revisionist and Political Zionist movements, focusing on explicit statements regarding territorial scope, Arab consent, and the role of force.',
    'If not sufficiently distinct, the two readings might be merged or reclassified as a single constraint with a wider range of internal variation, potentially altering the network structure and axiom set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the distinctiveness of the Revisionist Zionist reading from other Zionist ideologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__revisionist_zionism_reading, 1920, 1940).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1920, 0.08).
narrative_ontology:measurement(jewi_tr_t1925, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1925, 0.07).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1930, 0.06).
narrative_ontology:measurement(jewi_tr_t1935, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1935, 0.05).
narrative_ontology:measurement(jewi_tr_t1940, jewish_territorial_claim__revisionist_zionism_reading, theater_ratio, 1940, 0.05).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1920, 0.85).
narrative_ontology:measurement(jewi_be_t1925, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1925, 0.87).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1930, 0.89).
narrative_ontology:measurement(jewi_be_t1935, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1935, 0.9).
narrative_ontology:measurement(jewi_be_t1940, jewish_territorial_claim__revisionist_zionism_reading, base_extractiveness, 1940, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1920, 0.88).
narrative_ontology:measurement(jewi_su_t1925, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1925, 0.9).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1930, 0.92).
narrative_ontology:measurement(jewi_su_t1935, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1935, 0.93).
narrative_ontology:measurement(jewi_su_t1940, jewish_territorial_claim__revisionist_zionism_reading, suppression_requirement, 1940, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__revisionist_zionism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, palestinian_resistance_movements).
narrative_ontology:affects_constraint(jewish_territorial_claim__revisionist_zionism_reading, arab_nationalist_movements).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_territorial_claim' kernel, focusing on the Revisionist Zionist perspective. Other readings (political, labor, cultural Zionism) are modeled as separate constraints due to significant differences in their ε values and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
