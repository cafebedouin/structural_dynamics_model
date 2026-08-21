% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael (Religious Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'religious_zionist_reading' of the
 *   'jewish_sovereignty_palestine' kernel. It posits that the divine promise
 *   of Eretz Yisrael to the Jewish people establishes an inalienable
 *   territorial claim, with the modern State of Israel representing a
 *   theological fulfillment of this covenant. This reading asserts maximalist
 *   territorial rights, often rejecting the legitimacy of partition or
 *   Palestinian claims to sovereignty, and frames the Jewish people as a
 *   covenant community with a unique, divinely ordained relationship to the
 *   land.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.92).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.88).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '06e0a57e-8285-4a81-810c-219a05c7e61e').
narrative_ontology:cs_kernel_codification('06e0a57e-8285-4a81-810c-219a05c7e61e', fixed_text).
narrative_ontology:cs_authority_grounding('06e0a57e-8285-4a81-810c-219a05c7e61e', lineage).
narrative_ontology:cs_interpretation_layer_present('06e0a57e-8285-4a81-810c-219a05c7e61e').
narrative_ontology:cs_reading_relation('06e0a57e-8285-4a81-810c-219a05c7e61e', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('06e0a57e-8285-4a81-810c-219a05c7e61e', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('06e0a57e-8285-4a81-810c-219a05c7e61e', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('06e0a57e-8285-4a81-810c-219a05c7e61e', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('06e0a57e-8285-4a81-810c-219a05c7e61e', foundational, divine_covenant_inalienable_land_title).
narrative_ontology:cs_axiom_status(divine_covenant_inalienable_land_title, holdable).
narrative_ontology:cs_axiom_grounding('06e0a57e-8285-4a81-810c-219a05c7e61e', divine_covenant_inalienable_land_title, theological).
narrative_ontology:cs_axiom('06e0a57e-8285-4a81-810c-219a05c7e61e', foundational, statehood_as_messianic_fulfillment).
narrative_ontology:cs_axiom_status(statehood_as_messianic_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('06e0a57e-8285-4a81-810c-219a05c7e61e', statehood_as_messianic_fulfillment, theological).
narrative_ontology:cs_reference_frame('06e0a57e-8285-4a81-810c-219a05c7e61e', biblical_covenant_fulfillment).
narrative_ontology:cs_drift_state('06e0a57e-8285-4a81-810c-219a05c7e61e', contemporary_political_reality, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('06e0a57e-8285-4a81-810c-219a05c7e61e', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the inheritors of a divine covenant, they are the primary beneficiaries of the land's 'return' and the theological fulfillment of statehood. Their identity is deeply intertwined with the land, making exit from this claim unthinkable.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, beneficiary).

% Bear the direct costs of territorial maximalism, including displacement, loss of land, and denial of self-determination. Their claims to the land are rendered illegitimate or subordinate by the divine mandate, leaving them with no recognized exit within this framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people, excluded).

% The political entity tasked with actualizing the divine promise and enforcing the territorial claim. It acts as the primary agent of enforcement, translating theological claims into state policy and military action.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Interpret and propagate the theological basis for the territorial claim, guiding the community and influencing state policy. Their authority is derived from their role as interpreters of divine law.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_rabbis_and_leaders, agenda_setter,
    organized, generational, identity_locked, national).

% Observes and often critiques the conflict through a lens of international law and human rights, which frequently clashes with the theological claim. Their ability to influence is constrained by the perceived inalienability of the religious claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective identity and purpose of the Jewish people with the land of Eretz Yisrael, providing a theological framework for national aspiration and state-building.
% TRANSFER_FUNCTION: Transfers sovereignty, land, and resources from the Palestinian people to the Jewish people, justified by a divine covenant and historical connection.
% ABSENT_VOICES: The Palestinian people are structurally absent from the 'conversation' within this reading, as their claims to the land are deemed illegitimate or secondary to the divine mandate. Their perspective is excluded from the foundational premises.
% DISAPPEARANCE_RATIONALE: If the divine promise and its interpretation as an inalienable territorial claim vanished, the foundational legitimacy of the State of Israel, as understood by this reading, would collapse. This would necessitate a complete re-evaluation of borders, sovereignty, and the rights of all inhabitants, fundamentally reorganizing the region's political and social order.
% FOUNDING_PROBLEM: The historical exile, persecution, and statelessness of the Jewish people, coupled with the theological imperative to return to and settle the divinely promised land of Eretz Yisrael.
% FOUNDING_PROBLEM_CORROBORATION: The problem's status as 'live' is primarily attested by religious texts, traditions, and the ongoing self-perception of the Jewish people as a covenant community. External corroboration from non-religious or non-Zionist sources is contested, often framing the 'problem' as having been 'solved' by statehood, with the persistence of the claim now creating new problems for others.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because this reading asserts an absolute, non-negotiable claim over the entire territory, leading to the displacement and dispossession of the Palestinian people. Suppression is also very high (0.88) as the claim requires active, often coercive, enforcement against a native population whose existence and counter-claims are fundamentally delegitimized by the theological premise. Theater ratio is very low (0.05) because the claim is actively and earnestly pursued, with minimal performative maintenance; the theological mandate is a live, driving force. Resistance is high (0.85) due to the ongoing, violent conflict with the Palestinian people, who actively resist the imposition of this claim.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people (as covenant community), this is a righteous, divinely mandated act of return and reclamation, essential for their survival and identity. From the perspective of the Palestinian people, it is an act of dispossession and oppression, driven by a religious narrative that denies their existence and rights. The engine's classification as a Snare reflects the structural reality of high extraction and suppression from the victim's seat, regardless of the beneficiary's internal justification.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people, as the covenant community, are the primary beneficiaries, receiving the 'gift' of the land and the fulfillment of their national-religious identity. The Palestinian people are the clear victims, bearing the costs of dispossession and denial of self-determination. The State of Israel acts as the institutional agent enforcing this claim. The international community is an observer, often attempting to mediate or impose international law, but facing the intransigence of a divinely grounded claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_empirical_claim,
    'Is the claim to Eretz Yisrael primarily a theological truth, an empirical historical right, or a political construct?',
    'Conceptual analysis of the claim''s grounding, and empirical investigation into historical presence and legal precedent, acknowledging that no single resolution will satisfy all parties.',
    'If primarily theological, its resistance to political compromise is inherent. If empirical/political, it becomes subject to international law, historical counter-claims, and negotiation, potentially lowering its effective extractiveness and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_vs_empirical_claim, conceptual, 'Ambiguity in the grounding of the territorial claim.').

omega_variable(
    definition_of_jewish_people,
    'Does ''Jewish people'' refer to a religious community, an ethnic group, a nationality, or a combination, and how does this definition impact the territorial claim?',
    'Sociological and historical analysis of Jewish identity, and legal analysis of citizenship and immigration laws, alongside theological interpretations.',
    'A purely religious definition might allow for shared sovereignty with non-Jewish inhabitants, while an ethnic-national definition tends towards exclusive control and demographic majority, amplifying extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_jewish_people, conceptual, 'Ambiguity in the definition of the beneficiary group and its implications for the claim.').

omega_variable(
    kernel_reading_structural_delta,
    'How would the structural properties (beneficiaries, victims, extractiveness) of this constraint change if a sibling reading of the ''jewish_sovereignty_palestine'' kernel were adopted?',
    'Comparative analysis of the structural deltas between this reading and its siblings, as documented in their respective constraint stories.',
    'A ''liberal_nationalist_reading'' might allow for partition and shared sovereignty, reducing extraction. A ''settler_colonial_reading'' would reframe the beneficiaries and victims, but likely maintain high extraction. A ''post_zionist_reading'' would fundamentally challenge the legitimacy of the entire project, aiming for zero extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'This constraint is one reading of a contested kernel; its structural properties are contingent on the adopted reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1987, 0.06).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(jewi_tr_t2010, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1987, 0.88).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(jewi_be_t2010, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2010, 0.91).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1987, 0.85).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2000, 0.86).
narrative_ontology:measurement(jewi_su_t2010, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of multiple readings of the 'jewish_sovereignty_palestine' kernel. Each reading instantiates a distinct constraint with its own ε and structural properties, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
