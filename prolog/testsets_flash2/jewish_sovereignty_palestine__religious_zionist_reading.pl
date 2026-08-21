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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael (Religious Zionist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the Religious Zionist reading of Jewish
 *   sovereignty in Palestine, where the divine promise of Eretz Yisrael to
 *   the Jewish people grounds an inalienable territorial claim, and statehood
 *   is seen as theological fulfillment. This reading prioritizes maximalist
 *   territorial claims and views any compromise as a betrayal of divine will.
 *   Palestinians are largely absent from the calculus of rights or are
 *   subordinated. This is one reading of the 'jewish_sovereignty_palestine'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.95).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.9).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '2fd8fdd0-781d-4dd1-ba38-38b47a9d902e').
narrative_ontology:cs_kernel_codification('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', fixed_text).
narrative_ontology:cs_authority_grounding('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', lineage).
narrative_ontology:cs_interpretation_layer_present('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e').
narrative_ontology:cs_reading_relation('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_axiom('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', foundational, divine_mandate_inalienable_territory).
narrative_ontology:cs_axiom_status(divine_mandate_inalienable_territory, holdable).
narrative_ontology:cs_axiom_grounding('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', divine_mandate_inalienable_territory, theological).
narrative_ontology:cs_axiom('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', foundational, statehood_as_theological_fulfillment).
narrative_ontology:cs_axiom_status(statehood_as_theological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', statehood_as_theological_fulfillment, theological).
narrative_ontology:cs_reference_frame('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', biblical_covenant_and_redemption).
narrative_ontology:cs_drift_state('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', contemporary_political_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('2fd8fdd0-781d-4dd1-ba38-38b47a9d902e', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, international_law_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the fulfillment of a divine promise, grounding an inalienable territorial claim to Eretz Yisrael. This claim is central to their collective identity and theological purpose, making any territorial compromise an existential and spiritual threat.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, beneficiary,
    institutional, civilizational, identity_locked, universal).

% Acts as the political embodiment and enforcer of the divine promise, asserting sovereignty over the entire land. Its policies, including settlement expansion and control over Palestinian territories, are justified as steps towards theological fulfillment. Exit from this framework would mean abandoning its foundational religious-national identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel, agenda_setter,
    institutional, generational, constrained, national).

% Bear the direct costs of this claim, including displacement, loss of land, restricted movement, and denial of self-determination. Their historical presence and claims to the land are subordinated or negated by the divine mandate. Exit is structurally foreclosed by military and political control.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_people, payer,
    powerless, generational, trapped, regional).

% Its principles of self-determination, non-acquisition of territory by force, and human rights are often challenged or disregarded by this reading's territorial claims. While it attempts to mediate, its authority is frequently rejected as irrelevant or hostile to the divine mandate.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_law_framework, excluded,
    institutional, civilizational, constrained, global).

% While supporting Jewish self-determination, they often find the maximalist territorial claims and theological grounding of this reading problematic, as it complicates international legitimacy and democratic values. They seek a more pragmatic, less religiously absolute path to state security and recognition.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, liberal_nationalist_zionists, observer,
    powerful, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, state_of_israel).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective action and identity of the Jewish people around a shared theological narrative of return and territorial redemption, providing a powerful unifying purpose for state-building and settlement.
% TRANSFER_FUNCTION: Transfers land, sovereignty, and political control from the Palestinian people to the Jewish people and the State of Israel, justified by a divine mandate. It also transfers moral authority from secular international norms to religious doctrine.
% ABSENT_VOICES: The Palestinian people are structurally absent from the decision-making process regarding their land and future, their claims dismissed as illegitimate in the face of divine decree. Secular international bodies and human rights organizations are also often excluded or their authority rejected.
% DISAPPEARANCE_RATIONALE: If the divine promise as an inalienable territorial claim vanished, the foundational justification for Israeli sovereignty over disputed territories would collapse. This would necessitate a radical re-evaluation of borders, settlement policy, and the rights of Palestinians, leading to a profound political and social rearrangement of the region.
% FOUNDING_PROBLEM: The historical dispersion and persecution of the Jewish people, culminating in the Holocaust, necessitated a secure homeland where Jewish self-determination could be fully realized, rooted in ancestral ties to Eretz Yisrael.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist leaders and their followers attest that the problem of Jewish insecurity and the need for a divinely mandated homeland remains live, citing ongoing antisemitism and regional threats. This is corroborated by historical narratives of persecution, though the specific territorial implications are contested by external observers and other Zionist readings.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.95) because this reading asserts an exclusive, divinely mandated claim over the entire territory, leading to significant displacement and dispossession of the existing Palestinian population. Suppression is also very high (0.9) as the claim requires active military and political enforcement to maintain control and suppress Palestinian resistance or alternative claims. Theater ratio is low (0.1) because the theological justification is genuinely held and directly drives policy, with little performative cover for other motives. Accessibility collapse is high (0.9) for Palestinians, as their alternatives for self-determination within the claimed territory are almost entirely foreclosed. Resistance is high (0.85) due to ongoing Palestinian struggle against the occupation and dispossession.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people as a covenant community, this is a righteous fulfillment of a divine promise, a 'mountain' of theological truth. From the perspective of the Palestinian people, it is a 'snare' of dispossession and occupation. The engine's classification will reflect the latter due to the high extractiveness and suppression, despite the internal 'mountain' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people as a covenant community and the State of Israel are full beneficiaries (d near 0.0), as they receive the land and sovereignty. The Palestinian people are full targets (d near 1.0), bearing the costs of dispossession and denial of self-determination. The international law framework is excluded, as its principles are often rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_empirical_status,
    'Is the divine promise of Eretz Yisrael an empirically verifiable or universally accepted mandate, or a theological claim specific to a particular faith tradition?',
    'Conceptual analysis of theological claims vs. empirical evidence; assessment of universal vs. particularistic moral authority.',
    'If a particularistic theological claim, its universal political application becomes a preference-based constraint rather than a natural law, significantly increasing its measured extractiveness and suppression for non-adherents. If universally accepted, its ''mountain'' claim would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_empirical_status, conceptual, 'The epistemic status of the divine mandate claim.').

omega_variable(
    territorial_maximalism_necessity,
    'Is the maximalist territorial claim (no partition) a necessary consequence of the divine promise, or an interpretation that could be reconciled with alternative political arrangements?',
    'Theological re-interpretation within the tradition; comparative analysis of religious texts and their political applications.',
    'If not a necessary consequence, the constraint''s extractiveness could be reduced by allowing for political compromise and shared sovereignty, potentially shifting its classification towards a ''tangled_rope'' or even ''rope'' if extraction is reduced and coordination with Palestinians is achieved. If necessary, the high extractiveness is inherent to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_necessity, preference, 'Whether territorial maximalism is an inherent or interpretive aspect of the divine promise.').

omega_variable(
    palestinian_rights_reconciliation,
    'Can the divine promise be reconciled with the self-determination and human rights of the Palestinian people within the same territorial framework?',
    'Development of a shared political theology or legal framework that integrates both claims; empirical observation of successful power-sharing models.',
    'If reconciliation is possible, the constraint''s suppression and extractiveness would decrease, potentially leading to a reclassification towards a ''rope'' or ''scaffold'' if a transitional, equitable solution is implemented. If irreconcilable, the constraint remains highly extractive and suppressive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(palestinian_rights_reconciliation, empirical, 'Reconciliation of divine promise with Palestinian rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(jewi_tr_t2014, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2014, 0.1).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.85).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2005, 0.9).
narrative_ontology:measurement(jewi_be_t2014, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2014, 0.93).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(jewi_su_t2014, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2014, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
