% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Abrahamic Covenant Land Promise Territorial Legitimacy Constraint
 *   domain: religious/political/territorial
 *
 * SUMMARY:
 *   Genesis presents a covenant between the deity and Abraham that includes a
 *   territorial grant of Canaan. Over millennia, this has been read variously
 *   as conditional, fulfilled, or ongoing. The land_promise_constraint
 *   reading treats the grant as an eternal, unconditional material
 *   entitlement that legitimates modern Jewish state sovereignty and
 *   territorial expansion in the Levant. This reading is contested by sibling
 *   readings (Isaac lineage exclusivity vs. Ishmaelite inclusion) and by
 *   secular or civic-nationalist frames. The constraint operates as a
 *   high-extraction legitimacy structure: it transfers land and sovereignty
 *   from Palestinian Arab populations to Israeli state and settler
 *   institutions under the sanction of divine promise. Its persistence
 *   requires active suppression of alternative readings (conditional
 *   covenant, fulfilled covenant, Ishmaelite continuation) and of the
 *   Palestinian populations who physically occupy the promised territory.
 *
 * KEY AGENTS:
 *   - israeli_state_apparatus: Primary beneficiary/agenda_setter (institutional/generational/constrained) â leverages covenant reading for territorial policy and international legitimacy.
 *   - religious_zionist_institutions: Secondary beneficiary (organized/generational/identity_locked) â provides theological infrastructure and interpretive labor that maintains the reading.
 *   - palestinian_displaced_communities: Primary target (powerless/biographical/trapped) â bear the extraction through dispossession, displacement, and denial of return.
 *   - dissenting_jewish_theologians: Excluded voice (moderate/generational/constrained) â hold conditional or fulfilled readings but are marginalized in political theology.
 *   - international_human_rights_bodies: Analytical observer (institutional/generational/analytical) â monitors violations and challenges the legitimacy frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.82).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.78).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Abrahamic Covenant Land Promise Territorial Legitimacy Constraint").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/political/territorial").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '2279430b-0463-4e78-a5b3-1c141e5f7ab7').
narrative_ontology:cs_kernel_codification('2279430b-0463-4e78-a5b3-1c141e5f7ab7', fixed_text).
narrative_ontology:cs_authority_grounding('2279430b-0463-4e78-a5b3-1c141e5f7ab7', lineage).
narrative_ontology:cs_interpretation_layer_present('2279430b-0463-4e78-a5b3-1c141e5f7ab7').
narrative_ontology:cs_reading_relation('2279430b-0463-4e78-a5b3-1c141e5f7ab7', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('2279430b-0463-4e78-a5b3-1c141e5f7ab7', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_axiom('2279430b-0463-4e78-a5b3-1c141e5f7ab7', foundational, canaan_as_eternal_inheritance).
narrative_ontology:cs_axiom_status(canaan_as_eternal_inheritance, holdable).
narrative_ontology:cs_axiom_grounding('2279430b-0463-4e78-a5b3-1c141e5f7ab7', canaan_as_eternal_inheritance, theological).
narrative_ontology:cs_axiom('2279430b-0463-4e78-a5b3-1c141e5f7ab7', foundational, state_sovereignty_as_redemptive_instrument).
narrative_ontology:cs_axiom_status(state_sovereignty_as_redemptive_instrument, holdable).
narrative_ontology:cs_axiom_grounding('2279430b-0463-4e78-a5b3-1c141e5f7ab7', state_sovereignty_as_redemptive_instrument, theological).
narrative_ontology:cs_reference_frame('2279430b-0463-4e78-a5b3-1c141e5f7ab7', divine_grant_everlasting_possession).
narrative_ontology:cs_drift_state('2279430b-0463-4e78-a5b3-1c141e5f7ab7', contemporary_post_1967, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2279430b-0463-4e78-a5b3-1c141e5f7ab7', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_zionist_institutions).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_displaced_communities).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, divine_territorial_title).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, eternal_covenant_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers territorial expansion, settlement policy, and military occupation of the West Bank and Gaza. Uses the covenant reading to ground international diplomatic appeals and domestic legal frameworks for land annexation. Exit would require abandoning a core legitimacy narrative and reconstituting the state's foundational mythos, which is politically constrained but not identity-locked at the institutional level.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus, beneficiary).

% Provide theological scholarship, educational curricula, and rabbinic rulings that frame settlement as a religious commandment. Their institutional identity is fused with the land promise; exit would mean dissolving their theological and communal self-understanding.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_zionist_institutions, beneficiary,
    organized, generational, identity_locked, national).

% Bear the costs of the constraint through military occupation, land confiscation, movement restrictions, and denial of refugee return. Their physical presence on the land is treated as a temporary obstacle to covenant fulfillment. Exit options are structurally blocked by borders, statelessness, and international inaction.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_displaced_communities, payer,
    powerless, biographical, trapped, local).

% Maintain that the covenant is conditional, already fulfilled spiritually, or superseded, and therefore does not legitimate modern territorial expansion. They are marginalized in national political theology and media discourse, though they publish and organize within limited religious and academic channels.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, dissenting_jewish_theologians, excluded,
    moderate, generational, constrained, national).

% Monitor and report on occupation practices, settlement expansion, and displacement. They classify the territorial regime as violations of international law but lack enforcement power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claimed to coordinate the legitimate return, settlement, and sovereign governance of the Jewish people in their historic homeland by grounding territorial title in a transcendent divine covenant.
% TRANSFER_FUNCTION: Moves territorial control, habitation rights, and state sovereignty from Palestinian Arab populations to Israeli state institutions and settler movements under the sanction of a divine land grant.
% ABSENT_VOICES: Palestinian refugees and their descendants, non-Zionist Jewish theologians who read the covenant as conditional or spiritually fulfilled, and Islamic scholars advancing the Ishmaelite covenant reading are structurally excluded from the authoritative interpretation that grounds territorial policy.
% DISAPPEARANCE_RATIONALE: If the land-promise reading vanished overnight, the primary theological justification for exclusive Jewish territorial sovereignty over the contested land would collapse; diplomatic alignments, settlement policy, and international legal strategies would require re-grounding on civic-national rather than covenantal grounds.
% FOUNDING_PROBLEM: Diaspora and statelessness of the Jewish people following Roman expulsion and centuries of dispersal, compounded by 19th- and 20th-century nationalist movements seeking a territorial base for Jewish collective self-determination and security.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and state institutions assert the problem remains live due to ongoing insecurity and diaspora. Palestinian historians, critical Israeli scholars, and international law experts attest from outside the beneficiary set that the founding problem of Jewish statelessness is substantially resolved by the existing State of Israel, and that the covenant reading now operates to justify territorial expansion and population displacement rather than return and refuge.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint systematically transfers territory and sovereignty from a defined population to a state apparatus under a non-negotiable divine title claim. Suppression (0.78) is high because the constraint's persistence requires military occupation, legal discrimination, and diplomatic suppression of Palestinian self-determination and of rival covenant readings. Theater_ratio (0.45) reflects that a substantial fraction of religious justification is performative â state actors invoke covenant language primarily when it aligns with territorial expansion, not when it conflicts with security or economic interests. Accessibility_collapse (0.65) is substantial because alternative civic-national or binational frameworks are structurally marginalized by the hegemonic covenant narrative, though not fully eliminated. Resistance (0.70) is high due to persistent Palestinian popular and armed resistance, international BDS movements, and internal Israeli dissent.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (Israeli state and religious Zionist institutions) experiences the constraint as a sacred national mission and legitimate defense of historic rights. The payer seat (Palestinian communities) experiences it as dispossession and military domination. The excluded seat (dissenting theologians) experiences it as a heretical politicization of scripture. The engine computes this divergence from structural data: beneficiaries have institutional power and identity-locked exit; victims are powerless with trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state apparatus and religious Zionist institutions are declared beneficiaries (low d, subsidy and legitimacy flow toward them). Palestinian displaced communities are declared victims (high d, extraction flows from them). Dissenting Jewish theologians are excluded â their exit is constrained by communal pressure but not trapped; their directionality is mid-range because they are neither extracting nor extracted-upon by this specific constraint, though they suffer identity costs. The high spatial_scope for beneficiaries (national/global) versus local scope for victims amplifies effective extraction for victims because verification of their dispossession is buried under national security framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring explicit victim identification and high extraction metrics for snare. A naive reading might classify this as identity_coordination (religious community solidarity) or scaffold (transitional return). However, the founding problem of Jewish statelessness is substantially solved â the State of Israel exists â while the constraint persists to justify ongoing expansion and exclusion. The founding_problem_status=dead multiplied by disappearance_verdict=world_rearranges mismatch flags mandatrophy: the arrangement is a zombie structure whose real function is extraction, not return.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_promise_conditionality,
    'Is the Genesis land promise conditional on obedience (as read by conditional-covenant theologians) or an unconditional eternal grant (as read by religious nationalists)?',
    'Comparative theological analysis of Genesis 12, 15, and 17 alongside Deuteronomic conditional language; polling of rabbinical authorities on covenant conditionality.',
    'If conditional, the territorial claim is contingent on behavior rather than ontological, reducing extractiveness. If unconditional, extraction is structurally embedded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_promise_conditionality, conceptual, 'Conditionality of the covenant land promise').

omega_variable(
    kernel_territorial_lineage_dependency,
    'This constraint is the land_promise_constraint reading of kernel abrahamic_covenant. Does its territorial claim depend on the exclusive Isaac lineage reading, or can the land promise stand independently?',
    'Examine whether territorial-covenant theologians who reject exclusive lineage maintain the land promise, and whether Isaac-only readings without territorial emphasis remain stable.',
    'If lineage-dependent, isaac_covenant_reading is upstream in the constraint family. If independent, the family is parallel and the land promise has its own epsilon profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_territorial_lineage_dependency, conceptual, 'Structural location within the abrahamic_covenant kernel').

omega_variable(
    divine_legitimacy_vs_state_instrumentalization,
    'Is the covenant reading primarily a theological commitment held by believers, or a state legitimation strategy that instrumentalizes theology for territorial expansion?',
    'Historical tracing of the reading''s adoption in state policy; analysis of state actor behavior when covenant claims conflict with security or economic interests.',
    'If primarily state instrumentalization, the constraint is a snare using identity_coordination as cover. If primarily genuine theology, classification may shift toward lower extraction for believing communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_legitimacy_vs_state_instrumentalization, empirical, 'Theological sincerity versus political instrumentalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__land_promise_constraint, theater_ratio, 0, 0.15).
narrative_ontology:measurement(abra_tr_t20, abrahamic_covenant__land_promise_constraint, theater_ratio, 20, 0.25).
narrative_ontology:measurement(abra_tr_t50, abrahamic_covenant__land_promise_constraint, theater_ratio, 50, 0.35).
narrative_ontology:measurement(abra_tr_t70, abrahamic_covenant__land_promise_constraint, theater_ratio, 70, 0.42).
narrative_ontology:measurement(abra_tr_t85, abrahamic_covenant__land_promise_constraint, theater_ratio, 85, 0.45).
narrative_ontology:measurement(abra_tr_t100, abrahamic_covenant__land_promise_constraint, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__land_promise_constraint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(abra_be_t20, abrahamic_covenant__land_promise_constraint, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(abra_be_t50, abrahamic_covenant__land_promise_constraint, base_extractiveness, 50, 0.7).
narrative_ontology:measurement(abra_be_t70, abrahamic_covenant__land_promise_constraint, base_extractiveness, 70, 0.75).
narrative_ontology:measurement(abra_be_t85, abrahamic_covenant__land_promise_constraint, base_extractiveness, 85, 0.8).
narrative_ontology:measurement(abra_be_t100, abrahamic_covenant__land_promise_constraint, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__land_promise_constraint, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(abra_su_t20, abrahamic_covenant__land_promise_constraint, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(abra_su_t50, abrahamic_covenant__land_promise_constraint, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(abra_su_t70, abrahamic_covenant__land_promise_constraint, suppression_requirement, 70, 0.72).
narrative_ontology:measurement(abra_su_t85, abrahamic_covenant__land_promise_constraint, suppression_requirement, 85, 0.78).
narrative_ontology:measurement(abra_su_t100, abrahamic_covenant__land_promise_constraint, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% The abrahamic_covenant kernel decomposes into at least three structurally distinct constraints: isaac_covenant_reading (genealogical exclusivity), ishmael_covenant_reading (inclusive prophetic lineage), and land_promise_constraint (territorial materialization). This story models the territorial reading; its epsilon is substantially higher than lineage-only readings because it operates through state violence and displacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
