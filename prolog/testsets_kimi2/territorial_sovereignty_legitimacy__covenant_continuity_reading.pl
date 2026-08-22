% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Covenant-Continuity Reading
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the covenant-continuity READING of the
 *   contested territorial_sovereignty_legitimacy kernel. It asserts that
 *   Israeli sovereignty over the territory derives from an ancient divine
 *   covenant, continuous Jewish presence (however demographically thin), and
 *   modern international recognition (Balfour, UN Partition Plan, 1948
 *   establishment). This reading treats the temporal scope as extending to
 *   the biblical period, views partition as a compromise of a pre-existing
 *   right rather than the creation of a new one, and frames settlement as
 *   return rather than colonization. It is ONE reading among three in the
 *   kernel; the sibling readings (self_determination, existential_matrix) are
 *   structurally different constraints in separate files.
 *
 * KEY AGENTS:
 *   - Israeli state (agenda_setter/institutional): enforces the legitimacy framework and territorial allocation.
 *   - Jewish settler movement (beneficiary/organized): receives territorial and state support under the continuity narrative.
 *   - International recognizers (beneficiary/institutional): gain geopolitical stability from the recognized framework.
 *   - Palestinian Arab population (payer/powerless): bears extraction through occupation and dispossession.
 *   - Refugee descendants (payer/powerless): bear exclusion from return and statelessness.
 *   - Anti-Zionist Jewish dissidents (excluded/moderate): excluded from the covenantal discourse despite shared identity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.75).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.8).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy: Covenant-Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '47441f7a-6359-4fb0-991e-0bd36bcec8d3').
narrative_ontology:cs_kernel_codification('47441f7a-6359-4fb0-991e-0bd36bcec8d3', fixed_text).
narrative_ontology:cs_authority_grounding('47441f7a-6359-4fb0-991e-0bd36bcec8d3', lineage).
narrative_ontology:cs_interpretation_layer_present('47441f7a-6359-4fb0-991e-0bd36bcec8d3').
narrative_ontology:cs_reading_relation('47441f7a-6359-4fb0-991e-0bd36bcec8d3', territorial_sovereignty_legitimacy__self_determination_reading, influences).
narrative_ontology:cs_reading_relation('47441f7a-6359-4fb0-991e-0bd36bcec8d3', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('47441f7a-6359-4fb0-991e-0bd36bcec8d3', foundational, divine_covenant_territorial_mandate).
narrative_ontology:cs_axiom_status(divine_covenant_territorial_mandate, holdable).
narrative_ontology:cs_axiom_grounding('47441f7a-6359-4fb0-991e-0bd36bcec8d3', divine_covenant_territorial_mandate, theological).
narrative_ontology:cs_axiom('47441f7a-6359-4fb0-991e-0bd36bcec8d3', foundational, continuous_presence_preserves_sovereignty).
narrative_ontology:cs_axiom_status(continuous_presence_preserves_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('47441f7a-6359-4fb0-991e-0bd36bcec8d3', continuous_presence_preserves_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('47441f7a-6359-4fb0-991e-0bd36bcec8d3', covenantal_territorial_mandate).
narrative_ontology:cs_drift_state('47441f7a-6359-4fb0-991e-0bd36bcec8d3', contemporary_post_1967, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47441f7a-6359-4fb0-991e-0bd36bcec8d3', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_settler_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognizers).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, refugee_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the covenant-continuity sovereignty framework through state institutions, military occupation, settlement policy, and international diplomacy. Its own legitimacy is constituted by the framework; exit would require existential redefinition of the state.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, global).

% Receives state support for territorial expansion into areas framed as historical Jewish land. Their presence is treated as both evidence of continuous presence and fulfillment of the covenantal mandate.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_settler_movement, beneficiary,
    organized, generational, constrained, national).

% Collect geopolitical stability and a perceived moral resolution to Jewish statelessness by recognizing the covenant-continuity framework. They do not administer the constraint but benefit from the resulting alliance structure and regional order.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognizers, beneficiary,
    institutional, civilizational, analytical, global).

% Bears the cost of the legitimacy framework through dispossession, military occupation, settlement encroachment, and denial of self-determination. Their claims are structurally subordinated to the covenantal precedence narrative.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, regional).

% Bear the cost through exclusion from return and persistent statelessness. The continuity framework treats their demographic absence as natural discontinuity rather than as a consequence of expulsion, thereby negating their title.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, refugee_descendants, payer,
    powerless, generational, trapped, regional).

% Would object to the exclusivist covenantal interpretation from within Jewish tradition but are marginalized in mainstream Zionist discourse. Their Jewish identity is invoked to delegitimize their dissent.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, anti_zionist_jewish_dissidents, excluded,
    moderate, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified juridical-theological framework that coordinates Jewish diaspora support, state institutions, and international recognition into a single territorial sovereignty claim, solving the collective-action problem of sustaining statehood without a conventional demographic majority at founding.
% TRANSFER_FUNCTION: Moves territorial control, legal priority, and state resources from the Palestinian Arab population and refugee descendants to the Israeli state and settler society, justified by covenantal precedence and historical continuity.
% ABSENT_VOICES: Palestinian refugee descendants and anti-Zionist Jewish dissidents are structurally excluded; their counter-claims of indigenous modern majority status, expulsion, and democratic binational alternatives are delegitimized within the covenantal framework.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy framework vanished, the Israeli state would lose its primary theological-historical justification, diaspora-state relations would destabilize, and the international legal basis would shift toward pure demographic self-determination principles, fundamentally rearranging territorial claims and the regional state system.
% FOUNDING_PROBLEM: Jewish statelessness, diaspora vulnerability, and the need for a secure national homeland after millennia of dispersion and twentieth-century genocide.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiographers and Israeli state institutions attest the problem is live, citing persistent antisemitism. Palestinian historians and post-Zionist scholars attest the founding problem was resolved by state establishment and the framework now perpetuates domination; international human rights organizations outside the benefiting parties document ongoing structural subordination rather than vulnerability.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.75) is high because the framework transfers territorial control and self-determination from one population to another on the basis of a contested historical-theological claim. Suppression (0.8) is higher still because the constraint's persistence depends on actively excluding Palestinian return, suppressing alternative sovereignty claims, and maintaining military occupation. Theater ratio (0.52) is moderate-high: the framework maintains substantial performative democratic and liberal state imagery while operating an ethnocratic territorial logic. Accessibility collapse (0.72) is high because alternatives (binational democracy, full refugee return, equal citizenship) are largely excluded from mainstream policy and legal discourse. Resistance (0.78) is high due to persistent Palestinian armed and civil resistance, international solidarity movements, and repeated uprisings. The temporal measurements show monotonic extraction accumulation as the settlement project expanded and international law increasingly questioned the covenantal claim.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this constraint as the legitimate restoration of a historical right and a necessary response to genocide. The payer seats experience it as an enforced denial of self-determination and ongoing dispossession. The engine computes this divergence from the structural data: the state and settler movement carry low directionality (subsidized by the framework) while the Palestinian population and refugees carry high directionality (extracted from). The excluded Jewish dissident seat occupies a high-identity-lock position where exit from the framework is cognitively costly because the constraint fuses Jewish identity with Zionist territorialism.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (israeli_state, jewish_settler_movement, international_recognizers) are structurally subsidized by the constraint: their directionality is near the beneficiary end. Victims (palestinian_arab_population, refugee_descendants) are structural targets: their directionality is near the full-target end. The international_recognizers sit at analytical/global scope with low extraction despite beneficiary status because they do not experience the territorial transfer directly. The refugee_descendants sit at regional scope but with trapped exit, amplifying their effective extraction. No directionality overrides are needed because the structural derivation (beneficiary/victim + exit) captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy guard prevents misreading this constraint as pure coordination (a Rope providing Jewish national self-determination) or pure extraction (a Snare with no coordinating function). The Tangled Rope classification is structurally appropriate because there is a genuine coordination problem solved â Jewish statelessness and diaspora vulnerability were real historical problems that the framework addressed â AND identifiable asymmetric extraction â the Palestinian population pays the cost. The founding problem (statelessness) is contested: live for the beneficiaries, dead for the payers. This mismatch is exactly the signal the R5 genealogy interview is designed to surface. The constraint has not undergone mandatrophy resolution; it persists with its founding problem contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_historicity_ambiguity,
    'Is the ancient covenant a historically verifiable title-deed or a theological-mythological framework whose force is confined to believers?',
    'Archaeological and textual-critical assessment of Iron Age Levantine polities combined with sociological study of how secular international law incorporates theological claims.',
    'If purely theological, the legitimacy claim is confined to religious discourse and cannot ground secular state borders; if treated as historical title, it functions as a naturalized property right overriding modern self-determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_historicity_ambiguity, empirical, 'Whether the covenant is historical event or theological narrative').

omega_variable(
    demographic_absence_continuity,
    'Does the demographic absence of a Jewish majority between 70 CE and the nineteenth century break the continuity chain, or does diasporic memory and minority presence suffice?',
    'Historical-demographic analysis of population continuity versus historiographical construction of national memory; comparative assessment of similar absence-and-return claims in other territorial disputes.',
    'If absence breaks continuity, the modern Jewish presence is a new settlement movement rather than a return, fundamentally altering the directionality of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_absence_continuity, conceptual, 'Whether demographic absence voids or preserves territorial title').

omega_variable(
    international_recognition_scope,
    'Does the Balfour Declaration and UN Partition Plan validate sovereignty only within the 1948 borders, or do they ratify the entire covenantal land claim?',
    'Legal-historical analysis of the Mandate and Partition Plan language; state practice and UN resolution corpus regarding settlements beyond the Green Line.',
    'If recognition was limited to partition borders, the post-1967 settlement project is expansion beyond the internationally recognized constraint; if recognition ratified the entire claim, the constraint extracts over a wider scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_recognition_scope, conceptual, 'Scope of international recognition relative to territorial claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Palestinian resistance and self-determination claims structural (military, legal, economic barriers) or internalized (political docility produced by decades of fragmentation and hopelessness)?',
    'Post-exit suppression trajectory: if resistance resurges immediately when structural barriers relax, suppression was primarily structural; if fragmentation persists, internalization is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target population carries the suppression even when external barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(terr_tr_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(terr_tr_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(terr_tr_t38, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 38, 0.37).
narrative_ontology:measurement(terr_tr_t51, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 51, 0.43).
narrative_ontology:measurement(terr_tr_t64, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 64, 0.48).
narrative_ontology:measurement(terr_tr_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 76, 0.52).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(terr_be_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(terr_be_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(terr_be_t38, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 38, 0.55).
narrative_ontology:measurement(terr_be_t51, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 51, 0.62).
narrative_ontology:measurement(terr_be_t64, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 64, 0.69).
narrative_ontology:measurement(terr_be_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 76, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(terr_su_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(terr_su_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(terr_su_t38, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 38, 0.6).
narrative_ontology:measurement(terr_su_t51, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 51, 0.68).
narrative_ontology:measurement(terr_su_t64, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 64, 0.75).
narrative_ontology:measurement(terr_su_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 76, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
