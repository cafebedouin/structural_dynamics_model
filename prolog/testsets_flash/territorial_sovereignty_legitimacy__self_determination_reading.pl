% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Self-Determination Reading (Arab Population)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'self-determination' reading of
 *   territorial sovereignty legitimacy in the context of the
 *   Israeli-Palestinian conflict. It asserts that legitimacy derives from the
 *   modern principle of self-determination applied to the Arab population
 *   with demographic majority and continuous residence in the territory
 *   during the modern period (19th-20th centuries). This reading frames the
 *   Israeli state as a colonial project and supports the right of return as a
 *   restoration of the status quo ante. It is a highly extractive and
 *   suppressive constraint for the Israeli state and Jewish population, as it
 *   fundamentally denies their legitimacy and right to self-determination in
 *   the territory.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.9).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy: Self-Determination Reading (Arab Population)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, 'e7811b8d-07a8-4fc3-852d-3e8c99e36689').
narrative_ontology:cs_kernel_codification('e7811b8d-07a8-4fc3-852d-3e8c99e36689', distributed).
narrative_ontology:cs_authority_grounding('e7811b8d-07a8-4fc3-852d-3e8c99e36689', distributed).
narrative_ontology:cs_reading_relation('e7811b8d-07a8-4fc3-852d-3e8c99e36689', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e7811b8d-07a8-4fc3-852d-3e8c99e36689', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('e7811b8d-07a8-4fc3-852d-3e8c99e36689', foundational, modern_demographic_majority_confers_sovereignty).
narrative_ontology:cs_axiom_status(modern_demographic_majority_confers_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('e7811b8d-07a8-4fc3-852d-3e8c99e36689', modern_demographic_majority_confers_sovereignty, conventional).
narrative_ontology:cs_axiom('e7811b8d-07a8-4fc3-852d-3e8c99e36689', foundational, right_to_self_determination_is_universal).
narrative_ontology:cs_axiom_status(right_to_self_determination_is_universal, holdable).
narrative_ontology:cs_axiom_grounding('e7811b8d-07a8-4fc3-852d-3e8c99e36689', right_to_self_determination_is_universal, deontological).
narrative_ontology:cs_reference_frame('e7811b8d-07a8-4fc3-852d-3e8c99e36689', post_colonial_self_determination_era).
narrative_ontology:cs_drift_state('e7811b8d-07a8-4fc3-852d-3e8c99e36689', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e7811b8d-07a8-4fc3-852d-3e8c99e36689', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_states).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jewish_population_in_israel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts the legitimacy of Palestinian sovereignty based on the self-determination principle for the Arab population with continuous residence and demographic majority in the modern period. Frames the Israeli state as a colonial project and advocates for the right of return.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_national_movement, agenda_setter,
    organized, generational, identity_locked, regional).

% Support the self-determination reading as a matter of pan-Arab solidarity and international law, benefiting from the ideological coherence and regional influence it provides. Their support is often rhetorical but can involve diplomatic and financial pressure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_states, beneficiary,
    institutional, generational, constrained, regional).

% Is directly challenged by this reading, which denies its fundamental legitimacy and frames its existence as a colonial imposition. Bears the cost of continuous diplomatic, political, and sometimes military resistance stemming from this claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, payer,
    institutional, generational, trapped, national).

% Faces an existential threat under this reading, which implies their displacement or subjugation. Their identity and collective security are directly challenged, leading to high levels of internal cohesion and resistance to the claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_population_in_israel, payer,
    organized, generational, identity_locked, national).

% Interpret international law, particularly the principle of self-determination, to support the Palestinian claim to sovereignty based on modern demographic and historical factors. Their analysis provides intellectual grounding for the reading.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_law_scholars_pro_palestine, observer,
    analytical, generational, analytical, global).

% Argue against this reading, emphasizing historical Jewish ties, prior international mandates, and the right to self-determination for the Jewish people. Their arguments are often dismissed or marginalized within forums dominated by the self-determination reading.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_law_scholars_pro_israel, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, internationally recognized framework for the Palestinian national movement to articulate its claims to statehood and territorial rights, coordinating diplomatic efforts and resistance strategies.
% TRANSFER_FUNCTION: Transfers legitimacy and territorial claims from the Israeli state to the Palestinian national movement, implying a reversal of historical outcomes and a re-allocation of sovereign rights over the territory.
% ABSENT_VOICES: The Israeli state and its supporters, particularly those who emphasize ancient historical ties or the Jewish right to self-determination, are often excluded from the foundational premises of this reading, which frames their presence as illegitimate.
% DISAPPEARANCE_RATIONALE: If this reading of sovereignty legitimacy vanished, the Palestinian national movement would lose a core ideological and legal foundation for its claims, significantly altering the diplomatic landscape and the nature of the conflict. The international discourse on the conflict would fundamentally shift.
% FOUNDING_PROBLEM: The perceived denial of self-determination and statehood for the Arab population of Palestine following the collapse of the Ottoman Empire and subsequent British Mandate, culminating in the 1948 Nakba and ongoing occupation.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by the continuous displacement and statelessness of Palestinians, UN resolutions, and reports from numerous international human rights organizations. This corroboration comes from outside the direct beneficiaries (Palestinian national movement) and is widely recognized internationally, though its interpretation is contested.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading demands a fundamental re-ordering of sovereignty that would entail significant loss of territory, resources, and security for the Israeli state and its Jewish population. Suppression is also very high (0.9) as this reading actively seeks to delegitimize and dismantle the existing Israeli state structure, requiring active enforcement of its narrative and political pressure. Theater ratio is low (0.1) because the claims are direct and the political action taken is largely consistent with the stated goals; there is little performative maintenance masking a degraded function. Accessibility collapse is high (0.95) because, if fully adopted, this reading would leave virtually no legitimate alternative for the Israeli state's current form. Resistance is high (0.8) due to the existential nature of the challenge it poses to the Israeli state and Jewish population.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Palestinian national movement, this reading is a just and necessary framework for achieving self-determination. From the perspective of the Israeli state, it is an existential threat and a denial of their own right to self-determination. The engine's classification will reflect this deep divergence, likely showing a Snare for the Israeli seat and a Rope/Tangled Rope for the Palestinian seat, despite the overall constraint being highly extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian national movement and Arab states are beneficiaries (d near 0.0) as this reading provides the ideological and legal basis for their claims and diplomatic efforts. The Israeli state and the Jewish population in Israel are clear targets (d near 1.0) as their very existence and legitimacy are challenged by this reading. International law scholars who support this reading are observers, while those who oppose it are excluded from the discourse's foundational premises.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_period_definition,
    'Is the ''modern period'' (19th-20th centuries) the appropriate temporal scope for determining self-determination, or should earlier historical periods (e.g., ancient Jewish presence) also be considered?',
    'International consensus on the relevant historical scope for self-determination claims in post-colonial contexts, or a negotiated political settlement that explicitly defines the relevant historical period.',
    'If earlier periods are included, the legitimacy claims of the Jewish population would gain greater weight, potentially shifting the constraint towards a more balanced or even reversed beneficiary/victim structure. If the modern period is strictly maintained, the current extractive nature for the Israeli state is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_period_definition, conceptual, 'The contested temporal scope for self-determination claims.').

omega_variable(
    demographic_majority_vs_indigenous_rights,
    'Does ''demographic majority and continuous residence'' in the modern period sufficiently establish indigenous rights for self-determination, or do other forms of historical connection (e.g., religious, ancestral) also confer such rights?',
    'Development of international legal norms regarding indigenous rights and self-determination that explicitly address competing claims based on different forms of historical connection.',
    'If other forms of historical connection are recognized as conferring indigenous rights, the exclusive focus on modern demographic majority would be challenged, potentially weakening the constraint''s suppressive force on the Jewish population. If modern demographic majority remains the sole criterion, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_majority_vs_indigenous_rights, conceptual, 'The criteria for establishing indigenous rights and self-determination.').

omega_variable(
    colonial_project_framing_validity,
    'Is the framing of the Israeli state as a ''colonial project'' an accurate and universally accepted characterization, or is it a contested interpretation that serves specific political agendas?',
    'Historical and political science scholarship achieving broad consensus on the applicability of post-colonial theory to the founding and ongoing existence of the Israeli state, or a shift in international diplomatic language.',
    'If the colonial framing is widely rejected, the moral and legal force of this reading would diminish, reducing its extractiveness and suppression. If it becomes universally accepted, the constraint''s current classification would be further entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_project_framing_validity, empirical, 'The contested characterization of the Israeli state as a colonial project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1918, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1918, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1918, 0.05).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.09).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(terr_be_t1918, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1918, 0.6).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.75).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1918, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1918, 0.7).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.9).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.88).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'territorial_sovereignty_legitimacy' kernel. Each reading has a unique structural profile and set of beneficiaries/victims, leading to different classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
