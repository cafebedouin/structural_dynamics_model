% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Authorization
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the congressional primacy reading of the war
 *   powers allocation kernel: the claim that Article I of the Constitution
 *   requires explicit legislative authorization for military force beyond
 *   immediate self-defense. Under this reading, the legislative branch is the
 *   intended beneficiary of the constitutional allocation, but operational
 *   reality since the mid-twentieth century has seen systematic executive
 *   bypass, rendering Congress the victim of extracted constitutional
 *   authority. The constraint carries high suppression of competing inherent
 *   executive authority claims, yet enforcement capacity has decayed over the
 *   measured interval, producing substantial theater in the form of
 *   formalistic authorizations (AUMF) that delegate expansive discretion back
 *   to the executive. The high theater ratio and rising extractiveness paired
 *   with falling suppression requirement indicate a tangled rope undergoing
 *   institutional drift toward snare-like or piton-like operation.
 *
 * KEY AGENTS:
 *   - congress: Institutional payer â constitutional war power holder, routinely bypassed (institutional/constrained)
 *   - executive_branch: Agenda setter â administers national security, frequently bypasses authorization requirement (institutional/constrained)
 *   - citizenry: Beneficiary â receives democratic coordination through representative deliberation (organized/constrained)
 *   - federal_judiciary: Observer â adjudicates but generally avoids political questions (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.72).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.88).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Authorization").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '5c67ee41-fb9c-43a4-8be0-b7abc440caef').
narrative_ontology:cs_kernel_codification('5c67ee41-fb9c-43a4-8be0-b7abc440caef', fixed_text).
narrative_ontology:cs_authority_grounding('5c67ee41-fb9c-43a4-8be0-b7abc440caef', lineage).
narrative_ontology:cs_interpretation_layer_present('5c67ee41-fb9c-43a4-8be0-b7abc440caef').
narrative_ontology:cs_reading_relation('5c67ee41-fb9c-43a4-8be0-b7abc440caef', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('5c67ee41-fb9c-43a4-8be0-b7abc440caef', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('5c67ee41-fb9c-43a4-8be0-b7abc440caef', foundational, declare_war_clause_exclusive).
narrative_ontology:cs_axiom_status(declare_war_clause_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('5c67ee41-fb9c-43a4-8be0-b7abc440caef', declare_war_clause_exclusive, conventional).
narrative_ontology:cs_axiom('5c67ee41-fb9c-43a4-8be0-b7abc440caef', secondary, anti_unilateralism_principle).
narrative_ontology:cs_axiom_status(anti_unilateralism_principle, holdable).
narrative_ontology:cs_axiom_grounding('5c67ee41-fb9c-43a4-8be0-b7abc440caef', anti_unilateralism_principle, deontological).
narrative_ontology:cs_reference_frame('5c67ee41-fb9c-43a4-8be0-b7abc440caef', constitutional_textual_war_powers).
narrative_ontology:cs_drift_state('5c67ee41-fb9c-43a4-8be0-b7abc440caef', contemporary_security_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c67ee41-fb9c-43a4-8be0-b7abc440caef', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, citizenry).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, congress).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, declare_war_clause_supremacy).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, legislative_supremacy_in_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the constitutional authority to declare war and authorize military force, but routinely sees this power bypassed by unilateral executive action. When the constraint is respected, Congress deliberates and authorizes; when bypassed, its constitutional power is extracted by the executive branch. Exit is constrained by the constitutional framework itself â Congress cannot exit the separation of powers system.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress, payer,
    institutional, generational, constrained, national).

% Administers national security and decides whether to seek congressional authorization before military action. Frequently bypasses the authorization requirement by asserting inherent or implicit authority, extracting war power from the legislative branch. Structurally constrained by constitutional discourse but claims operational flexibility in security matters.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, national).

% Benefits from the coordination function of democratic deliberation over military commitments, receiving representative accountability. Cannot directly authorize wars outside the legislative framework; dependent on Congress to assert its constitutional role.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, citizenry, beneficiary,
    organized, generational, constrained, national).

% Adjudicates disputes over war powers allocation but generally avoids intervening in political questions between the political branches. Provides interpretive guidance that can either reinforce or undermine the congressional primacy reading.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures democratic deliberation and representative accountability before sustained military commitments, coordinating the relationship between the electorate, legislature, and executive by requiring explicit legislative approval for war.
% TRANSFER_FUNCTION: Moves the constitutional authority to initiate military force from the executive to Congress; when bypassed, effective war-making power is transferred from the legislative branch to the executive, extracting congressional constitutional authority.
% ABSENT_VOICES: Unitary executive theorists and advocates of inherent presidential authority are structurally marginalized in this reading; military commanders seeking operational flexibility and immediate response capability are sidelined in constitutional discourse.
% DISAPPEARANCE_RATIONALE: If the requirement of congressional authorization disappeared, the executive would possess unchecked war initiation authority, eliminating the legislative check and fundamentally altering the separation of powers architecture.
% FOUNDING_PROBLEM: Prevention of unilateral executive war-making and ensuring military force reflects popular will through representative deliberation, addressing the Framers' rejection of monarchical war powers.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and textualist legal scholars attest to the original allocation of war powers to Congress; however, national security legal advisors and executive branch practitioners dispute that this allocation remains viable in the modern security environment, and no corroborating consensus exists outside the interpretive communities aligned with this reading.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because executive bypass has become routine, extracting war power from Congress in practice despite the constitutional text. Suppression (0.88) is high because this reading must actively suppress inherent executive authority claims to maintain its structural integrity; the legal and political discourse constantly faces executive pushback. Theater ratio (0.45) reflects substantial performative maintenance â congressional authorizations often take the form of broad AUMFs that functionally delegate war power back to the executive, creating theater of legislative control without operational constraint. Accessibility collapse (0.78) is high because once the congressional primacy reading is accepted, executive unilateralism becomes constitutionally illegitimate and alternatives collapse within the interpretive framework. Resistance (0.70) is high because the executive branch and its legal apparatus actively resist the constraint through inherent authority claims and operational bypass. The temporal measurements show rising extraction paired with declining enforcement (falling suppression_requirement), the classic signature of a coordination mechanism being captured by the agenda setter.
 *
 * PERSPECTIVAL GAP:
 *   The citizenry seat and the congressional seat compute differently: from the citizenry's perspective, the constraint provides genuine democratic coordination (low d, low Ï). From Congress's perspective, the constraint formally allocates power but operationally enables executive extraction (high d, high Ï). The executive branch seat is ambiguous â it is the agenda setter that both maintains and bypasses the constraint â and would compute as near-symmetric or moderately targeted depending on whether the constraint's formal limits or its operational bypass dominates. The engine captures this divergence from the structural data: citizenry is declared beneficiary, Congress is declared victim, and the executive sits in agenda_setter without directional subsidy.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizenry is declared beneficiary because the coordination function (democratic deliberation) flows to the public. Congress is declared victim because executive bypass extracts constitutional war power from the legislative branch. The executive branch is not declared in either array because its relationship is asymmetric: it benefits from violating the constraint, not from the constraint's legitimate operation. Federal judiciary sits as analytical observer with no directional stake. The derived directionality places citizenry near the beneficiary pole, Congress near the target pole, and the executive at its power-atom fallback.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination function and asymmetric extraction for tangled rope classification. A pure rope reading would ignore the systematic executive bypass and Congress's victimization; a pure snare reading would ignore the genuine coordination benefit to democratic accountability. The temporal data â rising base_extractiveness paired with falling suppression_requirement â captures the drift from coordination toward extraction without collapsing the two into one category. The founding problem (preventing unilateral executive war) is contested in status, which flags the mandatrophy risk: the constraint may persist as theater after its enforcement function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_textual_fixity,
    'Does the congressional war power represent a fixed textual feature of the Constitution, or is it a constructed interpretation contingent on evolving institutional practice?',
    'Historical-textual analysis of founding-era understandings versus sociological institutionalism tracking how war powers practice has shifted across administrations.',
    'If fixed textual feature, the reading approaches mountain-like stability; if constructed and contingent, it is vulnerable to functional accommodation readings and institutional drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_textual_fixity, conceptual, 'Whether the war powers allocation is a natural-law-style constitutional fixture or a constructed norm.').

omega_variable(
    bypass_mechanism_prevalence,
    'What proportion of significant military deployments since 1945 have proceeded without explicit congressional authorization, and does this constitute systematic extraction or exceptional deviation?',
    'Comprehensive empirical inventory of military actions cross-referenced with congressional authorization records, distinguishing formal declarations from statutory authorizations and tacit acquiescence.',
    'If bypass is near-universal, the constraint''s coordination function is largely theater and the type shifts toward piton or snare; if authorization remains the operational norm, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bypass_mechanism_prevalence, empirical, 'Empirical prevalence of executive bypass of congressional war powers.').

omega_variable(
    suppression_enforcement_decay,
    'Does the decline in congressional enforcement of war powers authority represent institutional atrophy or a functional transformation toward tacit executive supremacy?',
    'Comparative analysis of congressional war powers assertions across historical periods, funding leverage usage, and War Powers Resolution compliance disputes.',
    'If atrophy, the constraint is degrading toward piton; if functional transformation, the constitutional order itself has shifted to a different kernel reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_enforcement_decay, empirical, 'Whether declining enforcement is decay or transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_powers_cp_tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(war_powers_cp_tr_t15, war_powers_allocation__congressional_primacy_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(war_powers_cp_tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(war_powers_cp_tr_t45, war_powers_allocation__congressional_primacy_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(war_powers_cp_tr_t60, war_powers_allocation__congressional_primacy_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(war_powers_cp_tr_t75, war_powers_allocation__congressional_primacy_reading, theater_ratio, 75, 0.5).

% Extraction over time
narrative_ontology:measurement(war_powers_cp_be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(war_powers_cp_be_t15, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(war_powers_cp_be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(war_powers_cp_be_t45, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(war_powers_cp_be_t60, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 60, 0.78).
narrative_ontology:measurement(war_powers_cp_be_t75, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(war_powers_cp_su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(war_powers_cp_su_t15, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(war_powers_cp_su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(war_powers_cp_su_t45, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(war_powers_cp_su_t60, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(war_powers_cp_su_t75, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 75, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__congressional_primacy_reading, 0.1).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% The war_powers_allocation kernel decomposes into three structurally distinct constraint readings: congressional_primacy (this file), which asserts constitutional necessity of legislative authorization; inherent_executive, which asserts presidential inherent authority; and functional_accommodation, which adopts a context-dependent approach. Each reading carries a distinct Îµ, beneficiary/victim structure, and classification. This reading claims high extraction via executive bypass and high suppression of inherent authority claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
