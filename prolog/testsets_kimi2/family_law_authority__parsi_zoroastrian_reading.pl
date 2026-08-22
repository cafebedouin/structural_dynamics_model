% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage as Community-Preserving Institution
 *   domain: religious governance/comparative family law
 *
 * SUMMARY:
 *   This constraint instantiates the parsi_zoroastrian_reading of the
 *   family_law_authority kernel: marriage is treated not as an individual
 *   contract but as a community-preserving institution governed by
 *   Zoroastrian religious law. Its distinctive structural delta is the
 *   endogamy requirement (loss of community status and religious burial for
 *   intermarriage), the authority of hereditary priests over ritual validity,
 *   and the small-community preservation logic that frames these restrictions
 *   as demographic survival. The constraint is actively enforced through
 *   personal law recognition, community trust rules, and priestly gatekeeping
 *   of funeral rites.
 *
 * KEY AGENTS:
 *   - Parsi priestly authority: agenda_setter and beneficiary (institutional/identity_locked) â controls ritual validity and enforces the endogamy boundary.
 *   - Endogamous community members: beneficiaries (moderate/identity_locked) â receive full religious recognition and communal trust access.
 *   - Intermarriage-seeking members: payers (moderate/identity_locked) â bear ostracism, loss of burial rights, and disinheritance.
 *   - Non-Parsi partners: excluded (powerless/trapped) â invisible to the religious law framework.
 *   - Indian judiciary: observer (institutional/analytical) â adjudicates tensions between personal law and constitutional rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.78).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.8).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage as Community-Preserving Institution").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious governance/comparative family law").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'f8decd76-1c48-43bb-b5d4-3eb3168f266d').
narrative_ontology:cs_kernel_codification('f8decd76-1c48-43bb-b5d4-3eb3168f266d', fixed_text).
narrative_ontology:cs_authority_grounding('f8decd76-1c48-43bb-b5d4-3eb3168f266d', lineage).
narrative_ontology:cs_interpretation_layer_present('f8decd76-1c48-43bb-b5d4-3eb3168f266d').
narrative_ontology:cs_reading_relation('f8decd76-1c48-43bb-b5d4-3eb3168f266d', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8decd76-1c48-43bb-b5d4-3eb3168f266d', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8decd76-1c48-43bb-b5d4-3eb3168f266d', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('f8decd76-1c48-43bb-b5d4-3eb3168f266d', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('f8decd76-1c48-43bb-b5d4-3eb3168f266d', foundational, endogamy_as_religious_imperative).
narrative_ontology:cs_axiom_status(endogamy_as_religious_imperative, holdable).
narrative_ontology:cs_axiom_grounding('f8decd76-1c48-43bb-b5d4-3eb3168f266d', endogamy_as_religious_imperative, theological).
narrative_ontology:cs_axiom('f8decd76-1c48-43bb-b5d4-3eb3168f266d', foundational, priestly_ritual_authority_over_validity).
narrative_ontology:cs_axiom_status(priestly_ritual_authority_over_validity, holdable).
narrative_ontology:cs_axiom_grounding('f8decd76-1c48-43bb-b5d4-3eb3168f266d', priestly_ritual_authority_over_validity, theological).
narrative_ontology:cs_reference_frame('f8decd76-1c48-43bb-b5d4-3eb3168f266d', zoroastrian_community_continuity_framework).
narrative_ontology:cs_drift_state('f8decd76-1c48-43bb-b5d4-3eb3168f266d', contemporary_indian_secular_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f8decd76-1c48-43bb-b5d4-3eb3168f266d', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarriage_seeking_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary priests who officiate Zoroastrian weddings and determine ritual validity. They administer the endogamy rule by refusing religious rites to Parsis marrying outside the faith, and they control access to religious burial and community trust benefits. Their standing depends on maintaining the boundary between Parsi and non-Parsi.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority, beneficiary).

% Parsi individuals who marry within the community and receive full religious and social recognition, including ritual burial rights, access to community trusts, and inheritance under Parsi personal law. Their children are accepted as Parsi.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members, beneficiary,
    moderate, biographical, identity_locked, national).

% Parsi individuals who wish to marry non-Zoroastrians. They face loss of community membership, exclusion from religious burial, disinheritance from communal trusts, and social ostracism if they proceed. Some marry under the Special Marriage Act but forfeit religious standing.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarriage_seeking_members, payer,
    moderate, biographical, identity_locked, national).

% Non-Zoroastrian individuals who form romantic partnerships with Parsis. They cannot be married in a Zoroastrian religious ceremony and are invisible to Parsi personal law; their children are not recognized as Parsi regardless of upbringing.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_parsi_partners, excluded,
    powerless, immediate, trapped, national).

% Courts that adjudicate disputes over Parsi personal law, including cases about the definition of Parsi for burial rights and trust entitlements. They operate under a constitutional regime that recognizes personal law but also guarantees fundamental rights.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_priestly_authority).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a tiny religious minority across generations by enforcing a demographic boundary through ritual control of marriage, preventing assimilation into the majority population.
% TRANSFER_FUNCTION: Moves religious legitimacy, burial rights, trust access, and community status from exogamous couples and their children to the priestly authority and the endogamous community core.
% ABSENT_VOICES: Intermarried Parsis and their children who have been denied funeral rights or trust benefits; secular feminists advocating individual autonomy over communal identity; non-Parsi partners who are structurally invisible to the religious law framework.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the priestly monopoly over marital validity would collapse, intermarriage would no longer trigger expulsion, the demographic boundary of the Parsi community would dissolve into the larger Indian population, and community trust inheritance rules would require renegotiation.
% FOUNDING_PROBLEM: Preservation of a tiny, endangered religious minority against demographic extinction and assimilation following Islamic conquest, colonial dispersal, and post-independence emigration.
% FOUNDING_PROBLEM_CORROBORATION: Parsi communal trusts and priestly authorities attest the problem is live, citing declining birth rates and emigration. Secular sociologists, demographic researchers, and excommunicated Parsis attest the problem is real but argue the endogamy constraint accelerates decline by shrinking the marriage pool; Indian court records and independent demographic studies from outside the beneficiary set support the counterproductive-reading.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.78 over the interval because the marriage pool shrinks as the population declines, making the cost of the endogamy rule increasingly severe for those who fall in love outside the community. Suppression rises from 0.50 to 0.80 as enforcement hardens around ritual burial and trust access â the state-backed personal law system and community shunning reinforce each other. Theater_ratio climbs from 0.20 to 0.55 because the demographic preservation rationale becomes less credible as the community continues to shrink despite enforcement, shifting the function toward maintaining priestly authority and ethnic purity. Accessibility_collapse is 0.60 because secular legal alternatives exist (Special Marriage Act) but community status collapses completely if they are used. Resistance is 0.45 because legal challenges occur but identity-lock keeps internal opposition fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The priestly authority and endogamous members experience this constraint as necessary survival infrastructure for a endangered minority; the intermarriage-seeking members experience it as a forced choice between love and identity. The engine computes this divergence from the structural asymmetry in exit options (both sides are identity_locked, but one side bears costs and the other collects status) and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The priestly authority is a structural beneficiary (d near 0.0) because it collects ritual monopoly and community gatekeeping power. Endogamous members are beneficiaries (low d) because the constraint subsidizes their social standing and trust access. Intermarriage-seeking members are targets (high d) because the constraint extracts community membership from them. Non-Parsi partners are excluded targets (high d, trapped) because they are entirely outside the benefit structure. The Indian judiciary sits as an analytical observer with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â demographic preservation of a endangered minority â was and may still be genuine, making the coordination component real. However, the temporal measurements show extraction and theater rising while the population continues to decline, suggesting the constraint has drifted from preservation toward ritual authority maintenance. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags a potential mandatrophy: the arrangement persists and rearranges the world if removed, but its original function may be dead, converting a scaffold or rope into a tangled rope or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope,
    'Does the Parsi Zoroastrian reading of marriage as community-preservation foreclose the secular contractual reading within Indian personal law, or merely coexist as a parallel jurisdiction?',
    'Jurisprudential analysis of Indian Supreme Court decisions on personal law versus fundamental rights, particularly in cases where Parsi intermarried individuals sought religious burial and trust rights.',
    'If foreclosing, the constraint denies state-law exit options and extracts more heavily; if coexisting, secular marriage remains a viable alternative and effective extraction is reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Whether the Parsi reading forecloses or coexists with secular marriage').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is endogamy enforcement maintained by structural state recognition of Parsi personal law, or by internalized community identity fusion that would persist without state backing?',
    'Comparative study of Parsi diaspora communities in jurisdictions without state personal law; persistence of endogamy norms indicates internalized suppression.',
    'Structural suppression locates extraction in the legal regime; internalized suppression raises effective extraction because the target carries the constraint beyond legal reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    preservation_extinction_paradox,
    'Does the endogamy constraint preserve the Parsi community or accelerate its demographic decline by shrinking the eligible marriage pool?',
    'Longitudinal demographic analysis comparing Parsi population trends to similar minority communities without strict endogamy enforcement.',
    'If decline is accelerated, the coordination narrative is cover for ritual authority maintenance; if preserved, extraction is the genuine cost of minority survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_extinction_paradox, empirical, 'Whether endogamy preserves or extinguishes the community').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_parsi_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fla_parsi_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(fla_parsi_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(fla_parsi_tr_t60, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 60, 0.44).
narrative_ontology:measurement(fla_parsi_tr_t80, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(fla_parsi_tr_t100, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(fla_parsi_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fla_parsi_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(fla_parsi_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(fla_parsi_be_t60, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(fla_parsi_be_t80, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 80, 0.73).
narrative_ontology:measurement(fla_parsi_be_t100, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fla_parsi_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fla_parsi_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(fla_parsi_su_t40, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(fla_parsi_su_t60, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(fla_parsi_su_t80, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(fla_parsi_su_t100, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into multiple constraints because the label 'marriage law' conflates structurally distinct claims: sacramental/samskara (Hindu), contract/nikah (Muslim), community-preservation/ritual (Parsi), sacrament/ecclesiastical (Christian), and autonomous civil contract (secular). Each reading has distinct epsilon values, beneficiary structures, and enforcement mechanisms. This file is the parsi_zoroastrian_reading; siblings are separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
