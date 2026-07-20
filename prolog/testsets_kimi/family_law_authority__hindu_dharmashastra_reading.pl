% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Sacramental Marriage (Pre-Reform Reading)
 *   domain: religious_governance/social/legal
 *
 * SUMMARY:
 *   This constraint instantiates the Hindu dharmashastra reading of the
 *   family_law_authority kernel: marriage as sacramental samskara governed by
 *   dharmic texts and customary practice. Under this reading, marriage is
 *   indissoluble (pre-1955), caste endogamy is a dharmic imperative, the wife
 *   enters the husband's joint family as a ritual participant rather than an
 *   autonomous contractor, and property moves within the patriline. Colonial
 *   courts gave this reading legal enforceability through Anglo-Hindu law,
 *   while caste councils and family elders provided social enforcement. The
 *   constraint is claimed as sacred coordination but operates with
 *   substantial asymmetric extraction.
 *
 * KEY AGENTS:
 *   - Brahminical interpreters (agenda_setter/institutional): interpret texts and certify legitimacy
 *   - Patrilineal joint families (beneficiary/organized): accrue labor, ritual service, and lineage continuity
 *   - Wives (payer/powerless): bear indissolubility and subordination with identity-locked exit
 *   - Lower-caste marriage seekers (payer/powerless): trapped by endogamy enforcement
 *   - Colonial personal law courts (observer/institutional): translate religious norms into state enforceability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.73).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.78).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.73).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Dharmashastra Sacramental Marriage (Pre-Reform Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "religious_governance/social/legal").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e').
narrative_ontology:cs_kernel_codification('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', fixed_text).
narrative_ontology:cs_authority_grounding('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', lineage).
narrative_ontology:cs_interpretation_layer_present('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e').
narrative_ontology:cs_reading_relation('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', foundational, marriage_as_sacramental_samskara).
narrative_ontology:cs_axiom_status(marriage_as_sacramental_samskara, holdable).
narrative_ontology:cs_axiom_grounding('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', marriage_as_sacramental_samskara, theological).
narrative_ontology:cs_axiom('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', foundational, wife_as_non_autonomous_ritual_participant).
narrative_ontology:cs_axiom_status(wife_as_non_autonomous_ritual_participant, holdable).
narrative_ontology:cs_axiom_grounding('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', wife_as_non_autonomous_ritual_participant, deontological).
narrative_ontology:cs_reference_frame('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', sacramental_dharma_order).
narrative_ontology:cs_drift_state('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', pre_reform_colonial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3de4e6b5-39f8-4ec9-aae7-1d54e89b4d8e', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_families).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahminical_interpreters).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, wives).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_marriage_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret dharmic texts to adjudicate marriage validity, caste status, and joint-family property rights. Their authority derives from mastery of Sanskrit textual tradition and customary precedent. They set the rules for sacramental performance and caste compatibility, and are consulted by colonial courts and local communities to certify what counts as legitimate Hindu marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahminical_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Receive the wife's labor, ritual participation, and reproductive capacity into the joint-family unit. Retain ancestral property within the patriline and ensure lineage continuity through sacramental marriage. They enforce caste endogamy to maintain social standing and are the primary beneficiaries of the indissoluble bond.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_families, beneficiary,
    organized, generational, constrained, regional).

% Enter the husband's joint family as a ritual participant and laborer. Bound by sacramental indissolubility prior to 1955 reform, with extremely limited grounds for separation. Their autonomy is subordinated to the joint family's collective interests and the dharmic duty of stridharma; exit means social death and loss of caste.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, wives, payer,
    powerless, biographical, identity_locked, local).

% Face severe social and economic sanctions for seeking marriage partners outside their caste varna or jati. Caste endogamy rules enforced by community councils and threat of social boycott limit their marriage market and reinforce hierarchical status distinctions.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_marriage_seekers, payer,
    powerless, biographical, trapped, regional).

% Apply Anglo-Hindu law by consulting dharmashastra texts and customary usage. They act as external arbiters translating religious norms into legal enforceability, with the power to decide which textual interpretations bind, but do not themselves collect from the arrangement.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, colonial_personal_law_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, patrilineal_joint_families).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social reproduction, ritual continuity, patrilineal property transmission, and caste-boundary maintenance through a sacramental framework recognized by the Hindu community and enforced by interpretive authority.
% TRANSFER_FUNCTION: Moves labor, ritual participation, property rights, and reproductive capacity from wives and lower-caste individuals to patrilineal joint families and Brahminical authority; transfers legitimacy and social standing through the performance of samskara.
% ABSENT_VOICES: Wives are not authors of dharmashastra texts; lower-caste voices are excluded from textual interpretation; reformist Hindu voices challenging indissolubility and caste endogamy are marginalized from the authoritative interpretive sphere.
% DISAPPEARANCE_RATIONALE: Joint-family property, caste endogamy, and sacramental indissolubility are organizing principles of the pre-reform Hindu social order; without them, family structure, caste boundaries, and Brahminical religious authority would reorganize fundamentally.
% FOUNDING_PROBLEM: Social and cosmic disorder from unregulated unions, property fragmentation, and varna-sankara (caste mixing); marriage as samskara orders dharma, patriline, and caste boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Colonial ethnographers and legal historians attest the social-order function from an analytical seat; anti-caste reformers and women's rights advocates contest the continued necessity of the founding problem from payer and excluded seats. No fully neutral corroboration existsâthe problem is asserted by beneficiaries and contested by targets.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.73, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.73) because the structure extracts labor, reproductive capacity, and autonomy from wives and caste mobility from lower-caste seekers, concentrating benefits in patrilineal families and interpretive authority. Suppression is higher (0.78) because persistence depends on active enforcement by caste councils, social ostracism, and colonial courts. Theater is moderate-high (0.44): colonial-era textual interpretation increasingly performed continuity with classical dharmashastra while actual practice and customary norms drifted substantially from the texts. Accessibility collapse is high (0.75) because alternatives such as inter-caste marriage, divorce, or autonomous contractual union were structurally foreclosed for Hindus under personal law. Resistance is moderate (0.45) because reformist and anti-caste movements challenged the arrangement but did not yet command state power.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahminical interpreter seat, the constraint is genuine coordination of dharma, social order, and cosmic continuity; its extraction is invisible or recast as necessary ritual cost. From the wife's seat, the same structure is enforced subordination with no exit. From the lower-caste seat, it is caste hierarchy reproduced through marriage regulation. The engine computes this divergence from the structural asymmetry in exit options and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical interpreters and patrilineal joint families are structural beneficiaries: they collect authority, labor, and lineage continuity, placing them near the beneficiary end of directionality. Wives and lower-caste marriage seekers are structural targets: they bear the costs of indissolubility, subordination, and endogamy with severely constrained or identity-locked exit, placing them near the full-target end. Colonial courts sit near symmetric: they enforce but do not collect, acting as a translating institution.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled rope prevents the false-summit error of treating gender and caste asymmetry as natural features of sacrament, while also preventing the snare error of denying that the constraint solves real coordination problems for joint-family property and religious continuity. The coordination function is real but inseparable from its extractive load; the indissolubility and endogamy are not incidental byproducts but structurally loaded onto specific seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_sibling_boundary,
    'Does the Hindu dharmashastra reading foreclose the secular contractual reading within a unified legal framework, or do they coexist as parallel options?',
    'Examine whether the Special Marriage Act and Hindu Marriage Act operate as mutually exclusive tracks or as coexisting layers; structural analysis of colonial and post-colonial personal-law jurisprudence.',
    'If foreclosed, the kernel is singular and the readings are rival claimants to one authority; if coexisting, the kernel is genuinely plural and each reading is a separate constraint in a federal legal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_boundary, conceptual, 'Whether dharmashastra and secular contractual readings are logically or structurally mutually exclusive').

omega_variable(
    textual_vs_customary_authority,
    'Is the constraint anchored in fixed dharmashastra texts or in evolving customary practice that diverges from those texts?',
    'Comparative analysis of colonial case-law records against ethnographic accounts of actual marriage practice in the same period.',
    'If anchored in text, the kernel is fixed_text and the constraint is a commitment system with interpretive drift; if anchored in custom, the kernel is implicit/distributed and the constraint is better modeled as practice-based identity coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_vs_customary_authority, empirical, 'Whether textual or customary authority dominates the constraint''s operation').

omega_variable(
    gender_extraction_naturalization,
    'Is the wife''s ritual subordination and indissolubility a necessary constitutive feature of the sacrament, or an extractive overlay that could be removed without dissolving the coordination function?',
    'Comparative analysis across samskara types and across reformist Hindu readings that retain sacrament while modifying indissolubility and autonomy.',
    'If constitutive, the extraction is partly the price of coordination; if overlay, the effective extractiveness is higher than the coordination floor and the constraint leans toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_extraction_naturalization, conceptual, 'Whether gender subordination is inherent to the sacramental form or separable extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(fami_tr_t80, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(fami_tr_t100, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 100, 0.44).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(fami_be_t20, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(fami_be_t40, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(fami_be_t60, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(fami_be_t80, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(fami_be_t100, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 100, 0.73).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(fami_su_t20, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(fami_su_t40, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(fami_su_t60, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(fami_su_t80, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement(fami_su_t100, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel family_law_authority. The kernel decomposes into multiple constraints because the natural-language concept 'family law authority' conflates structurally distinct claims: sacramental (Hindu, Christian, Parsi), contractual (Muslim, secular), and civil-individual frames. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
