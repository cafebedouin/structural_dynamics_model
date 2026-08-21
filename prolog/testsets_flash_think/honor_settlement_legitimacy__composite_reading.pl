% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Decline of Dueling as Legitimate Honor Settlement (Composite Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint describes the decline of dueling as a legitimate means of
 *   honor settlement, viewed through a 'composite reading' that emphasizes
 *   multiple reinforcing mechanisms. It posits that dueling's decline was
 *   overdetermined by cultural contraction (making dueling unthinkable) as a
 *   dominant force, reinforced by material and institutional changes (legal
 *   prohibitions, alternative dispute resolution) that would have
 *   independently suppressed the practice. The constraint itself is the
 *   diminishing legitimacy and increasing suppression of dueling, which acted
 *   as a snare for those still bound by its code.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.45).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.7).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, snare).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Decline of Dueling as Legitimate Honor Settlement (Composite Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '13f43be0-b0fb-4dc3-8183-904eaf79605d').
narrative_ontology:cs_kernel_codification('13f43be0-b0fb-4dc3-8183-904eaf79605d', implicit).
narrative_ontology:cs_authority_grounding('13f43be0-b0fb-4dc3-8183-904eaf79605d', practice).
narrative_ontology:cs_interpretation_layer_present('13f43be0-b0fb-4dc3-8183-904eaf79605d').
narrative_ontology:cs_reading_relation('13f43be0-b0fb-4dc3-8183-904eaf79605d', honor_settlement_legitimacy__contraction_reading, influences).
narrative_ontology:cs_reading_relation('13f43be0-b0fb-4dc3-8183-904eaf79605d', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('13f43be0-b0fb-4dc3-8183-904eaf79605d', foundational, decline_is_multi_causal).
narrative_ontology:cs_axiom_status(decline_is_multi_causal, holdable).
narrative_ontology:cs_axiom_grounding('13f43be0-b0fb-4dc3-8183-904eaf79605d', decline_is_multi_causal, empirically_contingent).
narrative_ontology:cs_axiom('13f43be0-b0fb-4dc3-8183-904eaf79605d', foundational, cultural_contraction_dominant).
narrative_ontology:cs_axiom_status(cultural_contraction_dominant, holdable).
narrative_ontology:cs_axiom_grounding('13f43be0-b0fb-4dc3-8183-904eaf79605d', cultural_contraction_dominant, empirically_contingent).
narrative_ontology:cs_reference_frame('13f43be0-b0fb-4dc3-8183-904eaf79605d', honor_code_supremacy).
narrative_ontology:cs_drift_state('13f43be0-b0fb-4dc3-8183-904eaf79605d', post_enlightenment_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('13f43be0-b0fb-4dc3-8183-904eaf79605d', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, legal_authorities).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, evolving_bourgeois_society).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_bound_gentlemen).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, families_of_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who felt compelled by an ingrained honor code to engage in dueling, despite increasing legal prohibitions and social stigma, bearing the risk of death, injury, and legal repercussions.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_bound_gentlemen, payer,
    powerless, biographical, identity_locked, local).

% Institutions (courts, legislatures) that actively criminalized dueling, enforced anti-dueling laws, and promoted alternative, state-sanctioned methods of dispute resolution, thereby eroding dueling's legitimacy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, legal_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Churches and other religious bodies that condemned dueling as immoral and un-Christian, advocating for peaceful reconciliation and contributing to the moral delegitimization of the practice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, religious_institutions, beneficiary,
    institutional, generational, mobile, national).

% The rising middle class and its associated values, which prioritized commerce, stability, and a more 'civilized' public order over the violent resolution of personal honor disputes, benefiting from the decline of dueling.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, evolving_bourgeois_society, beneficiary,
    organized, generational, mobile, national).

% The wives, children, and relatives of duelists who suffered the direct consequences of dueling, including bereavement, social ostracism, and financial hardship, with little agency to prevent participation.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, families_of_duelists, payer,
    powerless, immediate, trapped, local).

% Scholars who analyze the historical, social, and cultural factors contributing to the decline of dueling, interpreting its mechanisms and consequences from an academic perspective.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, cultural_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its historical context, dueling coordinated honor disputes among elites, providing a ritualized, albeit violent, mechanism for settling grievances, defending reputation, and asserting social standing when legal systems were perceived as inadequate for such matters. Its decline meant this function atrophied.
% TRANSFER_FUNCTION: The decline of dueling transferred the authority for dispute resolution from individual honor codes to state legal systems and evolving social norms. It shifted the costs of violence from individuals and their families to the state (through law enforcement) and society (through changing cultural values).
% ABSENT_VOICES: The voices of those who staunchly defended dueling as an essential component of honor culture, or those whose social standing was inextricably linked to its practice, became increasingly marginalized and absent from the dominant public discourse as the practice declined.
% DISAPPEARANCE_RATIONALE: If dueling as a legitimate practice had vanished overnight, the entire social fabric of elite dispute resolution, the conception of personal honor, and the role of violence in maintaining social status would have fundamentally reorganized. Legal systems and social norms evolved to fill the void, leading to a different social order.
% FOUNDING_PROBLEM: Dueling emerged to provide a formal, ritualized mechanism for men of honor to resolve grievances, defend reputation, and assert social standing in a context where legal systems might not adequately address perceived slights to honor.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and cultural anthropologists widely corroborate that dueling served this function, and that its decline was a response to evolving legal frameworks and social values, not a re-assertion of its original utility. Contemporary accounts from outside the dueling class also attest to its perceived necessity and subsequent obsolescence.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) reflects the high costs (life, limb, social standing) for participants, even as its social utility waned. Suppression (0.70) is high and increasing, driven by legal prohibitions and social stigma. Theater ratio (0.20) remains low, as the practice was genuinely declining and being suppressed, not merely theatrically maintained. Accessibility collapse (0.80) is high because alternatives became more viable and dueling itself became less accessible. Resistance (0.75) was significant from legal and religious institutions. The claimed type is 'snare' because, during its decline, dueling became a coercive mechanism for those trapped by honor codes, with clear victims and active enforcement against it, even as its original coordination function atrophied.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of legal authorities and bourgeois society, the decline of dueling was a positive development, a move towards a more rational and orderly society. From the perspective of honor-bound gentlemen, it was a loss of a vital mechanism for maintaining personal and social standing, leaving them trapped between conflicting codes.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal authorities, religious institutions, and the evolving bourgeois society are beneficiaries, as they gained from the increased social order and reduced violence. Honor-bound gentlemen and their families are victims, bearing the direct costs and risks of a practice that was increasingly criminalized and stigmatized. The 'honor-bound gentlemen' are identity-locked, compelled by internal and social pressures despite external costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    relative_weight_of_mechanisms,
    'What was the precise relative weight of cultural contraction versus institutional/material changes in the overdetermined decline of dueling?',
    'Comparative historical analysis across different national contexts with varying legal and cultural trajectories, using counterfactual modeling to isolate causal pathways.',
    'If cultural contraction was overwhelmingly dominant, the constraint leans more towards an ''identity_coordination'' type that became unthinkable. If institutional changes were equally or more dominant, it emphasizes the ''snare'' aspect of legal and social coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relative_weight_of_mechanisms, empirical, 'Quantifying the contribution of different causal factors to dueling''s decline.').

omega_variable(
    honor_code_persistence_threshold,
    'At what point did the honor code''s internal logic cease to compel dueling, independent of external suppression?',
    'Analysis of personal correspondence, diaries, and literary works from the late period of dueling to identify shifts in individual moral reasoning and social expectations regarding honor.',
    'If the internal compulsion ceased early, it supports the ''contraction_reading'' and suggests the ''snare'' aspect was primarily external. If it persisted late, it highlights the ''identity_locked'' nature of the victims and the internal dimension of the ''snare''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_code_persistence_threshold, empirical, 'Identifying the threshold of internal honor code collapse.').

omega_variable(
    fringe_practice_significance,
    'How significant was the persistence of dueling as a ''fringe practice'' (as per the ''drop_reading'') in shaping the overall decline narrative?',
    'Detailed micro-historical studies of dueling incidents in the late 19th and early 20th centuries, assessing their social impact and the degree to which they challenged or reinforced the dominant narrative of decline.',
    'If fringe practice was more widespread or influential than currently understood, it would temper the ''unthinkability'' aspect of the composite reading, suggesting a more gradual and less absolute cultural shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_practice_significance, empirical, 'Assessing the impact of residual dueling practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__composite_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1775, honor_settlement_legitimacy__composite_reading, theater_ratio, 1775, 0.12).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__composite_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(hono_tr_t1825, honor_settlement_legitimacy__composite_reading, theater_ratio, 1825, 0.18).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__composite_reading, theater_ratio, 1850, 0.2).
narrative_ontology:measurement(hono_tr_t1875, honor_settlement_legitimacy__composite_reading, theater_ratio, 1875, 0.18).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1750, 0.55).
narrative_ontology:measurement(hono_be_t1775, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1775, 0.5).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1800, 0.45).
narrative_ontology:measurement(hono_be_t1825, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1825, 0.4).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1850, 0.35).
narrative_ontology:measurement(hono_be_t1875, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1875, 0.3).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(hono_su_t1775, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1775, 0.5).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(hono_su_t1825, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1825, 0.7).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(hono_su_t1875, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1875, 0.85).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
