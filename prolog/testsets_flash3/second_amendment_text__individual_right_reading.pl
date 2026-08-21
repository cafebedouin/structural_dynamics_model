% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_right_reading, []).

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
 *   constraint_id: second_amendment_text__individual_right_reading
 *   human_readable: Second Amendment: Individual Right to Bear Arms (Self-Defense Reading)
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents the 'individual right' reading of the Second
 *   Amendment, which interprets the operative clause as guaranteeing an
 *   individual's right to possess firearms for self-defense, independent of
 *   militia service. This reading has gained prominence, particularly after
 *   Supreme Court decisions like Heller (2008) and Bruen (2022), shifting the
 *   focus from collective security to personal liberty. The constraint is
 *   classified as a Tangled Rope due to its genuine coordination function
 *   (providing a framework for self-defense) intertwined with significant
 *   asymmetric extraction (costs borne by disarmed populations and victims of
 *   gun violence) and active enforcement by gun rights advocacy groups and
 *   the judiciary.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Beneficiary (organized/constrained)
 *   - firearms_manufacturers: Beneficiary (powerful/arbitrage)
 *   - gun_rights_advocacy_groups: Agenda Setter (institutional/analytical)
 *   - disarmed_populations: Payer (powerless/trapped)
 *   - victims_of_gun_violence: Payer (powerless/trapped)
 *   - local_governments: Payer (organized/constrained)
 *   - federal_judiciary: Agenda Setter (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_text__individual_right_reading, 0.75).
domain_priors:theater_ratio(second_amendment_text__individual_right_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_text__individual_right_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_right_reading, "Second Amendment: Individual Right to Bear Arms (Self-Defense Reading)").
narrative_ontology:topic_domain(second_amendment_text__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_right_reading, '2f4e1382-2d17-4044-b5f7-edd85fc27038').
narrative_ontology:cs_kernel_codification('2f4e1382-2d17-4044-b5f7-edd85fc27038', fixed_text).
narrative_ontology:cs_authority_grounding('2f4e1382-2d17-4044-b5f7-edd85fc27038', lineage).
narrative_ontology:cs_interpretation_layer_present('2f4e1382-2d17-4044-b5f7-edd85fc27038').
narrative_ontology:cs_reading_relation('2f4e1382-2d17-4044-b5f7-edd85fc27038', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f4e1382-2d17-4044-b5f7-edd85fc27038', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('2f4e1382-2d17-4044-b5f7-edd85fc27038', foundational, individual_self_defense_is_fundamental_right).
narrative_ontology:cs_axiom_status(individual_self_defense_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('2f4e1382-2d17-4044-b5f7-edd85fc27038', individual_self_defense_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('2f4e1382-2d17-4044-b5f7-edd85fc27038', foundational, right_to_bear_arms_is_independent_of_militia_service).
narrative_ontology:cs_axiom_status(right_to_bear_arms_is_independent_of_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('2f4e1382-2d17-4044-b5f7-edd85fc27038', right_to_bear_arms_is_independent_of_militia_service, conventional).
narrative_ontology:cs_reference_frame('2f4e1382-2d17-4044-b5f7-edd85fc27038', post_heller_bruen_jurisprudence).
narrative_ontology:cs_drift_state('2f4e1382-2d17-4044-b5f7-edd85fc27038', contemporary_public_health_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2f4e1382-2d17-4044-b5f7-edd85fc27038', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_right_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_right_reading, gun_rights_advocacy_groups).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, disarmed_populations).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, victims_of_gun_violence).
narrative_ontology:constraint_victim(second_amendment_text__individual_right_reading, local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the legal protection of firearm ownership for self-defense, resisting regulations that would restrict access to weapons. Their identity is often tied to this right, making 'exit' (relinquishing the right) unthinkable.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, constrained, national).

% Profit from the broad interpretation of the right to bear arms, leading to increased sales and reduced regulatory burdens. They actively lobby to maintain and expand this interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, firearms_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Act as primary interpreters and enforcers of this reading, shaping public discourse and legal challenges. They mobilize individual gun owners and influence political processes to protect and expand firearm rights.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, gun_rights_advocacy_groups, agenda_setter,
    institutional, generational, analytical, national).

% Includes individuals legally prohibited from owning firearms (e.g., felons, domestic abusers) who are denied the right to self-defense via firearms, as well as communities disproportionately affected by gun violence who bear the social costs of widespread firearm access.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, disarmed_populations, payer,
    powerless, biographical, trapped, local).

% Bear the direct and indirect costs of gun violence, including physical harm, psychological trauma, and community disruption. They often advocate for stricter gun control measures but face significant political barriers.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, victims_of_gun_violence, payer,
    powerless, immediate, trapped, local).

% Struggle to implement local firearm regulations due to preemption laws and legal challenges based on this reading of the Second Amendment. They bear the costs of increased gun violence and public safety challenges.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, local_governments, payer,
    organized, biographical, constrained, local).

% Interprets the Second Amendment, with recent rulings (e.g., Heller, Bruen) affirming and expanding the individual right to bear arms for self-defense, thereby shaping the constraint's application and enforcement.
narrative_ontology:constraint_stakeholder(second_amendment_text__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__individual_right_reading, firearms_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_text__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for individual citizens to possess firearms for personal protection, ensuring a standardized legal basis for self-defense across jurisdictions, albeit with varying interpretations.
% TRANSFER_FUNCTION: Transfers the burden of self-protection to individuals, potentially reducing the state's perceived responsibility for public safety, while transferring legal and social costs of widespread firearm access to communities and victims.
% ABSENT_VOICES: Advocates for stricter gun control, public health experts, and communities disproportionately affected by gun violence are often marginalized in the legal and political discourse that shapes this reading, despite their direct experience with its consequences.
% DISAPPEARANCE_RATIONALE: If this reading of the Second Amendment vanished, the legal landscape for firearms would fundamentally shift. States and localities would gain significant power to regulate firearms, leading to a patchwork of stricter laws, potentially reducing gun violence but also sparking intense political and social reorganization around the issue of self-defense and state power.
% FOUNDING_PROBLEM: The founding problem was to ensure the capacity for self-defense and resistance against potential tyranny, rooted in a fear of standing armies and the need for a well-regulated militia composed of armed citizens.
% FOUNDING_PROBLEM_CORROBORATION: Gun rights advocates and some legal scholars argue the problem remains live, citing the need for individual protection against crime and government overreach. Gun control advocates and other scholars argue the original problem of militia service is largely obsolete, and the contemporary problem is gun violence, not tyranny, with corroboration from public health data and international comparisons.
narrative_ontology:disappearance_verdict(second_amendment_text__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__individual_right_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(second_amendment_text__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__individual_right_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant social and public health costs associated with widespread firearm access, borne by specific victim groups, while the benefits are concentrated among gun owners and the firearms industry. Suppression (0.75) is high due to the legal and political barriers to enacting stricter gun control measures, often enforced through judicial review and lobbying. Theater ratio (0.20) is relatively low, as the core function of protecting individual gun ownership is actively pursued, though some arguments for 'well-regulated militia' may be performative. Accessibility collapse (0.40) is moderate, as alternatives to firearm self-defense exist but are often deemed insufficient by beneficiaries, while resistance (0.80) is high, reflecting ongoing political and social contestation.
 *
 * PERSPECTIVAL GAP:
 *   Individual gun owners and advocacy groups perceive this as a fundamental liberty, a 'Rope' ensuring self-defense. Disarmed populations and victims of gun violence experience it as a 'Snare' that extracts safety and peace, with little recourse. The federal judiciary, while affirming the individual right, often frames it as a balanced constitutional principle, attempting to reconcile these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners, firearms manufacturers, and gun rights advocacy groups are clear beneficiaries, experiencing low directionality as the constraint subsidizes their interests. Disarmed populations, victims of gun violence, and local governments are targets, bearing the costs and facing high directionality. The federal judiciary, as an agenda-setter, shapes the constraint's application, influencing directionality for all parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this constraint as a pure Rope (as claimed by beneficiaries) or a pure Snare (as experienced by victims). It acknowledges the genuine coordination function of providing a framework for self-defense while highlighting the asymmetric extraction and active enforcement required to maintain it, especially as the founding problem of militia service becomes contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_vs_individual_right,
    'Is the ''well-regulated militia'' clause a condition precedent for the right to bear arms, or merely a prefatory clause stating a purpose?',
    'Further Supreme Court jurisprudence or a constitutional amendment clarifying the relationship between the two clauses.',
    'If conditional, the constraint''s scope would narrow significantly, allowing for greater state regulation. If prefatory, the individual right remains broad, limiting state power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_vs_individual_right, conceptual, 'Ambiguity regarding the relationship between the militia clause and the individual right.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of gun control efforts structural (legal precedent, lobbying power) or internalized (cultural identity, fear of government)?',
    'Analysis of legislative success rates in states with varying political cultures and legal challenges; surveys on motivations for gun ownership and opposition to regulation.',
    'If primarily structural, legal and political reforms could more directly alter the constraint. If significantly internalized, changes would require deeper cultural shifts, making the constraint more resilient to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for gun control.').

omega_variable(
    self_defense_efficacy_vs_social_cost,
    'Does the individual right to bear arms for self-defense genuinely enhance personal safety more than it contributes to overall gun violence and social costs?',
    'Comprehensive, longitudinal epidemiological studies comparing outcomes in jurisdictions with different firearm access laws, controlling for socioeconomic factors.',
    'Empirical evidence demonstrating a net negative impact on safety would challenge the foundational premise of this reading, potentially shifting public and judicial preference towards the collective security or public health readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_defense_efficacy_vs_social_cost, empirical, 'Empirical balance of self-defense benefits versus social costs of widespread firearm access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_right_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1970, second_amendment_text__individual_right_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_text__individual_right_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_text__individual_right_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_text__individual_right_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_text__individual_right_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__individual_right_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(seco_be_t1970, second_amendment_text__individual_right_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(seco_be_t1980, second_amendment_text__individual_right_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(seco_be_t1990, second_amendment_text__individual_right_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(seco_be_t2000, second_amendment_text__individual_right_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(seco_be_t2010, second_amendment_text__individual_right_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__individual_right_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1970, second_amendment_text__individual_right_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(seco_su_t1980, second_amendment_text__individual_right_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(seco_su_t1990, second_amendment_text__individual_right_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(seco_su_t2000, second_amendment_text__individual_right_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(seco_su_t2010, second_amendment_text__individual_right_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__individual_right_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, firearms_licensing_regulations).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, assault_weapons_bans).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, concealed_carry_laws).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_right_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'second_amendment_text' kernel, each with different beneficiaries, victims, and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
