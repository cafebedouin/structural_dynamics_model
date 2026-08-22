% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Sufficiency: Legal Mandate Overrides Prior Rural Practice
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override reading of a contested
 *   kernel about how imposed state practice acquires legitimacy: the claim
 *   that decree authority alone is sufficient to displace prior practice,
 *   with compliance following from legal mandate regardless of whether the
 *   population internalizes the new norm. Under this reading, the calendar
 *   reform is a pure override — legally abolished, practically ignored in the
 *   countryside, sustained only by parallel bookkeeping and periodic
 *   enforcement sweeps — while the dress reform is a partial override,
 *   achieved through direct coercive enforcement (fines, confiscation, public
 *   inspection) in urban and official contact zones but never displacing
 *   customary dress in private and rural life. The state and its urban
 *   administrative allies capture the legitimacy and legibility benefits;
 *   rural populations, calendar practitioners, and customary dress
 *   communities absorb the enforcement costs of a change they had no part in
 *   authoring. This is one of three readings of the same kernel — see
 *   kernel_context and cs_structure for the sibling relationships to the
 *   endogenous_climb_reading and hybrid_scaffolding_reading, which are
 *   separate constraint stories, not alternate framings of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.79).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Sufficiency: Legal Mandate Overrides Prior Rural Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'ca1c7ce9-422d-4b10-855f-d6afc31076bf').
narrative_ontology:cs_kernel_codification('ca1c7ce9-422d-4b10-855f-d6afc31076bf', formalized).
narrative_ontology:cs_authority_grounding('ca1c7ce9-422d-4b10-855f-d6afc31076bf', extraction).
narrative_ontology:cs_interpretation_layer_present('ca1c7ce9-422d-4b10-855f-d6afc31076bf').
narrative_ontology:cs_reading_relation('ca1c7ce9-422d-4b10-855f-d6afc31076bf', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca1c7ce9-422d-4b10-855f-d6afc31076bf', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('ca1c7ce9-422d-4b10-855f-d6afc31076bf', foundational, decree_authority_is_self_sufficient_for_displacement).
narrative_ontology:cs_axiom_status(decree_authority_is_self_sufficient_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('ca1c7ce9-422d-4b10-855f-d6afc31076bf', decree_authority_is_self_sufficient_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('ca1c7ce9-422d-4b10-855f-d6afc31076bf', secondary, internalization_is_irrelevant_to_compliance_validity).
narrative_ontology:cs_axiom_status(internalization_is_irrelevant_to_compliance_validity, holdable).
narrative_ontology:cs_axiom_grounding('ca1c7ce9-422d-4b10-855f-d6afc31076bf', internalization_is_irrelevant_to_compliance_validity, conventional).
narrative_ontology:cs_reference_frame('ca1c7ce9-422d-4b10-855f-d6afc31076bf', sovereign_decree_sufficiency_doctrine).
narrative_ontology:cs_drift_state('ca1c7ce9-422d-4b10-855f-d6afc31076bf', post_enforcement_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca1c7ce9-422d-4b10-855f-d6afc31076bf', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_calendar_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, customary_dress_communities).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, sovereign_decree_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central state apparatus decrees abolition of the old calendar and coercive replacement of customary dress, framing both as necessary markers of national modernization and international legibility. It drafts the statutes, dispatches enforcement officials, and collects the legitimacy dividend of appearing modern to foreign observers and elite domestic constituencies, without needing rural populations to internalize the change.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda, agenda_setter).

% Provincial administrators and police carry out inspections, fines, and public dress checks to make the decree stick. They rotate postings, report compliance statistics upward, and bear little personal cost from rural resentment since their careers and social lives are anchored in the capital, not the villages they regulate.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, central_enforcement_officials, agenda_setter,
    institutional, biographical, mobile, national).

% Farmers and villagers organize planting, harvest, and ritual life around the old calendar and continue to wear customary dress in practice, absorbing fines, harassment, and periodic crackdowns as the cost of continuing what the decree nominally abolished. They were not consulted on the change and have no legal channel to contest it, only informal noncompliance and evasion.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations, payer,
    powerless, biographical, trapped, regional).

% Local ritual specialists, market-day organizers, and elders who maintain the old calendar for agricultural and ceremonial coordination find the decree has no practical purchase in their communities; the state calendar governs official paperwork while the old calendar continues to govern actual life, at the cost of legal exposure and periodic double bookkeeping.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_calendar_practitioners, payer,
    powerless, generational, constrained, regional).

% Communities whose customary dress marks status, ethnicity, or religious observance face direct coercive enforcement — confiscation, fines, public humiliation — when appearing in official or urban spaces, forcing partial code-switching (state dress in town, customary dress at home) that the calendar's rural population never has to perform since decree enforcement rarely reaches deep countryside.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, customary_dress_communities, payer,
    powerless, biographical, trapped, regional).

% Urban professionals, bureaucrats, and merchants who had already adopted the new calendar and dress for commercial and diplomatic convenience gain legal validation and reduced friction in dealing with the state; they experience the decree as confirmation of practices they already found advantageous, not as imposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_class, beneficiary,
    organized, generational, mobile, national).

% Foreign governments and lending institutions read decree compliance as a modernization signal affecting diplomatic recognition and credit terms. They are not present in the domestic conversation about enforcement costs but their approval is a structural reason the state prefers a decree-sufficiency theory of legitimacy over a slower internalization path.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, foreign_observers_and_creditors, excluded,
    institutional, biographical, analytical, global).

% Historians and political scientists compare this decree's actual displacement outcomes against the endogenous-climb and hybrid-scaffolding readings, tracking where calendar reform failed outright, where dress reform partially succeeded, and why the two domains diverged under identical decree authority.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, comparative_state_formation_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_agenda).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The decree seeks to synchronize the polity's official temporal and sartorial markers with a single legible standard, reducing transaction costs for the state's own administration, courts, taxation, and international dealings by eliminating parallel customary systems it must otherwise accommodate.
% TRANSFER_FUNCTION: Moves legitimacy and legibility gains to the state and urban administrative class (who already held or wanted the new standard) while moving compliance costs — fines, enforcement exposure, loss of ritual coordination infrastructure, forced double-tracking of calendars and wardrobes — onto rural and customary-practice populations who had no part in setting the mandate.
% ABSENT_VOICES: Rural populations, calendar practitioners, and customary dress communities were not consulted before the decree; village councils, ritual specialists, and local market organizers who depend on the old calendar for coordinating agricultural and ceremonial life had no seat in the drafting process and are only visible to the state as compliance statistics.
% DISAPPEARANCE_RATIONALE: If the decree and its enforcement apparatus vanished, rural communities would simply continue operating on the calendar and dress norms they never actually abandoned, urban records would need a transitional reconciliation mechanism, and the state would lose the international legibility signal it currently claims from formal compliance — showing the decree's real-world grip was already partial rather than total.
% FOUNDING_PROBLEM: The state needed a single, internationally legible standard of time-reckoning and dress to negotiate treaties, administer courts and taxation uniformly, and present itself as a modern sovereign entitled to equal treatment among nations, rather than a patchwork of customary jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: State ministries and their urban administrative beneficiaries attest the problem remains live — that a unified legal standard is still necessary for functioning courts and diplomacy. Independent historians and rural civil-society organizations, outside the beneficiary set, attest that the calendar half of the founding problem was never actually solved (rural non-compliance persisted for generations) and that formal legal uniformity substituted for the substantive coordination the decree claimed to deliver.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68, substantial but not maximal, because the decree does secure real formal coordination gains for the state (courts, taxation, diplomacy operate on the new standard) even as its practical grip on the countryside remains thin — this is a hybrid coordination/extraction structure, not pure extraction. Suppression starts very high (0.88) during the initial enforcement wave and declines modestly (to 0.79) as the state settles into steady-state enforcement against dress in official contact zones while effectively abandoning calendar enforcement in the countryside — the suppression is raw and unscaled, reflecting the coercive machinery's actual intensity independent of how widely the constraint's scope reaches. Theater ratio rises from 0.20 to 0.42 as the compliance statistics the state reports increasingly diverge from actual rural practice — the decree becomes progressively more performative as a legibility signal for foreign observers even as its substantive grip weakens.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_modernization_agenda and urban_administrative_class sit near the beneficiary end: they collect legitimacy, legal uniformity, and reduced administrative friction without bearing the adjustment costs. Rural populations, calendar practitioners, and customary dress communities sit near the full-target end: trapped or constrained exit, no consultation, and they bear fines, enforcement exposure, and the burden of maintaining parallel practices. Enforcement officials occupy an intermediate agenda-setter role — they administer the coercion but are personally insulated from its social costs by career mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (international legibility, administrative uniformity) is contested rather than fully dead or fully live: the state's formal legal apparatus achieved its uniformity on paper, but the underlying coordination problem the calendar reform was meant to solve was never actually resolved in the countryside, where the old calendar continued governing real economic and ceremonial life. This divergence between formal compliance and substantive practice is exactly what the tangled_rope classification is built to detect — a real coordination function (uniform legal time-reckoning for courts and diplomacy) persists alongside a substantial extraction (coercive costs imposed on non-consulted rural populations) that the decree-sufficiency theory has no mechanism to acknowledge, since compliance is defined as following from the decree itself rather than from lived uptake.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decree_sufficiency_vs_internalization_requirement,
    'Is state decree authority genuinely sufficient to displace prior practice (this reading''s premise), or does displacement actually require the internalization pathway the endogenous_climb_reading insists is necessary — with the calendar reform''s practical failure serving as the decisive counter-case?',
    'Comparative historical analysis of decree outcomes across domains with and without accompanying ideological/educational reinforcement: if calendar reform (pure decree, no scaffolding) failed to displace practice while dress reform (decree plus visible coercive enforcement acting as a substitute for internalization) achieved partial displacement, this suggests decree sufficiency is domain-dependent rather than a general sufficiency claim.',
    'If decree sufficiency fails as a general claim, this reading''s own account of compliance following from legal mandate ''regardless of internalization'' is falsified in the calendar domain, which would reclassify the calendar sub-case as a snare (extraction persisting without even the state''s own claimed coordination benefit) rather than a tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decree_sufficiency_vs_internalization_requirement, conceptual, 'Whether the exogenous_override reading''s core sufficiency premise survives its own worst case (the calendar).').

omega_variable(
    compliance_statistics_vs_practice_reality,
    'Do the state''s reported compliance statistics reflect actual practice change, or do they measure only the administrative fiction of formal-legal adoption while rural life continues on the old calendar and customary dress?',
    'Ethnographic and archival cross-check of official compliance records against village-level records (market schedules, ritual calendars, dress in non-official settings) for the same period and region.',
    'A wide compliance-statistics-to-practice gap would confirm the rising theater_ratio trajectory authored here and support reading the decree''s later-period operation as substantially performative for foreign audiences rather than substantively transformative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_statistics_vs_practice_reality, empirical, 'Whether official compliance reporting tracks or diverges from lived practice.').

omega_variable(
    beneficiary_capture_of_modernization_narrative,
    'Is ''state modernization'' authored here as a coherent public good the decree pursues, or is it itself a proposition that primarily serves the urban administrative class and foreign-facing legitimacy needs, with rural adjustment costs treated as an externality never priced into the decision?',
    'Examine whether any consultation, compensation, or phased-transition mechanism was offered to rural populations at the time of decree, versus purely top-down promulgation.',
    'If no consultation or compensation existed, this strengthens the tangled_rope reading (coordination function is real for the state and urban class, but the extraction from rural populations is asymmetric and uncompensated) over a rope reading that would require rural populations to be net beneficiaries too.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_capture_of_modernization_narrative, empirical, 'Whether rural adjustment costs were ever weighed against the decree''s centrally claimed benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 8, 0.84).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 16, 0.81).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimacy_of_imposed_practice kernel, each a separate ε-invariant constraint story per the ε-invariance principle. exogenous_override_reading (this story) claims decree authority alone suffices for displacement and authors ε=0.68 for a mixed outcome (pure override failure on calendar, partial override success on dress). endogenous_climb_reading claims displacement requires bottom-up internalization and would author its own ε reflecting how imposed commitments fail absent adoption pathways. hybrid_scaffolding_reading claims decree succeeds only when paired with ideological messaging generating quasi-endogenous pull, and would author an intermediate ε reflecting partial, scaffolded displacement. The three stories share the same underlying historical episode but instantiate structurally distinct claims about the mechanism of legitimacy, with different implied victim/beneficiary weightings and different persistence conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
