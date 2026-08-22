% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Substance Control Legitimacy â Legalization Reading
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story models the legalization_reading of the
 *   substance_control_legitimacy kernel: the arrangement in which competent
 *   adults possess autonomy over substance use and state authority is
 *   strictly limited to preventing third-party harm. Under this reading, the
 *   criminalization of users is abolished, a legal market replaces the black
 *   market, and the state's role contracts to externality policing (DUI, age
 *   limits, secondhand exposure). The structural delta from sibling readings
 *   is that users exit the victim set, third-party harm bearers enter it, and
 *   corporate actors extract through the licensed market. The constraint
 *   coordinates around individual autonomy while extracting from diffuse
 *   third parties who cannot easily exit shared spaces.
 *
 * KEY AGENTS:
 *   - competent_adult_users: Primary beneficiary (organized/mobile) â gains autonomy and legal access.
 *   - legal_substance_industry: Primary beneficiary (powerful/arbitrage) â captures profits from licensed market.
 *   - third_party_harm_bearers: Primary target (powerless/trapped) â bears uncompensated externalities.
 *   - state_regulators: Agenda setter (institutional/analytical) â enforces the autonomy/harm boundary.
 *   - prohibitionist_advocates: Excluded voice (organized/constrained) â structurally marginalized by the framework.
 *   - public_health_system: Observer (institutional/analytical) â monitors but cannot override autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.7).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.6).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Substance Control Legitimacy â Legalization Reading").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'e16d2498-b1fc-40c6-b5dc-7b0f2d82c139').
narrative_ontology:cs_kernel_codification('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', formalized).
narrative_ontology:cs_authority_grounding('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', lineage).
narrative_ontology:cs_interpretation_layer_present('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139').
narrative_ontology:cs_reading_relation('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', foundational, adult_substance_autonomy_right).
narrative_ontology:cs_axiom_status(adult_substance_autonomy_right, holdable).
narrative_ontology:cs_axiom_grounding('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', adult_substance_autonomy_right, deontological).
narrative_ontology:cs_axiom('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', foundational, state_power_third_party_harm_limit).
narrative_ontology:cs_axiom_status(state_power_third_party_harm_limit, holdable).
narrative_ontology:cs_axiom_grounding('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', state_power_third_party_harm_limit, deontological).
narrative_ontology:cs_reference_frame('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', millian_autonomy_framework).
narrative_ontology:cs_drift_state('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', post_legalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e16d2498-b1fc-40c6-b5dc-7b0f2d82c139', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_substance_industry).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, third_party_harm_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal access to substances and autonomy over personal use decisions. No longer subject to criminalization for possession or use. Exit means abstaining, which is legally available but may not satisfy individual preferences.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    organized, biographical, mobile, national).

% Operates licensed production and distribution networks, capturing profits from a legally protected market. Benefits from regulatory barriers to entry that limit competition and from the state's suppression of unlicensed supply.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_substance_industry, beneficiary,
    powerful, biographical, arbitrage, national).

% Bear uncompensated costs from permitted substance use: secondhand exposure, impaired driving risk, and localized environmental or social degradation. Cannot easily opt out of sharing roads, air, or neighborhoods with users.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, third_party_harm_bearers, payer,
    powerless, immediate, trapped, local).

% Sets and enforces the boundary between permitted adult use and prohibited conduct. Administers licensing, taxation, DUI enforcement, and age restrictions. Justifies authority by reference to preventing third-party harm rather than protecting users from themselves.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for criminalization of all non-medical substance use. Under the legalization framework their preferred policy is structurally off the agenda; they may participate in debate but cannot advance prohibition within the current rules.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibitionist_advocates, excluded,
    organized, generational, constrained, national).

% Monitors population health impacts of legalized use and treats overdose or addiction sequelae. Under this framework lacks authority to restrict adult autonomous choice for paternalistic reasons; can only advise, remediate, and document outcomes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_system, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, legal_substance_industry).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of substance use by assigning decision rights to individuals and limiting state coercion to externality prevention, thereby displacing violent black markets and reducing incarceration.
% TRANSFER_FUNCTION: Moves autonomy and market profits to competent adult users and licensed corporations; moves uncompensated harm risk to third parties exposed to impaired behavior or environmental contamination.
% ABSENT_VOICES: Prohibitionist advocates who view any recreational substance use as moral failure are structurally excluded from policy setting. Future generations bearing long-term health and environmental externalities are not present at the founding.
% DISAPPEARANCE_RATIONALE: If the legalization framework vanished overnight, licensed corporations would lose legal market protections, criminal black markets would expand to meet demand, adult users would face renewed criminalization, and the state's regulatory apparatus would revert to prohibition-era enforcement â the policy landscape would fundamentally rearrange.
% FOUNDING_PROBLEM: Prohibition of substance use created violent unregulated black markets, empowered cartels, produced mass incarceration, and denied competent adults autonomy over their own bodies and choices.
% FOUNDING_PROBLEM_CORROBORATION: Criminal justice reform organizations and civil liberties groups outside the benefiting parties attest to prohibition's systemic harms; public health researchers and prohibitionist advocates contest that legalization fully solves the founding problem, citing rising corporate capture and population-level health effects.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.7, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70 at interval end) rises because legal-market corporate capture deepens over time and third-party harms (impaired driving, secondhand exposure) are partially externalized rather than fully internalized. Suppression (0.60) is moderate-high: the constraint requires active enforcement to maintain the legal market's monopoly against black-market competition and to police the autonomy/harm boundary. Theater ratio (0.48) rises as regulatory activity increasingly performs compliance theater (packaging warnings, zoning rules) that substitutes for genuine externality reduction. Accessibility collapse (0.40) is moderate: prohibitionist and public-health alternatives remain thinkable but are politically marginalized. Resistance (0.50) reflects ongoing prohibitionist advocacy and public health criticism of corporate capture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state regulators) experiences the constraint as a legitimate coordination mechanism protecting autonomy while policing harms. The beneficiary seats (adult users, industry) experience low extraction or net subsidy. The payer seat (third_party_harm_bearers) experiences high extraction via uncompensated risk exposure. The engine computes this divergence from the structural data â same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Adult users and the legal substance industry are declared beneficiaries because the constraint's operation directly transfers autonomy and profits to them. Third_party_harm_bearers are declared victims because they bear costs (roadway risk, secondhand exposure) generated by the very activity the constraint permits. The state is agenda_setter, not beneficiary, because its gain is administrative legitimacy rather than rent. Directionality derives from these declarations: d is low for beneficiaries, high for the trapped third-party victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the error of treating legalization as pure coordination (rope) â which would ignore the corporate extraction and uncompensated third-party harms â and the error of treating it as pure extraction (snare) â which would ignore the genuine autonomy gains and black-market displacement. The temporal measurements show extraction accumulating over the interval, suggesting that coordination decays into extraction (Goodhart drift) but has not become pure extraction. If the founding problem (prohibition's costs) is judged dead while the arrangement persists, mandatrophy would be flagged; here the founding problem status is contested, so the framework retains live legitimacy for one coalition while another reads it as atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_competition,
    'Is the legalization reading the most structurally accurate model of substance control legitimacy, or do the harm_reduction or prohibition readings better capture the constraint''s operation?',
    'Comparative policy analysis across jurisdictions adopting each reading''s framework, measuring extraction and coordination outcomes against one another.',
    'If the harm_reduction reading is more accurate, third-party harms and corporate extraction are larger than this reading admits; if the prohibition reading is more accurate, the user autonomy claimed here is illusory and extraction from users is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_competition, conceptual, 'Structural ambiguity arising from contested kernel readings').

omega_variable(
    prohibition_sibling_foreclosure,
    'Does the legalization reading''s core premise of adult autonomy logically foreclose the prohibition reading''s premise of state moral duty to prevent all use?',
    'Jurisprudential analysis of whether a single legal framework can simultaneously assert autonomy-as-right and duty-to-prohibit; empirical observation of hybrid regimes.',
    'If foreclosed, the two readings are mutually exclusive commitment systems; if coexisting, jurisdictions may oscillate between them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_sibling_foreclosure, conceptual, 'Logical relationship between legalization and prohibition readings').

omega_variable(
    corporate_extraction_scope,
    'Does the legal substance market''s corporate profit extraction represent contingent regulatory capture or an inherent structural feature of legalization?',
    'Cross-jurisdictional comparison of market structures (state monopoly, cooperative, private retail) measuring profit concentration and political influence.',
    'If inherent, the constraint''s extraction is inseparable from its coordination; if contingent, reform could reduce extraction without altering the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_extraction_scope, empirical, 'Whether corporate extraction is inherent to legalization').

omega_variable(
    third_party_harm_internality,
    'Can third-party harms from permitted substance use be fully internalized through tort or regulation, or do they represent irreducible extraction from non-consenting parties?',
    'Economic analysis of damage awards, insurance markets, and regulatory compensation funds in legalized jurisdictions.',
    'If fully internalizable, the extraction component diminishes toward rope; if irreducible, the tangled_rope classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_harm_internality, empirical, 'Whether third-party harms are eliminable or structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__legalization_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__legalization_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__legalization_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__legalization_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__legalization_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__legalization_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the legalization_reading of kernel substance_control_legitimacy. It decomposes from the natural-language concept of 'drug policy' into structurally distinct claims: prohibition (criminalization), harm reduction (public health minimization), and legalization (autonomy with harm limits). Each reading has different beneficiary/victim structures and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
