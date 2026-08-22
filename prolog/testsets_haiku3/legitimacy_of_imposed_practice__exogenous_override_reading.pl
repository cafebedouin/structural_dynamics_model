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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: State Decree Authority Over Prior Practice (Exogenous Override Reading)
 *   domain: political_history/state_formation
 *
 * SUMMARY:
 *   This constraint instantiates the EXOGENOUS OVERRIDE READING of the
 *   legitimacy-of-imposed-practice kernel. The reading's core claim: state
 *   decree authority is sufficient to displace prior practice; compliance
 *   follows from legal mandate regardless of internalization. The standing
 *   arrangement under contest is the decree-driven imposition of unified
 *   social practice (e.g., calendar reform, dress codes, language
 *   standardization) in early-modern or colonial state formation. The reading
 *   asserts that coercive authority and administrative machinery can enforce
 *   practice change without requiring the target population to internalize
 *   the new norms as legitimate. Rural communities and traditional
 *   practitioners bear the costs of displacement; state administrative elites
 *   and enforcement officials benefit. The constraint is CLAIMED as
 *   tangled_rope (coordination function + asymmetric extraction + active
 *   enforcement) and authored metrics confirm substantive extractiveness and
 *   suppression that rise over the interval as the enforcement apparatus
 *   matures.
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus: institutional agenda-setter; issues decree, deploys enforcement, benefits from unified administrative control — holds the reading's core premise that fiat authority suffices
 *   - rural_communities: powerless, trapped payers; bear adjustment costs without consent; exit is structurally unavailable
 *   - traditional_practitioners: moderate power, identity-locked; lose livelihood and self-concept; excluded from decree formation
 *   - urban_administrative_elite: institutional beneficiaries; gain legitimacy and extraction opportunities from the decree's success
 *   - enforcement_officials: hybrid beneficiary/agenda-setter; execute decree and rent-seek through selective enforcement
 *   - reformist_intelligentsia: moderate-power beneficiaries; ideological architects; internalize modernization narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.72).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Authority Over Prior Practice (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'd181dd4a-3b91-4e9e-a5af-f0c171c43492').
narrative_ontology:cs_kernel_codification('d181dd4a-3b91-4e9e-a5af-f0c171c43492', formalized).
narrative_ontology:cs_authority_grounding('d181dd4a-3b91-4e9e-a5af-f0c171c43492', extraction).
narrative_ontology:cs_interpretation_layer_present('d181dd4a-3b91-4e9e-a5af-f0c171c43492').
narrative_ontology:cs_reading_relation('d181dd4a-3b91-4e9e-a5af-f0c171c43492', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('d181dd4a-3b91-4e9e-a5af-f0c171c43492', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('d181dd4a-3b91-4e9e-a5af-f0c171c43492', foundational, state_decree_sufficient_for_displacement).
narrative_ontology:cs_axiom_status(state_decree_sufficient_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('d181dd4a-3b91-4e9e-a5af-f0c171c43492', state_decree_sufficient_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('d181dd4a-3b91-4e9e-a5af-f0c171c43492', secondary, compliance_follows_from_legal_mandate).
narrative_ontology:cs_axiom_status(compliance_follows_from_legal_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d181dd4a-3b91-4e9e-a5af-f0c171c43492', compliance_follows_from_legal_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('d181dd4a-3b91-4e9e-a5af-f0c171c43492', decree_authority_legitimacy).
narrative_ontology:cs_drift_state('d181dd4a-3b91-4e9e-a5af-f0c171c43492', generational_enforcement_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d181dd4a-3b91-4e9e-a5af-f0c171c43492', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_elite).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_officials).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, reformist_intelligentsia).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the decree mandating displacement of prior practice (calendar reform, dress code, language standardization, religious observance rules). Justifies the mandate as necessary for state cohesion, modernization, and uniform governance. Deploys enforcement machinery (inspectors, penalties, educational campaigns). Benefits by advancing a unified national culture and demonstrating state authority over social practice. The reading asserts that decree authority alone suffices — compliance follows from legal mandate, not from internalization or voluntary adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Bear the direct costs of practice displacement: economic disruption (altered planting/harvest cycles with calendar reform), social dislocation (dress code enforcement creates status degradation in community gatherings), identity erosion (religious or linguistic practice abandonment). Have minimal voice in decree formation and enforcement procedures. Subject to inspection, fines, public shaming, or educational coercion. Exit options are structured away: migration is prohibitively costly, the decree covers all governed territory, and resistance invokes punishment.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_communities, payer,
    powerless, generational, trapped, regional).

% Lose professional standing and economic livelihood when prior practices are legally abolished. Religious leaders, almanac keepers, traditional healers, and craft masters see their authority and income streams crimped by decree. Their identity is fused to the practice — exiting is not merely retraining but loss of self-concept and community position. They are excluded from decree-drafting; their objections are framed as resistance to modernization rather than legitimate expertise.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practitioners, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_practitioners, excluded).

% Gain from the decree's imposition: demonstrates state capacity and legitimacy ("the state can reshape society by fiat"), enables uniform administration and resource extraction, builds infrastructure for surveillance and control. They internalize the modernization narrative; the decree aligns with their interests. They have exit options (can move between regions, can reinterpret the decree) but hold no incentive to do so.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_elite, beneficiary,
    institutional, generational, arbitrage, national).

% Execute the decree and extract rents from selective enforcement: inspectors collect bribes for overlooking violations, teachers gain authority over linguistic practice, police gain grounds for detention and fines. They benefit both from the decree's legitimation of their authority and from the asymmetry between the rule and its enforcement — inconsistent application creates opportunities for extortion and favor-granting.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_officials, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_officials, agenda_setter).

% Ideological architects and public-facing advocates of the decree. Gain prestige, publishing opportunities, government advisory positions, and the psychological reward of believing they are advancing civilization. They internalize deeply the modernization narrative and may sincerely believe decree-driven displacement works without bottom-up adoption. They are not coerced; they are motivated and rewarded.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, reformist_intelligentsia, beneficiary,
    moderate, biographical, mobile, national).

% Diplomatic, academic, and press entities that document and interpret the decree's implementation. Record compliance rates, document reported abuses, compare this decree to similar impositions in other states. They take no direct position but their reporting shapes subsequent policy arguments and the narrative of success or failure.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, international_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_elite).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies social practice across a fragmented region under a single standardized template (calendar, dress, language, religious observance). Reduces transaction costs for inter-regional trade, administration, and military coordination. Creates a uniform citizen identity that the state can address as a coherent entity rather than negotiating with multiple local authorities.
% TRANSFER_FUNCTION: Transfers authority over cultural practice from local communities and traditional authorities to the state apparatus. Converts formerly legitimate local knowledge (traditional calendars, sartorial norms, linguistic registers) into deviance. Extracts compliance labor (learning new practices, suppressing old ones, monitoring neighbors) and creates economic rents captured by enforcement officials and urban beneficiaries.
% ABSENT_VOICES: Rural communities and traditional practitioners are structurally excluded: they have no seat in decree formulation, no right of veto, and their objections are pre-labeled 'resistance to modernization' rather than heard as legitimate expertise or cultural claims. The prior practice's own logic cannot speak; its displacement is a unilateral act.
% DISAPPEARANCE_RATIONALE: If the decree were suddenly rescinded, rural communities would revert toward prior practices within weeks or months — the costs of maintained suppression would evaporate, identity-locked practitioners would re-establish their authority, and the coordination function itself would fragment back into regional variation. The state's unified administrative apparatus would face re-negotiation with multiple local authorities. The decree's disappearance is the removal of a coercive structure, not the loss of a function that communities genuinely internalized and prefer.
% FOUNDING_PROBLEM: Regional fragmentation and administrative incoherence: the state faced multiple overlapping calendars (affecting tax collection and conscription), incompatible dress codes (creating status ambiguity), mutually unintelligible dialects (obstructing central commands). The decree was designed to solve this administrative chaos by fiat.
% FOUNDING_PROBLEM_CORROBORATION: State administrators attest the founding problem was acute and the decree necessary. However, historians of the same period and anthropologists who studied the region document that inter-regional trade and military coordination HAD functioned under prior systems of translation and negotiation (merchants maintained multiple calendars, military commands used lingua francas). The administrative chaos was less severe than reformers claimed; the decree was partly addressing a real problem and partly imposing a preferred cultural form that benefited the state's own ideology and rent-extraction apparatus. The corroboration from outside the benefiting parties (academic historians, local community oral histories) contests the severity framing.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.72) is high because the decree transfers authority and economic rents from local systems to state and enforcement apparatus, without compensation or consultation. Suppression is higher still (0.78) because the constraint persists only through active coercion — inspection, penalties, surveillance, and educational mandates maintain compliance against resistance. Theater ratio (0.41) is moderate: the decree has a real coordination function (unifying administrative practice), but an increasing share of enforcement activity defends cultural displacement rather than solving genuine administrative problems. The measurement series shows extractiveness rising from 0.48 to 0.72 over 40 years as the enforcement apparatus matures and rents accumulate; suppression requirement rises similarly (0.62 to 0.78), indicating the constraint requires increasing coercive investment to maintain as resistance persists. Theater ratio also rises (0.22 to 0.41) — over time, enforcement becomes more performative (publicized punishments, elaborate compliance theater) relative to functional necessity, suggesting Goodhart drift as enforcement metrics replace actual cultural integration goals.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state apparatus) experiences the constraint as a legitimacy demonstration and administrative solution: decree → compliance → unified state. The payer seats (rural communities, traditional practitioners) experience it as coercive displacement: decree → enforcement costs + identity loss + economic disruption. The measurement series captures this divergence in diachronic form: resistance drops from 0.68 (individual level, t0) to 0.54 (t40) as coercive pressure exhausts visible opposition, but organizational and class-level resistance remains elevated (0.48 and 0.61 at t40), indicating the constraint requires permanent enforcement to hold. This is not internalization; it is suppression holding a line that would break if enforcement were withdrawn. The engine's per-seat computation of constraint type will show the agenda-setter computing 'coordination success' while the payer seats compute 'extraction under coercion'.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus d ≈ 0.1 (beneficiary: collects authority and rents, holds analytical exit options, arbitrage mobility). Rural communities d ≈ 0.95 (targets: bear costs, trapped exit, identity-locked). Traditional practitioners d ≈ 0.88 (targets: identity-locked, lose livelihood). Urban elites d ≈ 0.15 (beneficiaries: internalize agenda, constrained but aligned). Enforcement officials d ≈ 0.25 (hybrid: benefit from enforcement but also constrained by need to maintain coercive machinery). These directionalities are derived from the beneficiary/victim declarations and exit-option profiles, not overridden. The suppression is a structural property (coercive, not internalized) independent of these directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in the sense that the founding problem is dead but the arrangement persists. Rather, this reading (exogenous override) asserts the founding problem IS live (administrative coherence is still needed) but contests whether decree-driven displacement is the right solution. The sibling readings contest this: endogenous_climb_reading asserts the founding problem requires internalization and decree alone fails; hybrid_scaffolding_reading asserts partial displacement needs ideological reinforcement. The mandatrophy emerges not from function decay but from structural mismatch — the reading claims decree suffices, but the measurement series shows suppression rising as resistance persists, suggesting decree-alone does NOT suffice and the arrangement is increasingly theatrical. A future reclassification omega addresses this: if this reading is correct, suppression should remain flat or decline (coercion pays off); if endogenous_climb or hybrid_scaffolding is correct, suppression rises and never plateaus (decree cannot hold without internalization or scaffolding).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_coercion_suppression,
    'Is the measured suppression structural (external barriers, enforcement machinery) or internalized (the target population has absorbed the new practice as legitimate)?',
    'Post-enforcement withdrawal: if suppression is internalized, the new practice persists after enforcement ends; if structural, the population reverts to prior practices. Alternatively: survey/oral history data distinguishing between fear-based compliance and preference-based acceptance.',
    'If suppression is internalized, the reading (exogenous override succeeds) is supported and the constraint approaches rope-type function. If suppression remains structural, the reading is under pressure and endogenous_climb or hybrid_scaffolding readings gain support — the decree requires permanent coercive maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_coercion_suppression, empirical, 'Whether measured suppression reflects internalized acceptance or coercive hold.').

omega_variable(
    coordination_function_necessity,
    'Was the decree''s coordination function (unified calendar, standardized dress, common language) genuinely necessary for state administrative coherence, or was it partly or wholly motivated by cultural homogenization ideology benefiting the reform elite?',
    'Comparative historical analysis: regions that achieved administrative coherence via negotiated multi-system translation without decree enforcement; archival evidence of the severity and urgency of the ''problem'' decree was meant to solve.',
    'If the function was genuinely necessary, the constraint approaches rope-type and the reading gains support. If motivation was partly ideological rent-seeking, the constraint is snare-type and hybrid_scaffolding reading gains support (the scaffolding is ideological, not functional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether coordination necessity justifies the decree or ideology drove it.').

omega_variable(
    reading_foreclosure_test,
    'Does the exogenous override reading''s core axiom (state_decree_sufficient_for_displacement) logically foreclose the endogenous_climb reading''s axiom (internalization_required_for_displacement), or do both remain live positions for different constituencies?',
    'Logical analysis: can a single observer/actor hold both ''decree suffices'' AND ''internalization is required''? (Answer: no — they are contradictory claims about the same phenomenon.) But can different parties hold them in an ongoing dispute? (Answer: yes — one party claims decree works, another claims it doesn''t.)',
    'If foreclosure is detected, the reading relation should be ''forecloses''; if not, ''coexists_with''. This affects the engine''s conjunctive treatment of the kernel''s constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Logical structure of the axiom pair (sufficiency vs. necessity).').

omega_variable(
    theater_ratio_interpretation,
    'Does rising theater_ratio (0.22 → 0.41) indicate Goodhart drift (enforcement metrics replacing functional goals) or increasing performative legitimation (the state is consolidating ideological buy-in)?',
    'Institutional analysis: what activities are classified as ''enforcement'' over the interval? Are they functional (genuine compliance verification) or performative (public punishments, educational spectacles, ceremonial affirmations of decree authority)? Ratio of functional to performative enforcement activity.',
    'If Goodhart drift, the constraint is degrading toward piton-type and the reading''s core claim is weakening. If performative legitimation, the constraint is approaching rope-type with internalization acceleration — hybrid_scaffolding reading is gaining support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Whether rising theater reflects decay of functional enforcement or accumulation of ideological reinforcement.').

omega_variable(
    kernel_reading_contest_framing,
    'Is the contest between readings (exogenous override vs. endogenous climb vs. hybrid scaffolding) a genuine logical disagreement about state capacity, or a descriptive contest about what ACTUALLY happened in specific cases?',
    'Meta-reading of the historical record: do the three readings make falsifiable claims about measurable phenomena (compliance rates, persistence after enforcement ends, ideological adoption metrics), or do they make irreducibly normative claims about what counts as ''legitimate'' displacement?',
    'If logical, one reading may foreclose another (operator ruling 2026-06-07: rare but real). If descriptive, all three remain live — they describe different historical trajectories under different conditions. This determines the structure of reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Whether readings are logical contradictions or empirical alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t5, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(legi_tr_t5, observed).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t15, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(legi_tr_t15, observed).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(legi_tr_t25, observed).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(legi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t5, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(legi_be_t5, observed).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t15, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(legi_be_t15, observed).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(legi_be_t25, observed).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(legi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t5, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(legi_su_t5, observed).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t15, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement_basis(legi_su_t15, observed).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(legi_su_t25, observed).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(legi_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(legi_grid_01, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(legi_grid_02, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(class), 40, 0.63).
narrative_ontology:measurement(legi_grid_03, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(individual), 0, 0.71).
narrative_ontology:measurement(legi_grid_04, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(individual), 40, 0.74).
narrative_ontology:measurement(legi_grid_05, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(organizational), 0, 0.64).
narrative_ontology:measurement(legi_grid_06, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(legi_grid_07, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(legi_grid_08, legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse(structural), 40, 0.62).
narrative_ontology:measurement(legi_grid_09, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(legi_grid_10, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(class), 40, 0.61).
narrative_ontology:measurement(legi_grid_11, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(legi_grid_12, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(individual), 40, 0.54).
narrative_ontology:measurement(legi_grid_13, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(legi_grid_14, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(organizational), 40, 0.48).
narrative_ontology:measurement(legi_grid_15, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(structural), 0, 0.41).
narrative_ontology:measurement(legi_grid_16, legitimacy_of_imposed_practice__exogenous_override_reading, resistance(structural), 40, 0.31).
narrative_ontology:measurement(legi_grid_17, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(class), 0, 0.54).
narrative_ontology:measurement(legi_grid_18, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(class), 40, 0.62).
narrative_ontology:measurement(legi_grid_19, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(individual), 0, 0.59).
narrative_ontology:measurement(legi_grid_20, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(individual), 40, 0.67).
narrative_ontology:measurement(legi_grid_21, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(organizational), 0, 0.41).
narrative_ontology:measurement(legi_grid_22, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(organizational), 40, 0.51).
narrative_ontology:measurement(legi_grid_23, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(structural), 0, 0.38).
narrative_ontology:measurement(legi_grid_24, legitimacy_of_imposed_practice__exogenous_override_reading, stakes_inflation(structural), 40, 0.48).
narrative_ontology:measurement(legi_grid_25, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(legi_grid_26, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(class), 40, 0.68).
narrative_ontology:measurement(legi_grid_27, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(individual), 0, 0.71).
narrative_ontology:measurement(legi_grid_28, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(individual), 40, 0.76).
narrative_ontology:measurement(legi_grid_29, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(legi_grid_30, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(organizational), 40, 0.74).
narrative_ontology:measurement(legi_grid_31, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(structural), 0, 0.51).
narrative_ontology:measurement(legi_grid_32, legitimacy_of_imposed_practice__exogenous_override_reading, suppression(structural), 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest over the legitimacy and mechanism of practice displacement in state formation. The exogenous override reading (this file) asserts decree authority suffices. The endogenous climb reading contests this by asserting internalization is necessary. The hybrid scaffolding reading proposes a synthesis: decree + ideological reinforcement achieve partial displacement. All three stories share the same referent (the standing arrangement of practice displacement), but author divergent epsilon values reflecting their reading-indexed assessment of how much extraction the arrangement embodies. Network links enable the engine to flag which readings would shift terminal classification if the empirical record confirmed one over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__exogenous_override_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
