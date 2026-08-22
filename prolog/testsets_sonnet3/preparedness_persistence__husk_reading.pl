% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__husk_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: preparedness_persistence__husk_reading
 *   human_readable: Flood Preparedness Drill and Inspection Regime (Husk Reading)
 *   domain: disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This is the husk reading of the preparedness_persistence kernel: drills
 *   and inspections are understood as memorial performance whose
 *   institutional form has decoupled entirely from the operational competence
 *   it once certified. On this reading, the constraint presents itself with
 *   mountain-like inevitability — 'of course we drill, of course we inspect,
 *   this is simply how disaster readiness works' — while the actual
 *   capability the ritual once verified has atrophied into a checklist
 *   exercise designed for predictable, presentable outcomes rather than real
 *   stress conditions. The beneficiary is institutional legitimacy (agency
 *   leadership, the flood authority, insurers/bond markets who need a legible
 *   compliance record); the victim is the floodplain population whose actual
 *   risk exposure is undiminished by a ritual that no longer tests the
 *   conditions a real flood would present.
 *
 * KEY AGENTS:
 *   - emergency_management_agency_leadership: agenda_setter/beneficiary (institutional/arbitrage) — schedules and certifies drills, bears little cost from their decay
 *   - municipal_flood_authority: beneficiary (institutional/constrained) — legitimacy sustained by the paper record independent of underlying capability
 *   - floodplain_residents: payer (powerless/trapped) — bear the real consequence when ritual diverges from operational reality
 *   - frontline_evacuation_personnel: payer/excluded (moderate/constrained) — know the drills are scripted but have no effective channel to say so
 *   - insurance_and_bond_underwriters: beneficiary (organized/mobile) — price risk off the compliance record without auditing its operational substance
 *   - state_emergency_oversight_board: observer (institutional/analytical) — checks schedule compliance, not operational quality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_persistence__husk_reading, 0.52).
domain_priors:theater_ratio(preparedness_persistence__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(preparedness_persistence__husk_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__husk_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__husk_reading, "Flood Preparedness Drill and Inspection Regime (Husk Reading)").
narrative_ontology:topic_domain(preparedness_persistence__husk_reading, "disaster_preparedness/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__husk_reading, '578bd415-089e-47f8-aff3-054dbc6edfa1').
narrative_ontology:cs_kernel_codification('578bd415-089e-47f8-aff3-054dbc6edfa1', formalized).
narrative_ontology:cs_authority_grounding('578bd415-089e-47f8-aff3-054dbc6edfa1', extraction).
narrative_ontology:cs_interpretation_layer_present('578bd415-089e-47f8-aff3-054dbc6edfa1').
narrative_ontology:cs_reading_relation('578bd415-089e-47f8-aff3-054dbc6edfa1', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('578bd415-089e-47f8-aff3-054dbc6edfa1', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('578bd415-089e-47f8-aff3-054dbc6edfa1', foundational, certified_form_substitutes_for_verified_function).
narrative_ontology:cs_axiom_status(certified_form_substitutes_for_verified_function, holdable).
narrative_ontology:cs_axiom_grounding('578bd415-089e-47f8-aff3-054dbc6edfa1', certified_form_substitutes_for_verified_function, empirically_contingent).
narrative_ontology:cs_axiom('578bd415-089e-47f8-aff3-054dbc6edfa1', secondary, institutional_legitimacy_decouples_from_protective_outcome).
narrative_ontology:cs_axiom_status(institutional_legitimacy_decouples_from_protective_outcome, holdable).
narrative_ontology:cs_axiom_grounding('578bd415-089e-47f8-aff3-054dbc6edfa1', institutional_legitimacy_decouples_from_protective_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('578bd415-089e-47f8-aff3-054dbc6edfa1', post_disaster_rehearsal_mandate).
narrative_ontology:cs_drift_state('578bd415-089e-47f8-aff3-054dbc6edfa1', contemporary_compliance_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('578bd415-089e-47f8-aff3-054dbc6edfa1', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, emergency_management_agency_leadership).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, municipal_flood_authority).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, floodplain_residents).
narrative_ontology:constraint_victim(preparedness_persistence__husk_reading, frontline_evacuation_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__husk_reading, insurance_and_bond_underwriters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schedules and certifies the annual drill calendar, signs off on inspection checklists, and reports compliance rates to oversight bodies and insurers. Could redesign drills to test actual failure modes but the cost of admitting current drills are theater — reputational, budgetary, legal — exceeds what leadership individually bears when the flood has not yet come. Rotates out on a timescale shorter than most flood-return periods, so the consequences of atrophy land on a successor.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, emergency_management_agency_leadership, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, emergency_management_agency_leadership, beneficiary).

% Cites completed drills and passed inspections as evidence of due diligence when negotiating budgets, insurance terms, and political cover after near-misses. The paperwork trail is real and audit-legible even when the underlying capability it certifies has decayed; the authority's legitimacy is propped up by the record independent of whether the record still tracks readiness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, municipal_flood_authority, beneficiary,
    institutional, generational, constrained, regional).

% Live behind levees and in evacuation zones whose maintenance is certified by the same inspection regime. Cannot independently verify whether an inspection tested real structural capacity or reviewed old paperwork, and cannot relocate away from flood risk on short notice. When the drill fails to reflect a real emergency's conditions, they bear the consequence directly — in casualties, property loss, or evacuation chaos — while the certifying agency bears reputational cost only.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, floodplain_residents, payer,
    powerless, biographical, trapped, local).

% Execute the drills as scripted, often against unrealistic scenarios (fair weather, daytime, pre-notified routes) that management selects for predictable, presentable outcomes. Many privately know the drills don't stress real failure conditions — night flooding, road washout, non-English speakers, mobility-impaired residents — but raising this in after-action reports has little career upside and is frequently filtered out before it reaches leadership.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, frontline_evacuation_personnel, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__husk_reading, frontline_evacuation_personnel, excluded).

% Price flood risk and set municipal bond terms partly on the basis of documented compliance with drill and inspection schedules. Benefit from a legible, auditable paper record regardless of whether it reflects operational reality; have no strong incentive to investigate the gap because the record satisfies their own downstream reporting requirements.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, insurance_and_bond_underwriters, beneficiary,
    organized, biographical, mobile, national).

% Reviews compliance filings and occasionally conducts audits, but typically checks that drills occurred on schedule rather than whether they tested anything meaningful. Has authority to mandate substantive reform but limited capacity to observe operational quality directly, and relies on the same self-reported records the local authority produces.
narrative_ontology:constraint_stakeholder(preparedness_persistence__husk_reading, state_emergency_oversight_board, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_persistence__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: standardize evacuation procedures and verify levee/pump/gate integrity across multiple agencies and jurisdictions so that, in a real flood, response would be coordinated rather than improvised.
% TRANSFER_FUNCTION: Moves legitimacy and continued budget/insurance access to the certifying agencies and their leadership, in exchange for a paperwork record; moves real risk exposure to floodplain residents, who receive the appearance of readiness without the underlying operational capacity the paperwork claims to certify.
% ABSENT_VOICES: Frontline personnel who know the drills are scripted for predictable success are structurally filtered out of after-action reporting. Floodplain residents, especially those without English fluency or mobility, are never consulted on drill design and have no channel to flag that evacuation routes assume conditions unlike a real flood night.
% DISAPPEARANCE_RATIONALE: If the drill and inspection regime vanished overnight, the paperwork-legitimacy function (insurance terms, bond ratings, political cover) would visibly collapse immediately — that arrangement clearly depends on it. Whether actual flood outcomes would be materially different is contested: on this reading, operational competence has already atrophied behind the paperwork, so removing the ritual might change little for outcomes while changing everything for who is exposed as unprepared.
% FOUNDING_PROBLEM: Historical flood disasters where uncoordinated, ad hoc evacuation and unverified infrastructure caused avoidable deaths; drills and inspections were built to ensure procedures were rehearsed and infrastructure verified before the next flood, not merely documented.
% FOUNDING_PROBLEM_CORROBORATION: After-action reviews from independent disaster researchers and journalists following recent flood events in comparable jurisdictions have documented drills that assumed daytime, dry-road, English-speaking, ambulatory populations — conditions absent in the actual floods that followed. No entity outside the certifying agencies and their insurers attests that the drills still test the conditions a real flood would present; the corroboration for competence comes only from the benefiting parties' own compliance filings.
narrative_ontology:disappearance_verdict(preparedness_persistence__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_persistence__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__husk_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is authored high and rising (0.35 to 0.81) because the story's central claim is exactly that form has decoupled from function over time — drills increasingly test conditions selected for successful, presentable completion rather than realistic flood scenarios. Extractiveness rises in parallel (0.30 to 0.68) because the paperwork-legitimacy function increasingly extracts institutional and financial value (budget justification, favorable bond terms, political cover) from a ritual whose protective value to residents has not kept pace. Suppression is moderate (0.52), not extreme: no one is coercively silenced, but frontline reporting is structurally filtered and resident input is never solicited, which functions as a softer, non-coercive suppression of the information that would reveal the gap. Accessibility collapse is moderate-high (0.6): once the drill schedule and inspection checklist exist and are treated as sufficient, alternative approaches (adversarial red-team drills, resident-designed evacuation tests) become institutionally invisible rather than actively forbidden. Resistance is low (0.35) precisely because the ritual's mountain-like self-presentation forecloses the perception that resistance is even warranted — you cannot resist what looks like settled, natural procedure.
 *
 * DIRECTIONALITY LOGIC:
 *   Agency leadership and the flood authority sit near the beneficiary end: they collect legitimacy, budget continuity, and political cover from the compliance record with minimal personal exposure to flood outcomes given short leadership tenures relative to flood-return periods. Insurers and bond underwriters likewise benefit from a legible record they have no strong incentive to audit substantively. Floodplain residents sit at the target end: trapped exit, powerless, and directly exposed to the gap between certified and actual readiness. Frontline personnel are structurally intermediate — they pay a professional cost (their honest observations are filtered) without receiving the institutional benefit leadership captures, and they are excluded from redesigning the exercises whose flaws they are best positioned to see.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — uncoordinated, unrehearsed, unverified flood response causing avoidable deaths — is read here as dead: the mechanism built to solve it has persisted long past the point where it demonstrably still solves it, corroborated by outside after-action analysis, not by the certifying agencies themselves. This is the mandatrophy signature: an arrangement whose mandate (verify real readiness) has been quietly substituted by a proxy mandate (produce a compliant record) that is easier to satisfy and equally sufficient for the institutional purposes (legitimacy, insurance, budget) that now sustain it. Classifying this as piton rather than mountain prevents the constraint from being mistaken for an irreducible feature of disaster governance; classifying it as piton rather than snare acknowledges that no single party is extracting concentrated rents — the extraction is diffuse institutional self-protection, not capture by an identifiable profiteer, which is exactly the piton signature the kernel context flags as the expected structural delta.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_empirical_discriminator,
    'Is the drill and inspection regime genuinely a husk (form without function) or is it live exercised knowledge (the competence_reading) — what observable would discriminate between the two readings for this specific jurisdiction?',
    'Compare drill scenario design against actual flood event conditions from the historical record (time of day, weather, road conditions, population mobility/language profile) across multiple cycles; a persistent mismatch between drilled and real conditions favors the husk reading, while adaptive scenario design that incorporates prior flood lessons favors the competence reading.',
    'If discriminating evidence shows drills adapting to real flood lessons over time, this story''s classification should shift toward the competence_reading''s structure (rope-like, genuine coordination) rather than piton; if the mismatch persists or widens, it corroborates the husk reading and would predict continued extraction growth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_empirical_discriminator, empirical, 'Whether the husk diagnosis is empirically distinguishable from the competence reading for this jurisdiction.').

omega_variable(
    husk_hybrid_boundary_location,
    'Where exactly does the husk/competent boundary sit — is it possible that engineering inspection (levees, pumps, gates) remains genuinely competent even while evacuation drills have hollowed out, as the hybrid_reading claims, making this story''s full-husk diagnosis too broad?',
    'Disaggregate the compliance record by component: compare physical infrastructure inspection findings (measurable engineering metrics — structural load tests, pump throughput tests) against evacuation drill after-action reports (procedural, harder to falsify) to see if one component shows genuine competence while the other shows atrophy.',
    'If infrastructure inspection is found to be substantively rigorous while only evacuation drills are theatrical, the more accurate reading for this jurisdiction may be the hybrid_reading rather than this full husk_reading — this story would then be over-generalizing atrophy across components that behave differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_hybrid_boundary_location, conceptual, 'Whether full-husk is the right granularity or whether atrophy is component-specific, as the hybrid reading claims.').

omega_variable(
    leadership_awareness_of_atrophy,
    'Do agency leaders who schedule and certify these drills privately know the exercises no longer test real conditions, or do they sincerely believe the drills remain operationally meaningful?',
    'Internal communications discovery, whistleblower testimony, or comparison of internal risk assessments against public compliance filings for divergence.',
    'Sincere belief in the drills'' adequacy would support classifying this as institutional self-deception (a piton with no deliberate deceiver); documented internal awareness of the gap without correction would push the classification toward snare-like concentrated culpability rather than diffuse piton drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leadership_awareness_of_atrophy, empirical, 'Whether the husk persists through sincere belief or knowing neglect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prep_tr_t4, preparedness_persistence__husk_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(prep_tr_t8, preparedness_persistence__husk_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement(prep_tr_t12, preparedness_persistence__husk_reading, theater_ratio, 12, 0.66).
narrative_ontology:measurement(prep_tr_t16, preparedness_persistence__husk_reading, theater_ratio, 16, 0.72).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__husk_reading, theater_ratio, 20, 0.77).
narrative_ontology:measurement(prep_tr_t24, preparedness_persistence__husk_reading, theater_ratio, 24, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__husk_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_be_t4, preparedness_persistence__husk_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(prep_be_t8, preparedness_persistence__husk_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(prep_be_t12, preparedness_persistence__husk_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(prep_be_t16, preparedness_persistence__husk_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__husk_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(prep_be_t24, preparedness_persistence__husk_reading, base_extractiveness, 24, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_persistence__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__husk_reading, 0.1).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__husk_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the preparedness_persistence kernel. competence_reading holds drills are live exercised knowledge (low ε, rope-like); husk_reading (this story) holds form has decoupled from function (high ε, piton); hybrid_reading holds atrophy is component-specific (mixed ε, mixed classification across sub-functions). Each reading is authored as a separate constraint with its own stable ε per the ε-invariance principle; they are linked here rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
