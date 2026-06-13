% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems (Balanced Coexistence Reading)
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   One Country, Two Systems is a constitutional framework for Hong Kong's
 *   relationship to the PRC that attempts to preserve Hong Kong's legal and
 *   civil-society distinctness while maintaining PRC territorial sovereignty.
 *   This constraint instantiates the BALANCED COEXISTENCE reading: neither
 *   sovereignty nor autonomy is absolute; boundaries are negotiated through
 *   political accommodation rather than resolved by legal supremacy; periodic
 *   crises trigger renegotiation of who controls what. The measurement series
 *   document the framework's empirical trajectory from 1997 (t=0) through
 *   2024 (t=27): extractiveness and suppression both rise, with theater-ratio
 *   rising more slowly, indicating the constraint operates increasingly as
 *   enforced boundary maintenance rather than genuine coordination. The
 *   claim/metric independence is deliberate: the constraint is CLAIMED as a
 *   tangled rope (coordination between PRC sovereignty and Hong Kong autonomy
 *   + asymmetric extraction from autonomy advocates and dissidents) while the
 *   metrics show medium extractiveness and rising suppression — the engine
 *   measures this structure without reconciling it to a pre-judged outcome.
 *
 * KEY AGENTS:
 *   - Hong Kong government: institutional agenda-setter (trapped exit) — administers the framework and renegotiates boundaries
 *   - Hong Kong civil society: organized beneficiary-and-payer (constrained exit) — retains autonomy but faces perpetual renegotiation crises
 *   - Mainland PRC state: institutional beneficiary (arbitrage exit) — preserves sovereignty and can reinterpret unilaterally
 *   - Hong Kong autonomy advocates: moderate payer (identity-locked exit) — experience the framework's renegotiability as delegitimation of Hong Kong identity
 *   - Mainland dissidents: powerless payer (trapped exit) — excluded from Hong Kong protections by the One Country framing
 *   - International commerce networks: institutional beneficiary (mobile exit) — depend on Hong Kong's autonomy-backed financial-center status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.42).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.38).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems (Balanced Coexistence Reading)").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '3eb2da52-af4e-4a08-8da2-2499336ef407').
narrative_ontology:cs_kernel_codification('3eb2da52-af4e-4a08-8da2-2499336ef407', fixed_text).
narrative_ontology:cs_authority_grounding('3eb2da52-af4e-4a08-8da2-2499336ef407', extraction).
narrative_ontology:cs_interpretation_layer_present('3eb2da52-af4e-4a08-8da2-2499336ef407').
narrative_ontology:cs_reading_relation('3eb2da52-af4e-4a08-8da2-2499336ef407', one_country_two_systems_framework__sovereignty_primacy_reading, influences).
narrative_ontology:cs_reading_relation('3eb2da52-af4e-4a08-8da2-2499336ef407', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('3eb2da52-af4e-4a08-8da2-2499336ef407', foundational, bilateral_boundary_negotiation).
narrative_ontology:cs_axiom_status(bilateral_boundary_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('3eb2da52-af4e-4a08-8da2-2499336ef407', bilateral_boundary_negotiation, conventional).
narrative_ontology:cs_axiom('3eb2da52-af4e-4a08-8da2-2499336ef407', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, overridden).
narrative_ontology:cs_axiom_grounding('3eb2da52-af4e-4a08-8da2-2499336ef407', neither_sovereignty_nor_autonomy_absolute, deontological).
narrative_ontology:cs_reference_frame('3eb2da52-af4e-4a08-8da2-2499336ef407', negotiated_boundary_accommodation).
narrative_ontology:cs_drift_state('3eb2da52-af4e-4a08-8da2-2499336ef407', post_2019_security_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3eb2da52-af4e-4a08-8da2-2499336ef407', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, mainland_prc_state).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_autonomy_advocates).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, mainland_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, international_commerce_networks).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the framework by accepting delegated autonomy in civil administration, common law courts, and economic policy while acknowledging PRC sovereignty over defense and foreign affairs. Must navigate periodic crises by renegotiating boundaries through political accommodation with Beijing. Bears the cost of managing constituencies with conflicting loyalty claims.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government, agenda_setter,
    institutional, generational, trapped, regional).

% Retains substantive civil liberties, independent judiciary, and economic autonomy that would not exist under full mainland integration. Operates under the constraint that fundamental law and basic law can be reinterpreted or amended; periodic security crises (2019–2020) demonstrably trigger renegotiation of the autonomy boundary, not stable legal limits. Leverage exists through international economic dependencies and diaspora networks, but depends on Beijing's strategic interest in maintaining the arrangement.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_civil_society, payer).

% Preserves nominal territorial sovereignty and the right to override Hong Kong law on national security grounds, while avoiding the direct administrative burden and international opacity cost of full integration. Maintains the arrangement because it generates capital flows, facilitates international commerce, and legitimates the One Country framing against autonomy-focused challenges. Can renegotiate boundaries unilaterally through security reinterpretation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_prc_state, beneficiary,
    institutional, civilizational, arbitrage, national).

% Belief that Hong Kong's distinct legal tradition and civil liberties form a non-negotiable identity; treating the boundaries as perpetually renegotiable is experienced as delegitimation of that identity. Face periodic crises in which the constraint is reinterpreted against them (2019–2020 security law, 2021 electoral overhaul) with no unilateral veto. Cannot credibly exit (Hong Kong residency, professional investment, family ties) and cannot appeal to a higher legal authority that would constrain Beijing's reinterpretation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_autonomy_advocates, payer,
    moderate, biographical, identity_locked, regional).

% The framework's commitment to 'One Country' means Hong Kong courts cannot shelter mainland citizens from PRC law; extradition frameworks and the 2019 crisis demonstrated that civil liberties protections in Hong Kong do not extend to mainland dissidents seeking refuge. Subject to the constraint's boundaries without participation in renegotiation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_dissidents, payer,
    powerless, immediate, trapped, national).

% Rely on Hong Kong's common law courts, dollar-denominated contracts, and neutral-arbiter status to conduct China-related business; the arrangement's stability is the public good. Derive rents from the constraint's persistence; their investment and regulatory participation reinforces the framework's legitimacy and creates international cost to abandonment.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_commerce_networks, beneficiary,
    institutional, biographical, mobile, global).

% Formally recognizes PRC sovereignty while maintaining Hong Kong-specific tariff and sanctions policies conditional on autonomy preservation; uses the constraint as a pressure point in broader US-China relations. Monitors reinterpretation episodes as tests of Beijing's commitment to negotiated rather than unilateral boundary-setting.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, united_states_government, observer,
    institutional, generational, arbitrage, global).

% Would argue that substantive autonomy requires legal supremacy of Hong Kong courts over PRC reinterpretation — a reading incompatible with the balanced-coexistence framing. Not seated at the renegotiation table; their advocacy takes place through foreign legislatures, NGOs, and advisory roles but carries no power in the constraint's actual operation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_rule_of_law_advocates, excluded,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, mainland_prc_state).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the integration problem: how to incorporate Hong Kong into the PRC while preserving the institutional, legal, and economic distinctness that generates Hong Kong's utility as a global financial center and prevents massive flight and capital withdrawal. Permits two legal systems with different legitimacy bases (PRC constitutional supremacy and Hong Kong common law) to coexist without requiring resolution of which supersedes the other.
% TRANSFER_FUNCTION: Moves capital, tax revenues, and legitimacy from Hong Kong (as a globally-connected, autonomy-preserving jurisdiction) to the mainland state apparatus; moves security guarantees and international commerce leverage from mainland PRC to Hong Kong's civil society. The constraint extracts from autonomy advocates (who bear the cost of perpetual renegotiability) and from mainland dissidents (who cannot access Hong Kong protections).
% ABSENT_VOICES: International rule-of-law advocates who would argue for legal entrenchment of autonomy (incompatible with the balanced reading); Hong Kong youth radicalized in 2019–2020 who experienced the constraint as a cover for unilateral reinterpretation rather than good-faith negotiation; mainland Han minorities and Tibetan/Uyghur separatists whose claims on autonomy are structurally excluded by the One Country framing.
% DISAPPEARANCE_RATIONALE: If the negotiated framework collapsed, Hong Kong would either integrate fully into PRC governance (ending the autonomy that generates financial-center status and producing capital flight and brain drain) or declare functional independence (forcing military confrontation and international sanctions). International commerce would reorganize around alternative financial hubs; the PRC would sacrifice the arrangement's legitimacy dividend and face intensified international isolation on human rights grounds.
% FOUNDING_PROBLEM: Hong Kong's 1997 handover required reconciling British-legacy liberal institutions with PRC territorial sovereignty without triggering immediate flight of capital and professional expertise. The One Country, Two Systems framework was designed to preserve Hong Kong's legal and economic distinctness as a transitional accommodation while maintaining PRC sovereign authority and avoiding explicit federalism (which would contradict PRC constitutional theory).
% FOUNDING_PROBLEM_CORROBORATION: The PRC government attests the founding problem remains live: Hong Kong requires continued autonomy to function as a global financial center, and precipitous integration would damage that utility. Hong Kong civil society and international commercial actors attest the founding problem has been substantially addressed by the arrangement's institutional success. Autonomy advocates and international observers dispute whether the problem persists or whether its apparent persistence is cover for extracting Hong Kong's distinctness while preserving mainland control — legislative testimony from autonomy advocates, economic analysis from international finance, and mainland dissident testimony all corroborate the shifted-function reading (the arrangement now extracts more than it coordinates).
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).
:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.42 at interval end) because the constraint solves a genuine coordination problem (how to preserve Hong Kong's utility while integrating it politically) but increasingly operates by suppressing autonomy claims rather than negotiating them. Suppression is lower than extractiveness (0.38) because the framework does permit substantial institutional autonomy; the suppression tracks the growing enforcement machinery required to maintain boundaries as Beijing's security priorities intensify. Theater is low-moderate (0.28) because the constraint retains functional coordination (common law courts, dollar peg, international arbitration all operate) but an increasing share of enforcement is directed at preventing exits (2019–2020 security law, 2021 electoral overhaul) rather than administering the agreed boundaries. The measurement series show a sharp inflection point around t=9 (corresponding to 2005–2008 reform pushback and pre-2008 Beijing Olympics security tightening) where extractiveness accelerates; another inflection at t=15 (2012, Xi consolidation of power) where suppression requirement jumps; and a final sustained rise through t=27 (2019–2024, post-protest security apparatus). The one shared time grid ensures every metric is authored at every examined point; no metric is missing from any row.
 *
 * PERSPECTIVAL GAP:
 *   The mainland PRC state and Hong Kong government should compute as moderate extractors from their seats (coordination burden offset by rent collection for state, negotiating capacity preserved for HK government). Hong Kong civil society should compute as paying significant extraction despite beneficiary status (genuine autonomy benefit offset by perpetual renegotiation threat). Hong Kong autonomy advocates should compute as high-extraction targets despite organizing to resist (identity lock prevents exit; no legal forum can constrain reinterpretation). Mainland dissidents should compute as pure extraction targets (no beneficiary component; trapped with no appeal). The engine derives these from the declared power, exit, beneficiary/victim structure, and directionality — the claim does not pre-judge the computation.
 *
 * DIRECTIONALITY LOGIC:
 *   The mainland PRC state is a full beneficiary (d near 0.0): preserves sovereignty, collects legitimacy and capital flows, retains unilateral reinterpretation power, faces no enforcement pressure. Hong Kong government is symmetric-to-slightly-extracting (d near 0.5): gains administrative autonomy and international credibility but loses the power to resist Beijing's reinterpretation of boundaries; trapped exit prevents walking away. Hong Kong civil society is slightly-extracting (d near 0.6): genuinely benefits from the arrangement's civil liberties and economic autonomy but bears the cost of perpetual renegotiation vulnerability and periodic crises; constrained exit (economic integration, family ties, professional investment) limits mobility. Hong Kong autonomy advocates are high-extraction targets (d near 0.9): experience the framework's renegotiability as existential threat to Hong Kong identity; identity lock is severe (professional, cultural, relational); no exit short of emigration. Mainland dissidents are full targets (d = 1.0): no beneficiary component, complete structural exclusion, trapped with no recourse. International commerce networks are beneficiaries (d near 0.2): depend on the arrangement but retain arbitrage options (can relocate to Singapore, Tokyo, Dubai if suppression rises sufficiently). Directionality overrides are not needed; the derivation chain produces accurate seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of mandatrophy resolution: the founding problem (preserving Hong Kong's utility during integration) was live and pressing in 1997; it is now contested — Beijing's priority has shifted from preserving the arrangement's legitimacy to asserting sovereignty visibly, which requires periodic reinterpretation of the autonomy boundary. The measurement series show the theater ratio rising slower than extractiveness, indicating the functional coordination is persistent (common law courts still operate, dollar peg holds, arbitration is still used) but an increasing share of the enforcement machinery is devoted to boundary suppression rather than coordination administration. This is characteristic of mandatrophy: the original mandate (create a stable, legitimacy-preserving arrangement) has been substantially achieved, but the constraint persists because both parties benefit from it (PRC gets capital flows and international legitimacy; Hong Kong gets relative autonomy) even though the organizing principle has shifted from negotiated coexistence to managed extraction. Classification as tangled rope is appropriate: genuine coordination (markets, courts, commerce) persists alongside asymmetric extraction (perpetual boundary renegotiation, security law expansion, electoral overhaul); the asymmetry is active (Beijing can unilaterally reinterpret) and requires enforcement (suppression machinery exists and has hardened).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negotiation_authenticity,
    'Are the periodic renegotiations of the autonomy boundary genuine political accommodations (both parties constrained to seek mutually acceptable solutions) or unilateral reinterpretations (Beijing sets the boundary and Hong Kong accommodates)?',
    'Examine institutional decision-making in boundary crises (2019 security law, 2021 electoral overhaul): determine whether Hong Kong''s institutions had veto power (true negotiation) or whether Beijing imposed changes against Hong Kong government objections (unilateral reinterpretation). Document testimony from negotiators on both sides; trace the causal chain from proposal to implementation.',
    'If negotiations are genuine (both parties have veto), the constraint remains balanced coexistence and should compute as medium extraction. If negotiations are unilateral (only Beijing has veto), the constraint has drifted toward sovereignty primacy and should compute as higher extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_authenticity, empirical, 'Whether boundary renegotiation is genuinely bilateral or unilaterally imposed by PRC.').

omega_variable(
    identity_lock_mechanism,
    'Is the measured suppression in Hong Kong autonomy advocates structurally imposed (legal barriers, economic dependency, geographic isolation) or significantly internalized (the advocates'' identity has fused with Hong Kong distinctness, making exit psychologically impossible even when structurally feasible)?',
    'Post-exit trajectory: survey Hong Kong residents and professionals who have emigrated; determine whether suppression persists after the enforced-boundary mechanism is removed (i.e., do they feel relieved of suppression or do they continue to experience identity displacement). Examine cultural and professional identity-fusion through interviews and historical discourse analysis.',
    'If suppression is primarily structural (external barriers), the constraint''s effective extraction ends when the barrier is removed (emigration should decompress suppression). If suppression is significantly internalized (identity fusion), the extraction persists after exit (emigrants experience ongoing identity dysphoria). Internalized suppression indicates the constraint is more extractive than the scalar structural measure suggests — the target carries the extraction with them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized suppression in Hong Kong autonomy advocates'' relationship to the framework.').

omega_variable(
    reading_foreclosure_risk,
    'Could the balanced_coexistence_reading foreclose the sovereignty_primacy_reading, or will both remain live options held by different parties?',
    'Examine Beijing''s and Hong Kong''s institutional commitment to renegotiation: if Beijing formalizes unilateral reinterpretation authority (e.g., through explicit constitutional revision superseding the Basic Law), the balanced reading is logically impossible within any single framework (foreclosed). If renegotiation remains formal (Beijing can dominate the negotiation but must maintain the fiction of accommodation), both readings coexist.',
    'Foreclosure would represent a reading-level regime change: the constraint would transition from balanced coexistence to sovereignty primacy and should be reclassified to a new constraint story. Coexistence would indicate the contested kernel remains live and the two readings compete across parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_risk, conceptual, 'Whether this reading logically forecloses the sovereignty-primacy alternative or whether both remain live readings.').

omega_variable(
    international_enforcement_leverage,
    'Can civil society retain bargaining power through international economic dependencies and diaspora networks, or is that leverage eroding as international actors deprioritize Hong Kong autonomy?',
    'Track US tariff policy, international commerce court usage, diaspora political activity, and geopolitical pressure on Hong Kong from EU and other democratic blocs. Determine whether international leverage is used to constrain Beijing''s reinterpretation or whether it is tacitly deprioritized in favor of broader US-China strategic considerations.',
    'If international leverage persists and is deployed to constrain reinterpretation, the balanced reading remains structurally viable (both parties must accommodate). If international leverage erodes or is deprioritized, Hong Kong loses a primary means of negotiating boundaries and the constraint drifts irreversibly toward sovereignty primacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_leverage, empirical, 'Whether international economic leverage can sustain Hong Kong autonomy negotiating power or whether geopolitical deprioritization erodes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(one__tr_t0, observed).
narrative_ontology:measurement(one__tr_t3, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 3, 0.12).
narrative_ontology:measurement_basis(one__tr_t3, observed).
narrative_ontology:measurement(one__tr_t9, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement_basis(one__tr_t9, observed).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(one__tr_t15, observed).
narrative_ontology:measurement(one__tr_t20, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(one__tr_t20, observed).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 27, 0.28).
narrative_ontology:measurement_basis(one__tr_t27, observed).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(one__be_t0, observed).
narrative_ontology:measurement(one__be_t3, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 3, 0.22).
narrative_ontology:measurement_basis(one__be_t3, observed).
narrative_ontology:measurement(one__be_t9, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 9, 0.28).
narrative_ontology:measurement_basis(one__be_t9, observed).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement_basis(one__be_t15, observed).
narrative_ontology:measurement(one__be_t20, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(one__be_t20, observed).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 27, 0.42).
narrative_ontology:measurement_basis(one__be_t27, observed).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(one__su_t0, observed).
narrative_ontology:measurement(one__su_t3, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 3, 0.22).
narrative_ontology:measurement_basis(one__su_t3, observed).
narrative_ontology:measurement(one__su_t9, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 9, 0.28).
narrative_ontology:measurement_basis(one__su_t9, observed).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement_basis(one__su_t15, observed).
narrative_ontology:measurement(one__su_t20, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(one__su_t20, observed).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 27, 0.38).
narrative_ontology:measurement_basis(one__su_t27, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__balanced_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_international_standing_credential).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, prc_territorial_sovereignty_legitimacy).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, offshore_finance_hub_function).

% DUAL FORMULATION NOTE:
% The one_country_two_systems_framework kernel decomposes into three structurally distinct constraints, each representing a different reading of the same fixed-text constitutional commitment. This story (balanced_coexistence_reading) assumes neither sovereignty nor autonomy is absolute and that boundaries are negotiated through political accommodation. The sovereignty_primacy_reading treats autonomy as delegated and revocable, with higher extraction and suppression. The autonomy_primacy_reading treats autonomy as treaty-guaranteed and internationally enforceable, with lower extraction and suppression. The three stories form a constraint family linked by network.affects_constraints: changes in one reading's empirical support (e.g., if unilateral reinterpretation becomes formalized, foreclosing the balanced reading) propagate to the others. The ε values differ substantially across readings because the observable used to evaluate the constraint (whether boundaries are negotiated or unilaterally imposed) differs across readings — each reading instantiates a different structural fact about how authority operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
