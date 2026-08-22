% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country Two Systems — Balanced Coexistence Reading
 *   domain: constitutional/political/sovereignty
 *
 * SUMMARY:
 *   This story models the 'balanced coexistence' reading of the One Country
 *   Two Systems framework — the position that the arrangement requires
 *   continuous substantive negotiation between sovereignty and autonomy, with
 *   neither absolute, and contested boundaries resolved through political
 *   accommodation rather than legal supremacy. This reading claims the
 *   framework is a genuine coordination mechanism (tangled rope) with a real
 *   but contested coordination function, asymmetric extraction (center
 *   extracts autonomy, local elites extract economic rents, civil society
 *   pays), and active enforcement that fluctuates with political crises. The
 *   constraint is NOT a stable equilibrium but a dynamic regime of periodic
 *   renegotiation triggered by crises (2003 Article 23, 2014 Umbrella
 *   Movement, 2019 protests, 2020 NSL).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.38).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.42).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country Two Systems — Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political/sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '37088d2e-e5b2-41e5-99a7-2f17ab9b7b39').
narrative_ontology:cs_kernel_codification('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', formalized).
narrative_ontology:cs_authority_grounding('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', lineage).
narrative_ontology:cs_interpretation_layer_present('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39').
narrative_ontology:cs_reading_relation('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', neither_sovereignty_nor_autonomy_absolute, conventional).
narrative_ontology:cs_axiom('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', foundational, political_accommodation_over_legal_supremacy).
narrative_ontology:cs_axiom_status(political_accommodation_over_legal_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', political_accommodation_over_legal_supremacy, conventional).
narrative_ontology:cs_axiom('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', secondary, functional_division_of_powers_with_contested_boundaries).
narrative_ontology:cs_axiom_status(functional_division_of_powers_with_contested_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', functional_division_of_powers_with_contested_boundaries, instrumental).
narrative_ontology:cs_axiom('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', secondary, civil_society_retains_bargaining_power).
narrative_ontology:cs_axiom_status(civil_society_retains_bargaining_power, holdable).
narrative_ontology:cs_axiom_grounding('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', civil_society_retains_bargaining_power, empirically_contingent).
narrative_ontology:cs_reference_frame('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', joint_declaration_transition_framework).
narrative_ontology:cs_drift_state('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', post_national_security_law_2020, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('37088d2e-e5b2-41e5-99a7-2f17ab9b7b39', '2026-06-15T14:30:00Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elites).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, international_financial_institutions).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_prodemocracy_civil_society).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_local_courts_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_legislative_council_opposition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government_executive).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, political_accommodation_over_legal_supremacy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, functional_division_of_powers).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__balanced_coexistence_reading, dual_sovereignty_ambiguity_as_feature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the constitutional framework through the Basic Law and NPCSC interpretations. Retains ultimate authority over defense, foreign affairs, and national security definitions. Benefits from Hong Kong's economic contribution and international legitimacy while managing sovereignty claims through calibrated intervention.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_government, beneficiary).

% Administers day-to-day governance under the Basic Law. Must implement central directives while maintaining local legitimacy. Bears political costs of both Beijing's interventions and local resistance. Dependent on central approval for chief executive selection but accountable to local electorate for performance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government_executive, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_government_executive, payer).

% Derive substantial economic value from Hong Kong's dual status — access to mainland markets with common law protections and international financial integration. Use economic leverage to influence both central and local policy. Can relocate capital and operations globally, giving them strong exit leverage.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_business_elites, beneficiary,
    powerful, biographical, mobile, global).

% Benefit from Hong Kong's role as a stable, rule-of-law gateway to China. Their continued presence and investment depend on the credibility of the 'two systems' promise. Can redirect capital flows to Singapore, Shanghai, or other centers if the framework degrades.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, international_financial_institutions, beneficiary,
    organized, generational, arbitrage, global).

% Bear the costs of autonomy erosion — diminished civil liberties, constrained political participation, legal persecution. Their identity is fused with Hong Kong's distinct civic culture; exit means abandoning the political project that constitutes their self-understanding. Organized through unions, NGOs, professional associations, and protest networks.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_prodemocracy_civil_society, payer,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_prodemocracy_civil_society, excluded).

% Tasked with interpreting the Basic Law while subject to NPCSC binding interpretations. Their institutional independence is the primary mechanism for rights protection but is progressively constrained by national security legislation and political appointments. Professional identity tied to common law tradition; exit means leaving the judiciary or the jurisdiction.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_local_courts_judiciary, payer,
    moderate, biographical, constrained, local).

% Formally part of the governance structure but systematically marginalized through electoral reform, disqualification powers, and procedural constraints. Bear costs of political exclusion while retaining residual veto power over some legislation. Exit options limited by identity commitment and legal barriers to participation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_legislative_council_opposition, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_legislative_council_opposition, excluded).

% Operates the national security office in Hong Kong and directs enforcement of the 2020 National Security Law. Defines the operational boundaries of 'national security' in practice. Not directly constrained by Hong Kong law; accountability runs upward through central party-state channels.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, analytical, continental).

% Monitor compliance with the Sino-British Joint Declaration and international human rights obligations. Issue statements, impose targeted sanctions, and adjust diplomatic engagement. Their leverage is reputational and economic but diluted by great-power competition and economic interdependence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, foreign_governments_diplomatic, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the coexistence of two distinct legal-political-economic systems within one sovereign state by establishing a functional division of powers (Hong Kong: commercial law, civil liberties, local governance; Central: defense, foreign affairs, national security) and a process for negotiating contested boundaries through political dialogue rather than judicial supremacy.
% TRANSFER_FUNCTION: Transfers political autonomy from Hong Kong to the center on national security and constitutional interpretation matters; transfers economic rents and policy influence from civil society to business elites and the central government; transfers legitimacy and international standing from the center to Hong Kong's distinct system.
% ABSENT_VOICES: Hong Kong residents without organizational representation — ordinary citizens, ethnic minorities, migrant workers — who bear the daily consequences of autonomy erosion but lack collective voice. Also absent: Taiwan, whose 'One Country Two Systems' template is implicated but has no seat at the negotiation table.
% DISAPPEARANCE_RATIONALE: If the balanced coexistence framework vanished overnight, Hong Kong would either face immediate centralization under direct PRC administration (loss of common law, capital flight, loss of international financial status) or a sovereignty crisis with unpredictable international consequences. The functional division of powers, the negotiation channel, and the ambiguity itself are load-bearing.
% FOUNDING_PROBLEM: The 1997 handover required reconciling Hong Kong's capitalist common-law system and international economic role with PRC sovereignty, without triggering capital flight, social collapse, or international rejection. The framework was built to solve the transition problem: how to absorb a distinct polity into a sovereign state while preserving the economic and legal infrastructure that made it valuable.
% FOUNDING_PROBLEM_CORROBORATION: The PRC and Hong Kong government attest the founding problem remains live — Hong Kong's distinct system still serves China's modernization and international integration. Pro-democracy actors, international legal scholars, and foreign governments attest the transition problem was substantially solved by the early 2000s and the framework now persists as a managed ambiguity that enables central control. The Joint Declaration's monitoring mechanisms (UK Foreign Office reports, UN treaty bodies) corroborate the shifted-function reading from outside the beneficiary set.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) reflects a medium-epsilon regime: the center extracts political autonomy (NPCSC interpretations, NSL, electoral reform) and local elites extract economic rents from the dual-status position, but civil society retains enough bargaining power (economic leverage, international attention, identity-locked resistance) to prevent total capture. Suppression (0.42) is moderate — the 2020 NSL spike (0.65) has partially relaxed but left a higher baseline. Theater ratio (0.28) captures the gap between the 'high degree of autonomy' promise and the operational reality of central intervention. Accessibility collapse (0.35) is low because alternatives (full integration, independence, internationalization) remain conceptually available though politically costly. Resistance (0.55) is significant — civil society, judiciary, and international actors actively contest boundary shifts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (PRC, HK executive, security apparatus) experience the constraint as a necessary coordination mechanism they administer — the ambiguity is a feature enabling management. The payer seats (civil society, judiciary, opposition) experience it as an extraction mechanism where the coordination story is cover for autonomy erosion. The beneficiary seats (business elites, international finance) experience it as a profitable ambiguity they want preserved. The engine computes this divergence from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC central government and mainland security apparatus are structural beneficiaries (d ~0.15-0.25) — they set the agenda, define national security, and extract political control. Hong Kong business elites and international financial institutions are beneficiaries with strong exit (d ~0.1-0.2) — they collect economic rents from the framework's ambiguity. The Hong Kong government executive sits near symmetric (d ~0.5) — administers the constraint but bears costs from both sides. Pro-democracy civil society, judiciary, and LegCo opposition are targets (d ~0.7-0.85) — bear extraction with constrained or identity-locked exit. Foreign governments are analytical observers (d ~0.5). The identity_locked exit of civil society is critical: their self-concept is constituted through Hong Kong's distinct civic identity, making exit existentially costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1997 transition) is contested: beneficiaries claim it remains live; payers claim it was solved and the arrangement now persists as managed ambiguity. This mandatrophy tension IS the constraint's engine — the unresolved status of the founding problem generates the periodic crises that force renegotiation. The classification as tangled_rope (not snare) depends on civil society's retained bargaining power and the center's continued need for Hong Kong's distinct economic utility. If either collapses, the constraint reclassifies toward snare (civil society collapse) or piton (center no longer needs the framework).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_status_ambiguity,
    'Is the 1997 transition problem genuinely still live, or has the framework''s persistence become a cover for autonomy erosion?',
    'Longitudinal analysis of central government policy statements vs. operational interventions; economic dependency metrics (Hong Kong''s share of China''s FDI, IPO listings, RMB internationalization); if Hong Kong''s distinct utility to China declines while interventions increase, the founding problem is dead and the framework is a snare.',
    'If founding problem is dead, the constraint reclassifies toward snare (coordination story is cover). If live, tangled_rope holds. The engine''s mandatrophy detection (founding_problem_status=dead + disappearance_verdict=world_rearranges) captures this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, empirical, 'Whether the constraint''s coordination justification remains valid or has atrophied into extraction cover.').

omega_variable(
    civil_society_bargaining_power_sustainability,
    'Can Hong Kong civil society''s bargaining power (economic leverage, international attention, identity-locked resistance) be sustained under escalating suppression, or is it on a trajectory to collapse?',
    'Track protest frequency/scale, organizational survival rates, emigration of activists, international sanctions effectiveness, and business elite defections. A sustained decline across multiple indicators would signal collapsing counter-extraction capacity.',
    'If bargaining power collapses, the constraint loses its tangled_rope character (coordination without countervailing power becomes snare). The balanced_coexistence reading''s claim that ''civil society retains bargaining power'' is falsified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civil_society_bargaining_power_sustainability, empirical, 'Sustainability of the counter-extraction force that keeps this tangled_rope rather than snare.').

omega_variable(
    negotiation_vs_imposition_boundary,
    'Where is the boundary between ''political accommodation'' (genuine negotiation) and ''central imposition'' (coercion dressed as negotiation)?',
    'Case-by-case analysis of NPCSC interpretations, electoral reforms, and NSL implementations: were Hong Kong institutions consulted? Did they have effective veto? Were concessions made? Pattern of unilateral vs. bilateral moves over time.',
    'If accommodation is systematically performative (theater_ratio understates performativity), the coordination function is weaker than claimed. The tangled_rope classification requires genuine negotiation episodes, not just crisis-triggered central decisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_vs_imposition_boundary, conceptual, 'Whether the ''political accommodation'' mechanism is structurally real or a theater for imposition.').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the balanced_coexistence_reading logically foreclose the sovereignty_primacy_reading within a single Chinese constitutional framework, or can they coexist as competing interpretations?',
    'Analyze whether a single institutional actor (e.g., NPCSC) can simultaneously hold that ''autonomy is negotiated not delegated'' and ''autonomy is delegated and revocable'' without contradiction. If the PRC''s own legal practice treats them as alternatives rather than complements, they may be foreclosed.',
    'If forecloses, the kernel has a genuine logical fracture. If coexists_with, the ambiguity is structurally stable — different actors hold different readings simultaneously. This reading''s axioms assume coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Structural relationship between this reading and the sovereignty_primacy_reading within the same commitment framework.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, security apparatus, electoral engineering) or internalized (self-censorship, chilling effects, identity fracture among resistors)?',
    'Post-NSL trajectory analysis: if formal legal barriers were removed but suppression behaviors persist, the internalized component is significant. Survey data on self-censorship, artistic/academic output changes, organizational risk-aversion.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would increase effective extraction for identity_locked agents beyond what structural metrics capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Hong Kong context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_balanced_tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.12).
narrative_ontology:measurement(octs_balanced_tr_t2003, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(octs_balanced_tr_t2010, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(octs_balanced_tr_t2014, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(octs_balanced_tr_t2019, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(octs_balanced_tr_t2020, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(octs_balanced_tr_t2024, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(octs_balanced_be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.15).
narrative_ontology:measurement(octs_balanced_be_t2003, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2003, 0.22).
narrative_ontology:measurement(octs_balanced_be_t2010, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(octs_balanced_be_t2014, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2014, 0.28).
narrative_ontology:measurement(octs_balanced_be_t2019, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2019, 0.35).
narrative_ontology:measurement(octs_balanced_be_t2020, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(octs_balanced_be_t2024, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(octs_balanced_su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.2).
narrative_ontology:measurement(octs_balanced_su_t2003, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2003, 0.28).
narrative_ontology:measurement(octs_balanced_su_t2010, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(octs_balanced_su_t2014, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2014, 0.35).
narrative_ontology:measurement(octs_balanced_su_t2019, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(octs_balanced_su_t2020, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(octs_balanced_su_t2024, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__balanced_coexistence_reading, 0.1).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sino_british_joint_declaration).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_basic_law).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, national_security_law_hong_kong).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_electoral_reform_2021).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings in the one_country_two_systems_framework kernel family. The sovereignty_primacy_reading (high extraction, central imposition) and autonomy_primacy_reading (low extraction, treaty-guaranteed autonomy) are sibling constraints linked via affects_constraints. This balanced_coexistence_reading occupies the middle: medium extraction, crisis-driven renegotiation, civil society counter-power. The ε values differ substantially across the three readings because they describe structurally distinct constraint regimes, not the same constraint viewed differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, institutional, 0.2).
constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, organized, 0.75).
constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
