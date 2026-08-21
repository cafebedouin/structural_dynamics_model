% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty Claim (Islamic Sovereignty Reading)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'Islamic Sovereignty Reading' of
 *   the Hagia Sophia's status. It describes the site's legitimacy as deriving
 *   from the 1453 Ottoman conquest and continuous Islamic endowment (waqf),
 *   making it sovereign Islamic worship space under Turkish state authority.
 *   This reading is actively enforced through executive decree and judicial
 *   action, leading to identifiable beneficiaries and victims. The claimed
 *   type is Tangled Rope, reflecting a genuine coordination function for
 *   Islamic worship combined with asymmetric extraction from other claimants
 *   and visitors.
 *
 * KEY AGENTS:
 *   - Turkish State Authority: Agenda setter (institutional/constrained)
 *   - AKP Political Coalition: Primary beneficiary (powerful/mobile)
 *   - Turkish Islamic Constituency: Beneficiary (organized/constrained)
 *   - Sunni Ummah (symbolic): Symbolic beneficiary (analytical/identity_locked)
 *   - Non-Muslim Visitors: Primary payer (powerless/constrained)
 *   - UNESCO Regime: Payer (institutional/constrained)
 *   - Secularist Turks: Payer (moderate/constrained)
 *   - Orthodox Church: Excluded (organized/trapped)
 *   - International Heritage Organizations: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.65).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Claim (Islamic Sovereignty Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, 'de4948ec-cc61-41b9-975b-6ecbb466cdde').
narrative_ontology:cs_kernel_codification('de4948ec-cc61-41b9-975b-6ecbb466cdde', formalized).
narrative_ontology:cs_authority_grounding('de4948ec-cc61-41b9-975b-6ecbb466cdde', lineage).
narrative_ontology:cs_interpretation_layer_present('de4948ec-cc61-41b9-975b-6ecbb466cdde').
narrative_ontology:cs_reading_relation('de4948ec-cc61-41b9-975b-6ecbb466cdde', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('de4948ec-cc61-41b9-975b-6ecbb466cdde', hagia_sophia_substrate__universal_heritage_reading, forecloses).
narrative_ontology:cs_axiom('de4948ec-cc61-41b9-975b-6ecbb466cdde', foundational, ottoman_conquest_establishes_sovereignty).
narrative_ontology:cs_axiom_status(ottoman_conquest_establishes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('de4948ec-cc61-41b9-975b-6ecbb466cdde', ottoman_conquest_establishes_sovereignty, conventional).
narrative_ontology:cs_axiom('de4948ec-cc61-41b9-975b-6ecbb466cdde', foundational, waqf_status_is_immutable).
narrative_ontology:cs_axiom_status(waqf_status_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('de4948ec-cc61-41b9-975b-6ecbb466cdde', waqf_status_is_immutable, conventional).
narrative_ontology:cs_reference_frame('de4948ec-cc61-41b9-975b-6ecbb466cdde', islamic_sovereignty_post_conquest).
narrative_ontology:cs_drift_state('de4948ec-cc61-41b9-975b-6ecbb466cdde', contemporary_global_heritage_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('de4948ec-cc61-41b9-975b-6ecbb466cdde', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The executive and judicial branches of the Turkish state, which enacted the conversion of Hagia Sophia from a museum to a mosque and maintain its current status, asserting sovereignty based on historical conquest and waqf law. They benefit from political consolidation and religious legitimacy.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_state_authority, agenda_setter,
    institutional, generational, constrained, national).

% The ruling political party and its allies, who benefit from the conversion through increased domestic political support, consolidation of their conservative-religious base, and a strong signal of national sovereignty.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, beneficiary,
    powerful, biographical, mobile, national).

% The segment of the Turkish population that identifies strongly with Islamic heritage and views the conversion as a rightful restoration of a sacred space, benefiting from enhanced religious identity and access to the site for worship.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, biographical, constrained, national).

% The broader global Sunni Muslim community, which symbolically benefits from the assertion of Islamic sovereignty over a historically significant site, reinforcing a narrative of Islamic resurgence and historical vindication. This is a symbolic, non-agent beneficiary.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically, beneficiary,
    analytical, civilizational, identity_locked, global).
narrative_ontology:stakeholder_non_agent(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolically).

% Tourists and non-Muslims who previously visited Hagia Sophia as a museum with open access. They now face restrictions on access, dress codes, and limited viewing areas, bearing the cost of reduced cultural access and a changed experience.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, constrained, global).

% The international body responsible for designating and protecting World Heritage Sites. It bears the cost of its jurisdiction being challenged and its recommendations being disregarded, leading to a perceived weakening of international heritage norms.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_regime, payer,
    institutional, generational, constrained, global).

% Turkish citizens who adhere to the secular principles of the Republic and viewed Hagia Sophia as a symbol of a pluralistic, secular national identity. They bear the cost of an ideological defeat and the erosion of secularist values in public life.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turks, payer,
    moderate, biographical, constrained, national).

% The Ecumenical Patriarchate and global Orthodox Christian communities, who view Hagia Sophia as their foundational cathedral and advocate for its return to Christian worship or its status as a neutral heritage site. They are excluded from any decision-making and their claims are actively denied by this reading.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_church, excluded,
    organized, civilizational, trapped, global).

% Other international bodies and NGOs focused on cultural heritage preservation. They observe the situation, issue statements, and engage in diplomatic efforts, but lack direct enforcement power over the Turkish state's sovereign decisions.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_heritage_organizations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the site under a singular Islamic worship function and Turkish state authority, providing a clear, consistent framework for its use and management from this perspective.
% TRANSFER_FUNCTION: Transfers symbolic and actual control of a major historical site from a secular/universal heritage framing to an exclusive Islamic sovereignty framing, moving access and interpretive authority from non-Muslims/international bodies to the Turkish state and its Islamic constituency.
% ABSENT_VOICES: The Ecumenical Patriarchate and global Orthodox Christian communities, as well as secularist Turkish intellectuals and international heritage advocates, are structurally excluded from the decision-making process. They would argue for restitution to Christian control, a return to museum status, or shared universal heritage management.
% DISAPPEARANCE_RATIONALE: If the current claim of Islamic sovereignty and state authority vanished overnight, there would be immediate, intense pressure for restitution to Orthodox Christian control or re-establishment as a secular museum, leading to significant geopolitical, cultural, and religious shifts in the region and globally.
% FOUNDING_PROBLEM: To assert Turkish and Islamic sovereignty over a historically significant site following the 1453 Ottoman conquest, and in the contemporary context, to consolidate political power and religious identity within Turkey.
% FOUNDING_PROBLEM_CORROBORATION: Turkish state media, religious institutions, and the ruling political coalition corroborate that the problem of asserting sovereignty and religious identity is live and ongoing. International bodies, Orthodox communities, and secularist Turks contest this, viewing the current arrangement as a political move rather than a resolution of a genuine historical or religious problem.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high due to the significant costs imposed on non-Muslim visitors and the diplomatic/jurisdictional friction with international bodies. Suppression (0.75) is high, reflecting the active enforcement by the Turkish state to maintain its exclusive claim and suppress alternative narratives or uses. The theater ratio (0.25) is relatively low, as the primary function of the site as a mosque is genuine, though some performative aspects exist in the political messaging surrounding its status. The claimed type is Tangled Rope because it coordinates a specific religious use for a large constituency (beneficiaries) while simultaneously extracting from and suppressing the claims of others (victims) through active state enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Turkish state and its Islamic constituency, the current status is a rightful assertion of sovereignty and religious freedom, a legitimate coordination of sacred space. From the perspective of non-Muslim visitors, UNESCO, and secularist Turks, it is an act of cultural appropriation and political maneuvering that extracts access and denies universal heritage principles. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish State Authority, AKP political coalition, and Turkish Islamic constituency are clear beneficiaries, gaining political capital, religious legitimacy, and access to worship space. Non-Muslim visitors, UNESCO, and secularist Turks are victims, losing access, jurisdiction, and ideological ground. The Orthodox Church is excluded, with its historical claims actively denied. The symbolic Sunni Ummah benefits from the narrative without direct agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from this reading's perspective, is to assert and maintain Islamic sovereignty over Hagia Sophia. This mandate is considered 'live' by its proponents. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring the coordination function for its beneficiaries). The contestation around its founding problem status (live vs. solved) is key to understanding its persistence despite international resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_grounding_ambiguity,
    'Is the claim of sovereignty primarily grounded in historical conquest, continuous religious endowment (waqf), or contemporary political will?',
    'Analysis of legal arguments presented by the Turkish state, historical scholarship on waqf law, and political science analysis of contemporary decision-making processes.',
    'If primarily political, the constraint''s constructed nature and extractiveness are amplified; if primarily historical/religious, its proponents'' ''naturalness'' claim gains more internal coherence, though not necessarily external legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_grounding_ambiguity, conceptual, 'Ambiguity in the foundational grounding of the sovereignty claim.').

omega_variable(
    waqf_interpretation_ambiguity,
    'Is the 1453 waqf document an immutable legal instrument, or is its interpretation subject to evolving legal and social norms?',
    'Comparative legal analysis of waqf law across different historical periods and jurisdictions, and examination of precedents for waqf modification or dissolution.',
    'If immutable, the constraint''s persistence appears more ''fixed''; if mutable, the current status is more clearly a contemporary political choice, increasing its perceived constructedness and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_interpretation_ambiguity, empirical, 'Ambiguity regarding the immutability and interpretation of the waqf document.').

omega_variable(
    suppression_mechanism_secularist_turks,
    'Is the suppression experienced by secularist Turks structural (legal/political barriers) or internalized (self-censorship, ideological defeat)?',
    'Sociological studies on political expression and identity formation among secularist Turks, post-policy change. If dissent persists but is muted, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as the target carries the suppression with them after formal barriers are removed or relaxed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_secularist_turks, empirical, 'Structural vs. internalized suppression mechanism for secularist Turks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(hagi_tr_t2005, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(hagi_tr_t2015, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(hagi_tr_t2025, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(hagi_be_t2005, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(hagi_be_t2015, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(hagi_be_t2025, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(hagi_su_t2005, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(hagi_su_t2015, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.73).
narrative_ontology:measurement(hagi_su_t2025, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Hagia Sophia's status, asserting Islamic sovereignty. It directly forecloses the Universal Heritage and Orthodox Restitution readings by claiming exclusive jurisdiction and purpose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
