% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Territorial Legitimacy via Security Necessity and Strategic Depth
 *   domain: political theory / international law / territorial sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the security_necessity_reading of the
 *   territorial_legitimacy kernel: the claim that Israeli control of the West
 *   Bank and Golan Heights is legitimized not by international partition or
 *   indigenous continuity but by the existential security requirement of
 *   strategic depth and defensible borders. The constraint coordinates
 *   collective defense for the Israeli polity while extracting sovereignty
 *   and territorial autonomy from Palestinian and Syrian resident
 *   populations. It is actively enforced by military administration,
 *   settlement infrastructure, and legal regimes that differentiate between
 *   sovereign territory and administered territory. The reading treats
 *   Palestinian sovereignty as conditionally permissible only under
 *   demilitarization, and treats civilian settlements as constituting a
 *   defensive security presence.
 *
 * KEY AGENTS:
 *   - israeli_security_establishment: Primary agenda-setter (institutional/arbitrage) â sets territorial doctrine, administers military government, and absorbs budget and prestige from the security framing.
 *   - settler_enterprise: Primary beneficiary (organized/constrained) â receives land allocations, subsidies, and legal protection under the security umbrella.
 *   - palestinian_communities: Primary target (powerless/trapped) â bear sovereignty denial, movement restrictions, and expropriation risk.
 *   - golan_residents: Secondary target (powerless/trapped) â subject to annexation without self-determination.
 *   - international_legal_institutions: Analytical observer (institutional/analytical) â maintains that the territory is occupied and the security necessity reading is invalid under international law.
 *   - palestinian_political_leadership: Constrained payer (moderate/constrained) â partially recognized interlocutor whose sovereignty claims are structurally deferred by security conditionality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.82).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Territorial Legitimacy via Security Necessity and Strategic Depth").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political theory / international law / territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '0acfbca3-b90a-4ac4-97fe-96a02c2e0677').
narrative_ontology:cs_kernel_codification('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', formalized).
narrative_ontology:cs_authority_grounding('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', expertise).
narrative_ontology:cs_interpretation_layer_present('0acfbca3-b90a-4ac4-97fe-96a02c2e0677').
narrative_ontology:cs_reading_relation('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_axiom('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', foundational, security_buffer_legitimizes_territorial_control).
narrative_ontology:cs_axiom_status(security_buffer_legitimizes_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', security_buffer_legitimizes_territorial_control, empirically_contingent).
narrative_ontology:cs_axiom('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', secondary, civilian_presence_constitutes_defensive_depth).
narrative_ontology:cs_axiom_status(civilian_presence_constitutes_defensive_depth, holdable).
narrative_ontology:cs_axiom_grounding('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', civilian_presence_constitutes_defensive_depth, instrumental).
narrative_ontology:cs_reference_frame('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', strategic_depth_doctrine).
narrative_ontology:cs_drift_state('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', contemporary_asymmetric_warfare_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0acfbca3-b90a-4ac4-97fe-96a02c2e0677', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, settler_enterprise).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_communities).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, golan_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_political_leadership).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, strategic_depth_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets military and strategic doctrine defining which territories constitute necessary security buffers. Administers military government in the West Bank and maintains defense infrastructure in the Golan Heights. Derives institutional budget, legal mandate, and national prestige from the territorial control mission. Could theoretically redefine the security perimeter but faces intense domestic political cost.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, beneficiary).

% Receives land allocations, building permits, infrastructure subsidies, and legal protection within territories designated as necessary for security. Expands civilian presence that is then retroactively justified as defensive depth. Exit from the constraint is constrained because property and livelihood depend on state support.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, settler_enterprise, beneficiary,
    organized, biographical, constrained, regional).

% Live under military administration that restricts building, movement, and land use in favor of settlement expansion and security zones. Their sovereignty claims are deferred indefinitely by the security-necessity framing. Exit is trapped because the land is their place of residence and origin, and departure means dispossession.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_communities, payer,
    powerless, generational, trapped, regional).

% Subject to annexation and citizenship imposition without self-determination. Their territorial identity and property rights are overridden by the security buffer claim. Exit is trapped because the Golan is their home; departure would be forced displacement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, golan_residents, payer,
    powerless, generational, trapped, regional).

% Maintain that the West Bank and Golan are occupied territories under the Geneva Conventions and that the security necessity reading does not override international humanitarian law. They issue findings and advisory opinions but lack enforcement capacity to alter the constraint.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_institutions, observer,
    institutional, civilizational, analytical, global).

% Negotiates interim arrangements while full sovereignty is conditioned on demilitarization and security guarantees that preserve Israeli territorial control. Bears the cost of deferred statehood. Exit is constrained because diplomatic channels are open but sovereignty remains structurally blocked.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_political_leadership, payer,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective territorial defense by asserting that centralized control of specific buffer territories is necessary for state survival against external conventional and non-state military threats.
% TRANSFER_FUNCTION: Moves territorial control, settlement rights, and sovereignty claims from Palestinian and Syrian resident populations to the Israeli state and settler enterprise; moves security justification into legal and administrative facts on the ground.
% ABSENT_VOICES: Palestinian refugees displaced in 1967 who hold property deeds in the West Bank and Golan; international humanitarian organizations with access restrictions; Israeli military refuseniks and anti-occupation civil society systematically marginalized in national security discourse.
% DISAPPEARANCE_RATIONALE: If the security-necessity legitimacy claim vanished, the legal and political scaffolding for permanent territorial control would collapse; settlements would face immediate sovereign challenge, the military administration would require alternative legal grounding or withdrawal, and the regional diplomatic architecture would shift toward border negotiations rather than security conditionality.
% FOUNDING_PROBLEM: The perceived existential vulnerability of pre-1967 borders to conventional military invasion and non-state armed attack, particularly the narrow geographic waist of Israel's coastal plain and lack of strategic depth.
% FOUNDING_PROBLEM_CORROBORATION: The Palestinian political leadership and international legal institutions attest the founding security problem is either misrepresented or superseded by peace treaties with Egypt and Jordan and by changed threat environments. Independent Israeli strategic analysts, including retired military and intelligence officials, corroborate that conventional invasion risk has diminished and that permanent territorial control now serves political rather than strictly defensive goals. The security establishment and settler enterpriseâthe benefiting partiesâassert the problem remains live, but no external corroboration supports their current assessment without substantial contestation.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint systematically transfers territorial control and settlement prerogatives from the resident population to the Israeli state and settler enterprise under a security rationale that is at best partially correlated with actual defensive needs. Suppression (0.82) is higher because the arrangement requires active military and legal suppression of Palestinian sovereignty claims, international legal findings, and alternative territorial arrangements. Theater ratio (0.45) is moderate: the security function is genuine against certain threat models, but a growing share of territorial policy operates as political theater performed in security vocabulary. Accessibility collapse (0.68) reflects that once the security frame is accepted, alternatives such as full withdrawal or equal citizenship become structurally unavailable in mainstream discourse. Resistance (0.75) is high: sustained Palestinian armed and unarmed resistance, international legal challenges, and sanctions discourse.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli security establishment experiences this constraint as genuine coordination: it solves a collective-action problem of territorial defense by centralizing control of a buffer zone. From the Palestinian and Golan-resident seats, the identical structure computes as extraction: their sovereignty is denied, their land expropriated, and their exit blocked by military orders and settlement geography. The international legal seat sees an unlawful occupation whose security justification lacks treaty basis. The engine computes this divergence from the structural data â beneficiaries with arbitrage-grade exit versus trapped powerless targets â rather than from narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The security establishment is a beneficiary-agenda_setter: it derives mission, budget, and institutional centrality from the territorial control regime (d near the beneficiary end). The settler enterprise is a concentrated beneficiary capturing land and state subsidy (d near the beneficiary end). Palestinian communities and Golan residents are trapped targets: they cannot exit the territorial constraint because they reside on the land being claimed, and identity and family bonds prevent mass departure (d near full target). Palestinian political leadership is a constrained payer: they have some diplomatic mobility but no sovereign exit from the constraint's effects (d mid-high). International legal institutions are analytical with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents the false binary of 'security coordination' versus 'colonial extraction.' It captures that the constraint does solve a coordination problem â collective territorial defense against a plausible external threat â while simultaneously operating as an extraction mechanism. The mandatrophy question is whether the founding security problem (pre-1967 conventional invasion vulnerability) is still live. The reading asserts it is; independent strategic analysis and regional peace architecture suggest it is substantially dead or transformed. The temporal measurements show rising extraction and theater over time, consistent with a coordination function degrading into inertial extraction. If the founding problem is dead and the constraint persists, the coordination story becomes cover for extraction, pushing the computed type toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_motive_authenticity,
    'Is the territorial control regime primarily motivated by defensive security necessity, or does security rhetoric serve as cover for territorial expansion and settlement consolidation?',
    'Archival analysis of cabinet and military decision-making records; correlation between settlement expansion timelines and contemporaneous threat assessments; comparison of territorial withdrawals versus holdings under varying security conditions.',
    'If security is the authentic motive, the tangled_rope classification holds (genuine coordination plus extraction). If expansion is the primary motive, the coordination story is cover and classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_motive_authenticity, empirical, 'Whether the security justification is authentic or cover for expansion').

omega_variable(
    strategic_depth_military_obsolescence,
    'Has the conventional ground-invasion threat that justified strategic depth been rendered obsolete by missile technology, drone warfare, and asymmetric threats?',
    'Independent strategic threat assessment comparing territorial depth to actual prevention metrics for missile and drone attacks; analysis of whether holding ground prevents the threats the state actually faces.',
    'If the threat model is obsolete, the coordination function is hollow, extraction dominates, and the constraint moves toward snare or piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_depth_military_obsolescence, empirical, 'Whether strategic depth remains militarily relevant').

omega_variable(
    demilitarized_sovereignty_viability,
    'Is conditional demilitarized sovereignty a structurally viable form of self-determination, or does the security conditionality permanently prevent sovereignty?',
    'Comparative case studies of demilitarized states and their actual sovereignty capacity; legal analysis of whether conditionality preserves a residual right of intervention that negates independence.',
    'If conditionality negates sovereignty, the constraint''s stated coordination outcome (two states) is illusory and extraction is total. If viable, the coordination story retains some structural integrity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demilitarized_sovereignty_viability, conceptual, 'Whether conditional sovereignty is genuine self-determination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tl_snr_tr_t0, territorial_legitimacy__security_necessity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tl_snr_tr_t14, territorial_legitimacy__security_necessity_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement(tl_snr_tr_t28, territorial_legitimacy__security_necessity_reading, theater_ratio, 28, 0.35).
narrative_ontology:measurement(tl_snr_tr_t42, territorial_legitimacy__security_necessity_reading, theater_ratio, 42, 0.4).
narrative_ontology:measurement(tl_snr_tr_t49, territorial_legitimacy__security_necessity_reading, theater_ratio, 49, 0.42).
narrative_ontology:measurement(tl_snr_tr_t56, territorial_legitimacy__security_necessity_reading, theater_ratio, 56, 0.45).

% Extraction over time
narrative_ontology:measurement(tl_snr_be_t0, territorial_legitimacy__security_necessity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tl_snr_be_t14, territorial_legitimacy__security_necessity_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(tl_snr_be_t28, territorial_legitimacy__security_necessity_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement(tl_snr_be_t42, territorial_legitimacy__security_necessity_reading, base_extractiveness, 42, 0.68).
narrative_ontology:measurement(tl_snr_be_t49, territorial_legitimacy__security_necessity_reading, base_extractiveness, 49, 0.72).
narrative_ontology:measurement(tl_snr_be_t56, territorial_legitimacy__security_necessity_reading, base_extractiveness, 56, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tl_snr_su_t0, territorial_legitimacy__security_necessity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tl_snr_su_t14, territorial_legitimacy__security_necessity_reading, suppression_requirement, 14, 0.55).
narrative_ontology:measurement(tl_snr_su_t28, territorial_legitimacy__security_necessity_reading, suppression_requirement, 28, 0.62).
narrative_ontology:measurement(tl_snr_su_t42, territorial_legitimacy__security_necessity_reading, suppression_requirement, 42, 0.72).
narrative_ontology:measurement(tl_snr_su_t49, territorial_legitimacy__security_necessity_reading, suppression_requirement, 49, 0.78).
narrative_ontology:measurement(tl_snr_su_t56, territorial_legitimacy__security_necessity_reading, suppression_requirement, 56, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel decomposes into three structurally distinct constraints because the label 'territorial legitimacy' conflates incompatible grounds for sovereignty: international legal partition, indigenous continuity, and security necessity. Each reading has a different beneficiary/victim structure, different Îµ, and different empirical status. They are linked as a constraint family because they compete to occupy the same legitimating function for the same territory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
