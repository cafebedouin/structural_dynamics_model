% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Cultural Heritage
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint frames Hagia Sophia as a site of universal human cultural
 *   heritage, transcending specific religious or national claims. This
 *   reading emphasizes its architectural significance, historical layers, and
 *   role as a global tourist attraction and subject of secular scholarship.
 *   It implicitly or explicitly suppresses claims for its use as an active
 *   religious site by either Islamic or Orthodox Christian communities,
 *   positioning a technocratic museum administration under a secular
 *   constitutional framework as its legitimate authority. The constraint is
 *   actively enforced to maintain its secular, museum status, generating
 *   significant tourism revenue and projecting an ideological signal of
 *   secular modernity.
 *
 * KEY AGENTS:
 *   - global_tourism_sector: Beneficiary (powerful/arbitrage) — profits from site's accessibility
 *   - international_cultural_organizations: Beneficiary (institutional/analytical) — promotes universal heritage narrative
 *   - secularist_turkish_elites: Beneficiary (powerful/mobile) — aligns with secular state ideology and tourism revenue
 *   - islamic_worship_advocates: Payer (organized/constrained) — seeks to restore site as mosque, claims suppressed
 *   - orthodox_restitution_advocates: Payer (organized/constrained) — seeks to restore site as church, claims suppressed
 *   - technocratic_museum_administration: Agenda Setter (institutional/constrained) — manages site, enforces secular status
 *   - turkish_state_secular_judiciary: Agenda Setter (institutional/analytical) — upholds secular status, adjudicates challenges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.65).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.7).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Cultural Heritage").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '4787b413-c718-4bb7-a186-86fbfe1159d1').
narrative_ontology:cs_kernel_codification('4787b413-c718-4bb7-a186-86fbfe1159d1', formalized).
narrative_ontology:cs_authority_grounding('4787b413-c718-4bb7-a186-86fbfe1159d1', lineage).
narrative_ontology:cs_interpretation_layer_present('4787b413-c718-4bb7-a186-86fbfe1159d1').
narrative_ontology:cs_reading_relation('4787b413-c718-4bb7-a186-86fbfe1159d1', hagia_sophia_substrate__islamic_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('4787b413-c718-4bb7-a186-86fbfe1159d1', hagia_sophia_substrate__orthodox_restitution_reading, influences).
narrative_ontology:cs_axiom('4787b413-c718-4bb7-a186-86fbfe1159d1', foundational, cultural_heritage_transcends_religious_sovereignty).
narrative_ontology:cs_axiom_status(cultural_heritage_transcends_religious_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4787b413-c718-4bb7-a186-86fbfe1159d1', cultural_heritage_transcends_religious_sovereignty, deontological).
narrative_ontology:cs_axiom('4787b413-c718-4bb7-a186-86fbfe1159d1', foundational, secular_administration_ensures_universal_access).
narrative_ontology:cs_axiom_status(secular_administration_ensures_universal_access, holdable).
narrative_ontology:cs_axiom_grounding('4787b413-c718-4bb7-a186-86fbfe1159d1', secular_administration_ensures_universal_access, instrumental).
narrative_ontology:cs_reference_frame('4787b413-c718-4bb7-a186-86fbfe1159d1', secular_museum_status_1934).
narrative_ontology:cs_drift_state('4787b413-c718-4bb7-a186-86fbfe1159d1', contemporary_religious_revival_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4787b413-c718-4bb7-a186-86fbfe1159d1', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_cultural_organizations).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the site's status as an accessible, secular tourist destination, generating revenue from visitors, tours, and related services. Actively promotes the 'universal heritage' narrative to maintain access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    powerful, generational, arbitrage, global).

% Promotes the preservation of Hagia Sophia as a UNESCO World Heritage site and a symbol of shared human culture, aligning with its mission to protect global heritage. Benefits from the site's secular status as it facilitates their involvement.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_cultural_organizations, beneficiary,
    institutional, civilizational, analytical, global).

% Aligns with the secularist principles of the Turkish Republic, viewing Hagia Sophia's museum status as a symbol of modern, secular identity. Benefits from the cultural capital and international recognition derived from this status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    powerful, generational, mobile, national).

% Advocates for the re-conversion of Hagia Sophia into an active mosque, citing its Ottoman conquest and waqf status. Their claims are suppressed by the 'universal heritage' framing and secular state policies, bearing the cost of denied religious access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates, payer,
    organized, generational, constrained, national).

% Advocates for the restoration of Hagia Sophia as an Orthodox Christian cathedral or a neutral interfaith space, emphasizing its Byzantine origins. Their claims are suppressed by the current secular museum status, bearing the cost of denied religious access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_advocates, payer,
    organized, generational, constrained, global).

% Manages the daily operations of Hagia Sophia as a museum, overseeing preservation, visitor access, and educational programs. Enforces the secular status and regulations that prevent religious worship, acting under the authority of the Turkish state.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, technocratic_museum_administration, agenda_setter,
    institutional, biographical, constrained, local).

% Upholds the secular constitutional framework that underpins Hagia Sophia's museum status. Adjudicates legal challenges to its status, consistently ruling in favor of its secular designation, thereby enforcing the 'universal heritage' reading.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_secular_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation and global accessibility of Hagia Sophia as a unique architectural and historical monument, allowing diverse visitors and scholars to engage with its multi-layered heritage without privileging any single religious or national claim.
% TRANSFER_FUNCTION: Transfers the site's primary function from active religious worship to secular cultural exhibition and tourism, generating revenue for the tourism sector and cultural capital for secular institutions, while suppressing religious claims.
% ABSENT_VOICES: The voices of those advocating for exclusive religious use (both Islamic and Orthodox) are present but marginalized within the dominant 'universal heritage' discourse. Their claims are actively suppressed by the legal and administrative framework that maintains the site's secular status.
% DISAPPEARANCE_RATIONALE: If the 'universal heritage' constraint vanished, the site would immediately become a focal point of intense religious and nationalistic contestation, likely leading to its re-conversion into either a mosque or a church, fundamentally altering its accessibility and symbolic meaning for global audiences. The international cultural preservation framework would also be significantly challenged.
% FOUNDING_PROBLEM: The problem of managing a site with profound, contested religious and national significance, preventing its capture by any single group and ensuring its preservation and accessibility for all humanity.
% FOUNDING_PROBLEM_CORROBORATION: International cultural organizations and secularist Turkish elites corroborate the ongoing problem of managing contested heritage. However, religious advocates (Islamic and Orthodox) dispute this, arguing that the 'problem' is a construct to deny their legitimate claims, and that the site's original religious functions should be restored. Historical records and international legal debates provide corroboration for the contested nature of the problem's status.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the 'universal heritage' framing, while ostensibly inclusive, effectively extracts the site from specific religious communities, converting its value into secular tourism revenue and ideological capital. Suppression (0.70) is also high, as active legal and administrative enforcement is required to prevent religious re-conversion and maintain its museum status against strong internal and external pressures. The theater ratio (0.20) is relatively low, indicating that the administration's activities are genuinely focused on maintaining the site's physical integrity and museum function, even if the underlying justification is contested. Accessibility collapse (0.40) is moderate, as religious alternatives exist but are suppressed at this specific site. Resistance (0.55) is significant, reflecting ongoing advocacy for religious use.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of global tourism and secularist elites, this constraint is a legitimate coordination mechanism for preserving a unique cultural asset. From the perspective of Islamic and Orthodox advocates, it is an extractive imposition that denies their historical and religious claims. The technocratic administration acts as an agenda-setter, enforcing the secular status, while religious communities are payers, bearing the cost of denied access and suppressed claims. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The global tourism sector, international cultural organizations, and secularist Turkish elites are beneficiaries (low d) as they gain from the site's secular, accessible status and the associated revenue/prestige. Islamic and Orthodox worship advocates are victims/payers (high d) as their claims for religious use are actively suppressed. The technocratic museum administration and the Turkish state secular judiciary are agenda-setters, enforcing this particular reading of the site's status.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates global access and preservation (a public good) while simultaneously extracting the site from specific religious claims through active enforcement. It avoids being a Snare by having a defensible, albeit contested, coordination function (preserving universal heritage). It avoids being a Rope because of the clear, asymmetric extraction from religious communities and the high suppression required to maintain its status. The 'mandate' of universal heritage is live for some, but contested by others who see its original religious mandates as paramount.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine expression of universal heritage, or a political framing to suppress specific religious claims?',
    'Analysis of enforcement patterns: if enforcement primarily targets religious expression while promoting secular tourism, it leans towards political framing. If it genuinely balances diverse cultural access, it leans towards universal heritage.',
    'If primarily a political framing, the constraint''s extractiveness and suppression are higher, reclassifying it closer to a Snare. If genuine, it remains a Tangled Rope with a stronger coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''universal_heritage_reading'' of the ''hagia_sophia_substrate'' kernel. Sibling readings (''islamic_sovereignty_reading'', ''orthodox_restitution_reading'') would shift beneficiaries/victims and claimed authority.').

omega_variable(
    universal_vs_local_legitimacy,
    'Can the claim of universal cultural heritage genuinely transcend local religious and national claims, or does it inherently suppress them?',
    'Long-term observation of cultural policy outcomes: if local religious and national claims are consistently marginalized or denied, the ''universal'' framing is effectively extractive. If a genuine synthesis or shared access model emerges, it supports the coordination claim.',
    'If the ''universal'' claim consistently suppresses local claims, its extractiveness is higher, and its coordination function is weaker, pushing it towards a Snare. If it facilitates genuine shared access, it reinforces its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_local_legitimacy, empirical, 'Ambiguity in whether ''universal heritage'' is a genuinely inclusive framework or a mechanism for secular control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hagi_tr_t10, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(hagi_tr_t20, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hagi_be_t10, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(hagi_be_t20, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(hagi_su_t10, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(hagi_su_t20, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Hagia Sophia's contested status, each with distinct beneficiaries, victims, and claimed authority. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
