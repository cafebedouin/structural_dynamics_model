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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Cultural Heritage
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint story models the 'universal heritage' reading of Hagia
 *   Sophia's legitimacy, which frames the site as a secular museum
 *   transcending specific religious or national claims. This reading,
 *   dominant from 1934 until 2020, served to coordinate international
 *   cultural preservation and tourism while simultaneously suppressing
 *   competing Islamic and Orthodox Christian claims for religious use. The
 *   constraint is classified as a Tangled Rope because it genuinely
 *   coordinates a global public around cultural heritage but does so through
 *   an actively enforced secular administration that extracts from and
 *   suppresses religious advocates. The metrics reflect the increasing
 *   extractiveness (tourism revenue, ideological signal) and suppression
 *   required to maintain this secular status against rising religious and
 *   nationalist pressures.
 *
 * KEY AGENTS:
 *   - turkish_state_secular_administration: Agenda-setter (institutional/constrained)
 *   - global_tourism_sector: Beneficiary (organized/mobile)
 *   - international_scholarship: Beneficiary (moderate/mobile)
 *   - secularist_turkish_elites: Beneficiary (powerful/constrained)
 *   - islamic_worship_advocates: Payer (organized/identity_locked)
 *   - orthodox_christian_advocates: Payer (organized/identity_locked)
 *   - unesco: Observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Cultural Heritage").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, 'ce9d7f0a-6cff-4354-912c-5f39317554ea').
narrative_ontology:cs_kernel_codification('ce9d7f0a-6cff-4354-912c-5f39317554ea', formalized).
narrative_ontology:cs_authority_grounding('ce9d7f0a-6cff-4354-912c-5f39317554ea', extraction).
narrative_ontology:cs_interpretation_layer_present('ce9d7f0a-6cff-4354-912c-5f39317554ea').
narrative_ontology:cs_reading_relation('ce9d7f0a-6cff-4354-912c-5f39317554ea', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce9d7f0a-6cff-4354-912c-5f39317554ea', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_axiom('ce9d7f0a-6cff-4354-912c-5f39317554ea', foundational, cultural_heritage_transcends_religious_sovereignty).
narrative_ontology:cs_axiom_status(cultural_heritage_transcends_religious_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ce9d7f0a-6cff-4354-912c-5f39317554ea', cultural_heritage_transcends_religious_sovereignty, deontological).
narrative_ontology:cs_axiom('ce9d7f0a-6cff-4354-912c-5f39317554ea', secondary, secular_administration_ensures_universal_access).
narrative_ontology:cs_axiom_status(secular_administration_ensures_universal_access, holdable).
narrative_ontology:cs_axiom_grounding('ce9d7f0a-6cff-4354-912c-5f39317554ea', secular_administration_ensures_universal_access, instrumental).
narrative_ontology:cs_reference_frame('ce9d7f0a-6cff-4354-912c-5f39317554ea', ataturk_secular_republic_founding).
narrative_ontology:cs_drift_state('ce9d7f0a-6cff-4354-912c-5f39317554ea', contemporary_religious_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce9d7f0a-6cff-4354-912c-5f39317554ea', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_scholarship).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Hagia Sophia as a museum, emphasizing its universal cultural value and secular status. Benefits from international prestige and tourism revenue. Actively suppresses claims for religious use to maintain its secular identity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_secular_administration, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from Hagia Sophia's status as a major tourist attraction, generating revenue through visitor flows. Supports its museum status for broad accessibility.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Benefits from open access for research and conservation, promoting a narrative of shared human history. Supports the museum status for academic freedom and preservation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_scholarship, beneficiary,
    moderate, generational, mobile, global).

% Aligns with the universal heritage narrative as a symbol of modern, secular Turkey. Benefits ideologically and politically from this framing, which counters religious nationalist claims.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    powerful, biographical, constrained, national).

% Seeks to restore Hagia Sophia as an active mosque, viewing its museum status as a suppression of Islamic heritage and Ottoman sovereignty. Bears the cost of denied religious practice and political marginalization.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_advocates, payer,
    organized, generational, identity_locked, national).

% Advocates for the site's return to Christian worship or a neutral status honoring its Byzantine origins. Bears the cost of denied religious and historical claims, feeling excluded from its management.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_advocates, payer,
    organized, generational, identity_locked, global).

% Monitors Hagia Sophia's status as a World Heritage Site, advocating for its preservation and universal accessibility. Can issue recommendations or warnings but has limited direct enforcement power over sovereign decisions.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts for cultural preservation and scholarship, providing a neutral space for diverse interpretations of history and art, accessible to a global public.
% TRANSFER_FUNCTION: Transfers symbolic ownership and management authority from specific religious or national claims to a secular, universalist framework, generating tourism revenue and academic prestige for the administering state.
% ABSENT_VOICES: The voices of those who view Hagia Sophia primarily as a sacred space for worship (both Islamic and Orthodox Christian) are marginalized in the universal heritage discourse, which prioritizes secular access and academic interpretation over religious practice.
% DISAPPEARANCE_RATIONALE: If the universal heritage framing and its associated museum administration vanished, the site would immediately become a focal point of intense religious and nationalistic contestation, likely leading to its re-conversion to a place of worship under one of the competing claims, fundamentally altering its public function and accessibility.
% FOUNDING_PROBLEM: The problem of managing a site with deeply contested religious and national significance, preventing its capture by any single group and ensuring its preservation for all humanity.
% FOUNDING_PROBLEM_CORROBORATION: International cultural organizations (like UNESCO) and secular academic bodies corroborate the ongoing challenge of balancing diverse claims while preserving the site. This is attested from outside the direct beneficiaries of the secular administration.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) reflects the significant tourism revenue and ideological capital derived from presenting Hagia Sophia as a symbol of secular modernity and universal culture. Suppression (0.75) is high due to the active state enforcement required to prevent religious services and maintain the museum status against strong internal and external pressures. The theater ratio (0.40) indicates that while genuine preservation and scholarly functions exist, a substantial portion of administrative effort is performative, aimed at reinforcing the secular narrative and deflecting religious claims. Accessibility collapse (0.60) is moderate, as physical access is open, but religious alternatives for the site are structurally foreclosed. Resistance (0.55) is significant, reflecting ongoing advocacy and political pressure from religious groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the secular Turkish administration and international cultural bodies, this constraint is a legitimate coordination mechanism for global heritage. However, from the perspective of Islamic and Orthodox Christian advocates, it is an extractive and suppressive mechanism that denies their historical and religious claims to the site. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a Rope-like coordination and payers experiencing a Snare-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish State Secular Administration, global tourism, international scholarship, and secularist Turkish elites are beneficiaries, as they gain prestige, revenue, or ideological validation from the universal heritage framing (low directionality). Islamic and Orthodox Christian advocates are payers, as their claims for religious use are suppressed, and they bear the cost of exclusion (high directionality). UNESCO acts as an observer, advocating for the universal heritage principle but not directly benefiting or paying in the same way.
 *
 * MANDATROPHY ANALYSIS:
 *   The universal heritage reading, while serving a genuine coordination function for cultural preservation, also became a tool for ideological extraction by secularist elites. The rising extractiveness and suppression over time, coupled with persistent resistance, indicate that the coordination function was increasingly intertwined with the suppression of alternative framings. The classification as a Tangled Rope prevents mislabeling this as pure coordination (Rope) or pure extraction (Snare), highlighting its dual nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_heritage_vs_religious_claim,
    'Is the ''universal heritage'' framing a genuinely neutral coordination mechanism, or does it implicitly privilege a secular-modernist worldview over religious claims?',
    'Analysis of resource allocation and narrative control: if the framing consistently marginalizes religious perspectives in favor of secular ones, it suggests an implicit bias.',
    'If biased, the effective extractiveness for religious advocates is higher than measured, as their worldview is structurally devalued; if truly neutral, the coordination function is purer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_vs_religious_claim, conceptual, 'Ambiguity of neutrality in universal heritage discourse.').

omega_variable(
    secular_state_legitimacy_coupling,
    'To what extent is the Turkish state''s secular identity coupled to the universal heritage status of Hagia Sophia?',
    'Political analysis of state rhetoric and policy shifts: if changes in Hagia Sophia''s status correlate strongly with shifts in the state''s secularist commitments, the coupling is high.',
    'If highly coupled, the constraint''s persistence is tied to the political fortunes of secularist factions, making it more vulnerable to political shifts and increasing the stakes for all parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_state_legitimacy_coupling, empirical, 'Coupling between state secularism and Hagia Sophia''s status.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''hagia_sophia_substrate'' kernel. This specific reading is ''universal_heritage_reading''. Sibling readings include ''islamic_sovereignty_reading'' and ''orthodox_restitution_reading''. What are the precise structural elements that differentiate this reading from its siblings?',
    'Comparative analysis of each reading''s declared beneficiaries, victims, authority grounding, and core axioms.',
    'The classification of this constraint (and its siblings) depends entirely on the specific structural choices made in defining each reading. If the structural deltas are insufficient, the readings may collapse into a single, ambiguous constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Differentiating structural elements across kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1934, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(hagi_tr_t1970, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(hagi_tr_t1990, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(hagi_tr_t2010, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1934, 0.5).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(hagi_be_t1970, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(hagi_be_t1990, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(hagi_be_t2010, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1934, 0.6).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(hagi_su_t1970, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(hagi_su_t1990, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(hagi_su_t2010, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hagia_sophia_substrate' kernel, each representing a distinct structural claim about the site's legitimacy. This 'universal_heritage_reading' focuses on its secular, global cultural value, distinct from religious or national claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
