% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia Universal Heritage Reading — Museum Frame
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   The universal heritage reading frames Hagia Sophia as a UNESCO World
 *   Heritage site whose legitimacy derives from its status as shared human
 *   cultural heritage transcending any single religious or national claim.
 *   This reading was instantiated by Atatürk's 1934 museum conversion and
 *   cemented by UNESCO's 1985 inscription. It coordinates preservation,
 *   tourism access, and scholarly research — genuine coordination functions.
 *   Simultaneously, it extracts worship sovereignty from Islamic claimants
 *   (banned 1934–2020, subordinated 2020–present) and ideological capital for
 *   the secular Turkish state. The Turkish state administration acts as
 *   agenda-setter and primary beneficiary (tourism revenue, secular identity
 *   signal); UNESCO and the global tourism sector are co-beneficiaries.
 *   Islamic worship claimants and Muslim pilgrims are the primary victims —
 *   their devotional access is suppressed or conditioned. The Orthodox
 *   restitution claim is excluded entirely. The constraint requires active
 *   enforcement: museum ticketing, prayer bans, tourist flow management, and
 *   UNESCO monitoring missions. Extraction has risen steadily as tourism
 *   volumes grew and the site became a revenue anchor; theater ratio rose as
 *   conservation performance increasingly serves the visitor gaze over
 *   material need; suppression remained high throughout because the reading's
 *   persistence depends on actively preventing worship sovereignty from
 *   reasserting.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.72).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.78).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia Universal Heritage Reading — Museum Frame").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, 'b6f15ef4-1294-4cf0-bdf7-bc842e8d2596').
narrative_ontology:cs_kernel_codification('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', formalized).
narrative_ontology:cs_authority_grounding('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', extraction).
narrative_ontology:cs_interpretation_layer_present('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596').
narrative_ontology:cs_reading_relation('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', foundational, cultural_heritage_transcends_religious_sovereignty).
narrative_ontology:cs_axiom_status(cultural_heritage_transcends_religious_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', cultural_heritage_transcends_religious_sovereignty, conventional).
narrative_ontology:cs_axiom('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', secondary, secular_administration_optimizes_universal_access).
narrative_ontology:cs_axiom_status(secular_administration_optimizes_universal_access, holdable).
narrative_ontology:cs_axiom_grounding('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', secular_administration_optimizes_universal_access, empirically_contingent).
narrative_ontology:cs_reference_frame('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', unesco_world_heritage_framework).
narrative_ontology:cs_drift_state('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', post_2020_reconversion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b6f15ef4-1294-4cf0-bdf7-bc842e8d2596', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_apparatus).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_sector).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_claimants).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, muslim_pilgrims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, turkish_state_administration).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_convention).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, cultural_heritage_transcends_national_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the site as a museum (1934–2020) and mosque (2020–present) under secular constitutional authority. Controls access rules, ticketing, conservation priorities, and the narrative presented to visitors. Captures tourism revenue and the ideological signal of secular modernity. Can change the site's status by decree but faces international pressure from UNESCO.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, turkish_state_administration, beneficiary).

% Operates tour packages, guide services, hospitality, and transport around the site. Depends on the museum frame for unimpeded, non-worship access to the interior. Revenue scales with visitor volume and ticket prices set by the state. Would lose commercial access if the site returned to exclusive worship use.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% View the museum status as the definitive symbol of Atatürk's secular republic — proof that the state transcends Ottoman-Islamic identity. Their political identity is fused to the site's museum framing; reconversion to a mosque is experienced as a personal and civilizational loss. Exit means emigrating or accepting political marginalization.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    powerful, biographical, constrained, national).

% Inscribed the site in 1985 under criteria (i), (ii), (iii), (iv). Provides the international legal framework that legitimizes the universal heritage reading. Monitors conservation status and pressures Turkey through World Heritage Committee procedures. Its authority derives from the Convention's near-universal ratification; it collects no direct revenue but its institutional relevance depends on sites like this performing the universal heritage script.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_apparatus, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_apparatus, beneficiary).

% Byzantine, Ottoman, and art historians whose research access, funding, and publication venues depend on the site being a conserved, accessible museum. The museum frame enables scholarly work that worship use would restrict (scaffolding, lighting, visitor flows, documentation). They advocate for the universal heritage reading because it secures their epistemic access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_sector, beneficiary,
    organized, biographical, mobile, global).

% Include the Directorate of Religious Affairs (Diyanet), Waqf administrators, and pious Muslims who view the site as an Ottoman endowment (waqf) legally dedicated to Islamic worship in perpetuity. The museum frame (1934–2020) and the post-2020 hybrid status (mosque with tourist access) both suppress full worship sovereignty — prayer was banned for 86 years; now tourist flows interrupt prayer times, and Christian iconography remains uncovered during worship. Their identity is fused to the site's Islamic character; exit is not an option without abandoning a core religious-historical claim.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_claimants, payer,
    organized, generational, identity_locked, national).

% Domestic and international Muslims who come to pray. Under the museum frame they were excluded entirely; under the current hybrid frame they must pray around tourist schedules, ticket queues, and uncovered Christian mosaics. They bear the cost of the universal heritage reading's suppression of worship sovereignty — their devotional experience is subordinated to the visitor gaze. No alternative site substitutes for Hagia Sophia's specific historical-spiritual weight.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, muslim_pilgrims, payer,
    moderate, immediate, trapped, global).

% Ecumenical Patriarchate and Orthodox faithful who claim the site as the mother cathedral of Orthodoxy, converted by conquest in 1453. They are excluded from both the museum frame (which erases its Christian liturgical past) and the mosque frame (which asserts Islamic sovereignty). They would object to both the universal heritage reading and the Islamic sovereignty reading; their absence from the UNESCO/state negotiation is structural.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_claimants, excluded,
    organized, generational, constrained, global).

% ICOMOS, IUCN, and specialized NGOs that monitor physical conservation. They evaluate the site's material condition impartially but their reports are weaponized by all readings. They do not collect revenue or bear worship costs; their interest is the fabric's survival.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, heritage_conservation_ngos, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the physical fabric of a 1,500-year-old building against neglect, earthquake, and war; provides regulated public access to a globally significant monument; channels tourism revenue into conservation (nominally).
% TRANSFER_FUNCTION: Moves tourism revenue (ticket sales, hospitality spend) and ideological capital (secular modernity signal, UNESCO legitimacy) from the site's visitor economy to the Turkish state administration and the global tourism sector. Moves worship sovereignty from Islamic claimants to the secular state's administrative discretion. Moves scholarly access from religious gatekeepers to academic institutions.
% ABSENT_VOICES: Orthodox Christian claimants (Ecumenical Patriarchate, Orthodox faithful) are structurally excluded — they would demand either restitution or neutral status honoring Byzantine origins, but neither the Turkish state nor UNESCO includes them in governance. Muslim worship claimants were excluded from 1934–2020; post-2020 they are included as worshippers but not as sovereign authorities over the space.
% DISAPPEARANCE_RATIONALE: If the universal heritage reading vanished overnight, the site would revert to a contest between Islamic sovereignty and Orthodox restitution readings — the Turkish state would lose its primary international legitimacy frame for managing the site, UNESCO would lose its flagship 'shared heritage' case, tourism operators would lose guaranteed non-worship access, and the secularist elite identity symbol would collapse. The physical site remains but the governance arrangement rearranges completely.
% FOUNDING_PROBLEM: After the Ottoman collapse, the new Turkish Republic needed to convert a contested conquest symbol into a neutral civic asset that signaled Western-oriented modernity and could be managed without triggering inter-religious conflict.
% FOUNDING_PROBLEM_CORROBORATION: Atatürk's own speeches and the 1934 Cabinet decree attest the founding problem (secularizing a conquest monument). Contemporary Turkish secularists attest it remains live (the republic's identity still requires it). Islamic claimants and independent historians attest the founding problem is dead — the Ottoman waqf was never legally extinguished, and the museum conversion was an extra-legal seizure. UNESCO's 1985 inscription rationale corroborates the universal heritage framing as a live international problem.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness 0.72 reflects the gap between tourism revenue captured (estimated $50M+ annually direct/indirect) and marginal conservation cost. The ideological extraction — the secular modernity signal — is harder to quantify but structurally central. Suppression 0.78 reflects 86 years of total prayer ban plus the current hybrid regime where worship is permitted but subordinated to tourist schedules and uncovered Christian iconography. Theater ratio 0.45: conservation is real but a growing share of expenditure serves presentation (lighting, pathways, multilingual signage) rather than structural stabilization. Accessibility collapse 0.55: alternatives exist (other Byzantine churches, other Ottoman mosques) but none substitute for this specific site's symbolic weight. Resistance 0.68: continuous Islamic legal challenges, political movements, and the 2020 reconversion demonstrate sustained contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the Turkish state/UNESCO seat, the constraint appears as rope — genuine coordination of preservation and access. From the Islamic claimant seat, it appears as snare — suppression of worship sovereignty under a coordination cover. From the secularist elite seat, it appears as mountain — the natural order of a modern republic. The engine computes this divergence from the structural data: same constraint, different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Turkish state administration: d ≈ 0.15 (agenda-setter, collects revenue, controls narrative — strong beneficiary). UNESCO: d ≈ 0.20 (institutional beneficiary, legitimacy depends on site performing the reading). Global tourism sector: d ≈ 0.25 (organized, mobile, captures revenue but depends on state permission). Secularist elites: d ≈ 0.10 (identity-locked beneficiaries — their political self-concept requires this reading). Heritage scholars: d ≈ 0.30 (beneficiaries with mobile exit but high dependence on access). Islamic worship claimants: d ≈ 0.90 (identity-locked payers — waqf dedication makes exit theologically impossible). Muslim pilgrims: d ≈ 0.95 (trapped — no substitute site, immediate devotional need). Orthodox claimants: d ≈ 0.85 (excluded payers — would bear costs of any restitution but have no voice). Conservation NGOs: d = 0.50 (analytical observers).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (secularizing a conquest monument to signal Western modernity) was live in 1934. By 2024, Turkey is no longer a Western-aspiring secular republic in the same sense; the secularist elite that founded the reading has lost political hegemony. The arrangement persists because it now serves new extractive functions: tourism revenue for the current Islamist-leaning government, and UNESCO legitimacy for international standing. The mandate has atrophied but the constraint has not — it has been repurposed. This is not a piton (no theatrical maintenance of a dead function); the function shifted from secular signaling to revenue extraction. The universal heritage reading is now a Tangled Rope with different beneficiaries than its founders intended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the universal heritage reading a distinct constraint from the Islamic sovereignty and Orthodox restitution readings, or are they observables of a single constraint?',
    'Apply the ε-invariance test: if measuring the site''s governance under the universal heritage frame yields ε≈0.72 but the Islamic sovereignty frame yields ε≈0.35 (worship sovereignty as low-extraction coordination for Muslims), they are different constraints. The UNESCO inscription and Turkish law instantiate the universal heritage reading as a specific legal-administrative arrangement with its own ε.',
    'If they are one constraint, the corpus must model observable-dependent classification (forbidden by DP-001). If three constraints, each gets its own story linked by network.affects_constraints — this is the correct decomposition per the BGS worked example.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system kernel decomposition: one substrate, three readings, three constraints.').

omega_variable(
    secularist_elite_identity_lock,
    'Is the secularist Turkish elite''s identity lock to the museum frame genuine (cannot exit without identity dissolution) or performative (political posture with material exit options)?',
    'Track emigration, political adaptation, and discursive shifts among self-identified secularists after the 2020 reconversion. If the cohort shrinks via exit (emigration, depoliticization) rather than identity dissolution, the lock is performative.',
    'If performative, their directionality shifts toward mobile (d≈0.35) and their extraction experience drops. If genuine, d≈0.10 holds and they remain a concentrated beneficiary cohort with high χ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularist_elite_identity_lock, empirical, 'Identity lock mechanism for secularist beneficiaries — professional/class exit vs. identity fusion.').

omega_variable(
    tourism_revenue_allocation,
    'What fraction of Hagia Sophia tourism revenue actually funds conservation vs. general treasury?',
    'Turkish Court of Accounts audit or parliamentary inquiry into ticket revenue allocation. UNESCO reactive monitoring reports may contain partial data.',
    'If >80% funds conservation, extraction drops toward coordination cost (ε→0.3). If <20%, the universal heritage frame is a revenue extraction mechanism (ε→0.85). Current 0.72 assumes ~30% conservation allocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tourism_revenue_allocation, empirical, 'Revenue transparency determines whether coordination function is genuine or cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Islamic worship structural (legal ban, tourist scheduling) or internalized (Muslims accepting subordinate status)?',
    'Post-2020 ethnography: do Muslim worshippers experience the hybrid regime as suppression, or has the universal heritage frame been internalized as legitimate? Survey data on devotional experience vs. tourist presence.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the subject after exit. If structural only, suppression drops when legal barriers lift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in interpersonal-religious constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1934, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hss_uhr_tr_t1934, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1934, 0.25).
narrative_ontology:measurement(hss_uhr_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(hss_uhr_tr_t1985, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(hss_uhr_tr_t2000, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(hss_uhr_tr_t2010, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(hss_uhr_tr_t2020, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(hss_uhr_tr_t2024, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(hss_uhr_be_t1934, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1934, 0.35).
narrative_ontology:measurement(hss_uhr_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(hss_uhr_be_t1985, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(hss_uhr_be_t2000, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(hss_uhr_be_t2010, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(hss_uhr_be_t2020, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(hss_uhr_be_t2024, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hss_uhr_su_t1934, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1934, 0.65).
narrative_ontology:measurement(hss_uhr_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(hss_uhr_su_t1985, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement(hss_uhr_su_t2000, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(hss_uhr_su_t2010, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(hss_uhr_su_t2020, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(hss_uhr_su_t2024, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.08).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This reading, the Islamic sovereignty reading, and the Orthodox restitution reading form the Hagia Sophia constraint family. All three instantiate the same physical/historical substrate (kernel_id: hagia_sophia_substrate) but declare different legitimacy sources, different beneficiary/victim sets, and different ε values. The universal heritage reading (this story) has the highest ε (0.72) because it extracts tourism revenue and ideological capital while suppressing worship sovereignty. The Islamic sovereignty reading likely has lower ε for Muslim worshippers (coordination of worship) but higher ε for non-Muslims (exclusion). The Orthodox restitution reading is currently counterfactual (no state power) but would have its own ε if instantiated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__universal_heritage_reading, institutional, 0.15).
constraint_indexing:directionality_override(hagia_sophia_substrate__universal_heritage_reading, organized, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
