% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Human Heritage (Museum-Framework Reading)
 *   domain: cultural heritage / sovereignty / religious authority
 *
 * SUMMARY:
 *   This story instantiates the 'universal heritage' reading of the Hagia
 *   Sophia kernel: the claim that the site's legitimacy rests on its status
 *   as shared human patrimony transcending any single confessional or
 *   national claimant. This is the framework under which the 1934
 *   secularization and subsequent museum administration operated for 86
 *   years, and which international heritage and tourism institutions continue
 *   to invoke even after the 2020 reversion to mosque status. Under this
 *   reading, the coordination story (neutral stewardship serving all
 *   humanity) is real but sits alongside a concentrated extraction structure:
 *   secularist Turkish state administrators and the global
 *   tourism/scholarship apparatus captured access, revenue, and interpretive
 *   control, while the specific and continuous local Islamic worship claim
 *   was suppressed as a matter of state policy for most of the 20th century.
 *   This story does NOT evaluate the Islamic sovereignty reading or the
 *   Orthodox restitution reading as competing hypotheses about the SAME
 *   constraint — those are separate constraints with their own ε and
 *   stakeholder structures, linked here via network.affects_constraints. The
 *   high extractiveness authored here reflects the concentration of tourism
 *   revenue and ideological benefit in a narrow set of state and
 *   institutional actors operating under a 'neutrality' banner that itself
 *   functioned as a substantive (not neutral) choice against a specific
 *   living religious claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.62).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Human Heritage (Museum-Framework Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural heritage / sovereignty / religious authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '83efc529-ccf6-473c-a361-f128dd37dbc4').
narrative_ontology:cs_kernel_codification('83efc529-ccf6-473c-a361-f128dd37dbc4', distributed).
narrative_ontology:cs_authority_grounding('83efc529-ccf6-473c-a361-f128dd37dbc4', extraction).
narrative_ontology:cs_interpretation_layer_present('83efc529-ccf6-473c-a361-f128dd37dbc4').
narrative_ontology:cs_reading_relation('83efc529-ccf6-473c-a361-f128dd37dbc4', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('83efc529-ccf6-473c-a361-f128dd37dbc4', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_axiom('83efc529-ccf6-473c-a361-f128dd37dbc4', foundational, heritage_value_transcends_confessional_ownership).
narrative_ontology:cs_axiom_status(heritage_value_transcends_confessional_ownership, holdable).
narrative_ontology:cs_axiom_grounding('83efc529-ccf6-473c-a361-f128dd37dbc4', heritage_value_transcends_confessional_ownership, conventional).
narrative_ontology:cs_axiom('83efc529-ccf6-473c-a361-f128dd37dbc4', secondary, secular_administration_is_neutral_stewardship).
narrative_ontology:cs_axiom_status(secular_administration_is_neutral_stewardship, overridden).
narrative_ontology:cs_axiom_grounding('83efc529-ccf6-473c-a361-f128dd37dbc4', secular_administration_is_neutral_stewardship, instrumental).
narrative_ontology:cs_reference_frame('83efc529-ccf6-473c-a361-f128dd37dbc4', kemalist_secular_museum_framework).
narrative_ontology:cs_drift_state('83efc529-ccf6-473c-a361-f128dd37dbc4', post_2020_reversion, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('83efc529-ccf6-473c-a361-f128dd37dbc4', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_scholarship_community).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, unesco_and_heritage_bodies).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, local_muslim_worship_claimants).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, istanbul_religious_community).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, secular_modernity_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, cultural_heritage_transcends_sectarian_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically administered the site as a museum (1934–2020) under Kemalist constitutional secularism, framing it as a neutral monument to civilizational continuity rather than a site of any single faith. They benefit from the site functioning as an ideological signal of Turkey's secular, Western-facing identity and from the international legitimacy this framing confers.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Tour operators, hospitality businesses, and cultural travel infrastructure profit from the museum framing, which permits unrestricted secular visitation, ticketed access, and marketing as a universally accessible wonder rather than an active mosque with prayer-time restrictions. Revenue streams depend on the site remaining maximally open to non-worship visitation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Byzantinists, art historians, and conservation scientists gain unrestricted research and preservation access when the site is administered as a museum. Reversion to active worship space has historically curtailed study of mosaics and iconography (covering, restricted access), so this community's professional and institutional standing is tied to the museum framing persisting.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_scholarship_community, beneficiary,
    organized, generational, mobile, global).

% World Heritage authorities certify the site's value precisely on the premise that it belongs to humanity rather than to any national or confessional claimant. Their institutional authority and funding relevance depend on sites like this remaining framed as universal patrimony rather than sovereign religious property.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_and_heritage_bodies, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, unesco_and_heritage_bodies, observer).

% Turkish religious constituencies who view continuous Islamic use since 1453 as the site's authentic identity experience the universal-heritage framing as suppression of an active waqf endowment and a living religious claim, subordinated to a secular-technocratic and international tourism agenda. Their exit options are limited to domestic political mobilization, which succeeded in 2020 but remains contested.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, local_muslim_worship_claimants, payer,
    organized, generational, constrained, national).

% Local worshippers whose access to the space as a functioning mosque was foreclosed for 86 years under the museum framework; they bore the practical cost of the universal-heritage narrative — an emptied prayer space converted to a paid-admission cultural attraction on their own historic ground.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, istanbul_religious_community, payer,
    moderate, biographical, constrained, local).

% Greek Orthodox communities and the Ecumenical Patriarchate would object that 'universal heritage' framing itself erases the site's specific Christian founding and Byzantine liturgical history by flattening it into a generic secular monument alongside other claims. They are not party to Turkish domestic administrative decisions and have no seat in the framing debate.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_christian_diaspora, excluded,
    organized, civilizational, trapped, global).

% The sovereign authority that legally instantiated and later dissolved the museum status (1934 decree; 2020 Council of State ruling). Adjudicates between the competing framings as a matter of domestic law, subject to international diplomatic pressure but ultimately unconstrained by any external body.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_constitutional_court_and_state, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, turkish_constitutional_court_and_state, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, ostensibly neutral framework under which a site claimed by multiple religious and national traditions can be preserved, studied, and visited without formally awarding sovereignty to any single claimant — avoiding the need for the Turkish state, Orthodox Christianity, and global heritage institutions to litigate the underlying ownership question directly.
% TRANSFER_FUNCTION: Moves access, ritual use, and interpretive authority away from the local Muslim worship community and toward secular state administrators, international tourism operators, and the global scholarly-heritage apparatus; moves tourism and prestige revenue toward the Turkish state and heritage-adjacent industries under the universal-patrimony brand.
% ABSENT_VOICES: The Ecumenical Patriarchate and global Orthodox diaspora are excluded from the framing debate entirely — 'universal heritage' forecloses their specific restitution claim just as effectively as it forecloses the Islamic sovereignty claim, but with no domestic political mechanism available to contest it, unlike Turkish Muslim constituencies who could and did mobilize domestically.
% DISAPPEARANCE_RATIONALE: If the universal-heritage framing disappeared as the operative legitimacy claim, the site's status would immediately default to being adjudicated as either a sovereign religious space (already partially realized in 2020) or an unresolved restitution question — international tourism revenue models, UNESCO's certification posture, and Turkey's secular-modernity self-presentation would all require reconstruction.
% FOUNDING_PROBLEM: In 1934, the young Turkish Republic needed to neutralize the site's status as both an active symbol of Ottoman-Islamic sovereignty and a live Christian restitution grievance, converting a contested religious monument into a depoliticized secular museum to consolidate Kemalist state authority and gain international legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: International heritage bodies and academic historians outside Turkey (UNESCO documentation, Byzantine studies literature) corroborate that the museum framing served a genuine depoliticization and preservation function through the 20th century. However, the 2020 Turkish Council of State ruling, domestic religious-nationalist political movements, and independent legal scholars outside both the Turkish state and the tourism/scholarship beneficiary set have concluded the secularization decree itself was procedurally irregular (issued by executive fiat rather than legislative process) and that the 'universal heritage' framing was, from its founding, an instrument of Kemalist state ideology rather than a neutral response to inter-confessional dispute.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68 by interval end) reflects the concentration of tourism revenue, scholarly access privilege, and ideological signaling value in the hands of the secular administrative apparatus and its international beneficiaries, extracted at the direct cost of the local worship community's access. Suppression (0.62) is substantial but not maximal — the framing operated through legal/administrative exclusion (decree, ticketing, prayer restriction) rather than violent coercion, and it was eventually reversed through domestic legal and political channels in 2020, indicating the suppression was not absolute or irreversible. Theater ratio (0.42) is moderate-high: a genuine coordination/preservation function exists (conservation science, mosaic restoration, cross-cultural access) but an increasing share of the 'neutral heritage' rhetoric functioned as legitimating cover for what was, from the founding, a specific ideological project of Kemalist secular nationalism — theater rose over the interval as the gap between the neutrality claim and the concentrated beneficiary structure became more visible through scholarship and eventually litigation. Accessibility collapse (0.5) is moderate: alternatives (worship access, alternative heritage-sharing arrangements like timed prayer access) were not fully foreclosed even during peak museum administration, and were in fact realized in 2020. Resistance (0.6) is substantial, reflecting sustained domestic religious-nationalist political mobilization against the framing across decades.
 *
 * DIRECTIONALITY LOGIC:
 *   Secularist Turkish elites and the international tourism/scholarship/heritage complex are structural beneficiaries — they collect revenue, prestige, and ideological validation from the universal-heritage framing without bearing its costs (d near the beneficiary end). Local Muslim worship claimants and the Istanbul religious community are the structural targets — their access to the site as living religious space was directly curtailed by the same framework that enriched the beneficiary set (d near the target end). The Turkish state itself occupies a dual position: it is simultaneously the agenda-setter administering the framework and, at a civilizational time horizon, a beneficiary of the international legitimacy the framing confers — this dual role is captured via secondary_role rather than an override, since the derivation from beneficiary/agenda-setter roles already produces the right structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (depoliticizing a contested 1934-era religious/national flashpoint) has a contested status: heritage institutions and much of the international scholarly community treat it as still live (the site remains genuinely contested among three traditions), while domestic Turkish religious-nationalist actors and the 2020 judicial ruling treat it as dead or as a pretext — the 'neutrality' was never neutral, and continuous Islamic religious use since 1453 makes the museum interregnum the anomaly, not the norm. This mismatch (founding_problem_status: contested, disappearance_verdict: world_rearranges) is exactly the signal the classification is built to surface: a coordination story (shared patrimony, avoiding sectarian conflict) that is real at the level of function but co-exists with an asymmetric extraction structure that concentrated all practical benefit in secular-state and international-tourism hands while imposing the cost of exclusion narrowly on the local Muslim community. Classifying this as tangled_rope rather than snare preserves the genuine coordination content (cross-cultural preservation, conflict avoidance) while still registering the concentrated, enforced cost transfer — collapsing it to snare would erase the real coordination function; collapsing it to rope would erase the documented suppression and beneficiary concentration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_versus_substantive_choice,
    'Was the 1934 secularization a genuinely neutral act of conflict-avoidance between competing religious claims, or was it a substantive ideological choice (Kemalist secular nationalism) that merely presented itself as neutral while suppressing the specific, continuous Islamic claim?',
    'Historical-archival analysis of the 1934 decree''s drafting process and contemporaneous state rhetoric; comparison with how the state treated the Orthodox restitution claim in the same period (whether both faiths were equally restricted, which would support genuine neutrality, or whether only Islamic worship was actively suppressed while the site remained legally unavailable to Orthodox claims as well, which would support the ideological-instrument reading).',
    'If the decree is found to be a substantively anti-religious (rather than neutral) act, the universal_heritage_reading''s coordination claim weakens considerably and the constraint moves further toward snare; if genuinely neutral in intent and effect at the time, the tangled_rope classification (real coordination function plus later-emergent extraction) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_versus_substantive_choice, conceptual, 'Whether the founding secularization was neutral conflict-avoidance or ideological suppression dressed as neutrality.').

omega_variable(
    kernel_reading_incommensurability,
    'Can the three readings of the hagia_sophia_substrate kernel (universal_heritage, islamic_sovereignty, orthodox_restitution) be reconciled under any single administrative arrangement, or does adopting any one reading necessarily foreclose the practical claims of the other two?',
    'Comparative institutional analysis of shared-use religious heritage sites elsewhere (e.g., Cordoba''s Mezquita-Catedral, Temple Mount/Haram al-Sharif time-sharing arrangements) to determine whether partial coexistence arrangements are structurally possible or whether the three readings are genuinely mutually exclusive in practice even if not in abstract principle.',
    'If coexistence arrangements are structurally feasible, this reading''s high suppression score may be more a function of specific administrative choices (full exclusion of worship rather than negotiated shared access) than an inherent feature of the universal-heritage framing itself, which would lower the appropriate extractiveness/suppression scores.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, empirical, 'Whether the kernel''s readings are structurally exclusive or admit negotiated coexistence.').

omega_variable(
    who_defines_universal,
    'Who has standing to define what counts as ''universal human heritage,'' and does that definitional authority itself encode a particular (Western-secular, museological) cultural framework rather than a genuinely cross-cultural consensus?',
    'Survey of UNESCO World Heritage criteria formation history and the cultural/institutional composition of bodies that adjudicate ''universal value'' designations, checking for systematic bias toward secular-museological framings over living-religious-use framings across other contested heritage sites.',
    'If the ''universal'' framework systematically favors secular-museological administration over living religious use across many sites (not just this one), the universal_heritage_reading''s beneficiary structure (global tourism/scholarship sector) reflects a broader structural pattern rather than a Hagia-Sophia-specific anomaly, which would support treating the extraction as more deeply embedded and harder to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_defines_universal, conceptual, 'Whether the definitional authority behind ''universal heritage'' itself encodes a particular cultural framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hagi_tr_t15, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(hagi_tr_t30, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(hagi_tr_t45, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(hagi_tr_t75, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement(hagi_tr_t90, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 90, 0.42).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hagi_be_t15, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(hagi_be_t30, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(hagi_be_t45, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(hagi_be_t75, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(hagi_be_t90, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hagi_su_t15, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(hagi_su_t30, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(hagi_su_t45, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(hagi_su_t60, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(hagi_su_t75, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement(hagi_su_t90, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 90, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the colloquial 'Hagia Sophia legitimacy question' per the ε-invariance principle. The universal_heritage_reading (this story) claims a distinct ε and beneficiary/victim structure from islamic_sovereignty_reading (which would show low ε from the perspective of continuous Islamic endowment claims recognized by the 2020 ruling, with Orthodox and secular-heritage constituencies as the excluded/victim set) and orthodox_restitution_reading (which would show a different beneficiary structure centered on the Ecumenical Patriarchate and Orthodox diaspora, with Islamic worship claimants and the Turkish secular state as the excluded set). Each reading is administered by a different authority structure (secular technocratic museum administration here; Diyanet/Turkish state religious authority under islamic_sovereignty_reading; Ecumenical Patriarchate under orthodox_restitution_reading) and should never be merged into a single measurement — doing so would average across genuinely distinct ε values rather than reporting the contested structure honestly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
