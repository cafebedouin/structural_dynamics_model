% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Hagia Sophia Universal-Heritage Museum Regime (Universal Heritage Reading)
 *   domain: cultural/political/religious
 *
 * SUMMARY:
 *   From 1934 to 2020 the building was administered as a state museum under a
 *   legitimacy claim that its significance belongs to humanity as a whole and
 *   outranks any single confession's or nation's claim to possess it. This
 *   story instantiates the universal_heritage_reading of the
 *   hagia_sophia_substrate kernel: the museum arrangement itself is the
 *   standing arrangement under assessment, with its own ticketing economy,
 *   conservation apparatus, worship prohibition, and ideological function.
 *   The claimed type (tangled_rope) is stated from structural analysis — the
 *   arrangement genuinely coordinates preservation and multi-claimant access
 *   while asymmetrically extracting revenue and suppressing congregational
 *   worship — and the metrics are authored independently as descriptive
 *   facts; the engine computes per-seat classifications from the structural
 *   data, and divergence between claim and computed type is signal, not
 *   error. Sibling readings (islamic_sovereignty_reading,
 *   orthodox_restitution_reading) are separate constraints linked through the
 *   network section. KEY AGENTS (by structural relationship): -
 *   turkish_state_cultural_administration: Agenda-setting administrator
 *   (institutional/arbitrage) — runs the museum, collects admission revenue,
 *   enforces the worship prohibition - global_tourism_sector: Primary
 *   commercial beneficiary (organized/mobile) — monetizes visitor flow -
 *   secularist_turkish_elites: Ideological beneficiary
 *   (institutional/identity_locked) — collects symbolic capital from the
 *   arrangement's existence - international_scholarly_community:
 *   Knowledge-sector beneficiary (organized/constrained) — depends on
 *   administered access - international_visitors: Mass beneficiary-payer
 *   (moderate/mobile) — pays admission, receives access -
 *   muslim_worship_communities: Primary target (organized/trapped) — barred
 *   from congregational worship for the full interval -
 *   orthodox_ecumenical_patriarchate: Secondary target (moderate/trapped) —
 *   founding-community claims unrecognized - waqf_claimant_networks: Excluded
 *   voice (moderate/trapped) — endowment-based claims never adjudicated -
 *   unesco_world_heritage_committee: Analytical observer
 *   (institutional/analytical) — monitors and recommends without enforcement
 *   power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.72).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia Universal-Heritage Museum Regime (Universal Heritage Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural/political/religious").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '32d52620-91a4-4d9c-8c7b-6e85f65ee465').
narrative_ontology:cs_kernel_codification('32d52620-91a4-4d9c-8c7b-6e85f65ee465', formalized).
narrative_ontology:cs_authority_grounding('32d52620-91a4-4d9c-8c7b-6e85f65ee465', expertise).
narrative_ontology:cs_interpretation_layer_present('32d52620-91a4-4d9c-8c7b-6e85f65ee465').
narrative_ontology:cs_reading_relation('32d52620-91a4-4d9c-8c7b-6e85f65ee465', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('32d52620-91a4-4d9c-8c7b-6e85f65ee465', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('32d52620-91a4-4d9c-8c7b-6e85f65ee465', foundational, cultural_significance_transcends_confessional_claim).
narrative_ontology:cs_axiom_status(cultural_significance_transcends_confessional_claim, holdable).
narrative_ontology:cs_axiom_grounding('32d52620-91a4-4d9c-8c7b-6e85f65ee465', cultural_significance_transcends_confessional_claim, deontological).
narrative_ontology:cs_axiom('32d52620-91a4-4d9c-8c7b-6e85f65ee465', secondary, liturgical_use_subordinate_to_preservation_and_access).
narrative_ontology:cs_axiom_status(liturgical_use_subordinate_to_preservation_and_access, holdable).
narrative_ontology:cs_axiom_grounding('32d52620-91a4-4d9c-8c7b-6e85f65ee465', liturgical_use_subordinate_to_preservation_and_access, instrumental).
narrative_ontology:cs_reference_frame('32d52620-91a4-4d9c-8c7b-6e85f65ee465', universal_heritage_museum_regime).
narrative_ontology:cs_drift_state('32d52620-91a4-4d9c-8c7b-6e85f65ee465', post_2020_reconversion, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('32d52620-91a4-4d9c-8c7b-6e85f65ee465', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_scholarly_community).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, muslim_worship_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, international_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_ecumenical_patriarchate).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, world_heritage_universal_value_thesis).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, kemalist_secularization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the building as a state museum under the Ministry of Culture: sets opening hours, ticket prices, and visitor rules; staffs conservation and security; directs admission revenue into ministry and treasury budgets; and enforces the legal prohibition on congregational prayer inside the building. Its administrative instruments (council decrees, ministerial orders) define what the site is permitted to be, and it demonstrated in 2020 that it can redefine the arrangement by administrative act.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_cultural_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Tour operators, hotel and airline interests, licensed guides, and souvenir vendors build itineraries and businesses around visitor traffic to the building. Visitor volume sets their revenue, and they lobby for stable opening regimes and marketing support. If traffic shifted elsewhere they would reroute their products with little lasting loss.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% Byzantine, Ottoman, and art-historical researchers together with conservators depend on state permits and museum cooperation for site access, survey work, and publication. The museum's documentation and restoration campaigns produced much of the modern scholarship on the building. Their research agendas bind them to continued access under whatever regime grants it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_scholarly_community, beneficiary,
    organized, generational, constrained, global).

% Republican-establishment figures, secularist civil society, and parts of the urban professional class authored the 1934 conversion and thereafter treated the museum status as an emblem of the republic's secular character and Western orientation. They defend the arrangement in press, courts, and electoral politics. Their attachment is to what the arrangement says about who they are; losing it reads to them as losing ground in a domestic identity conflict.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter).

% Millions of paying visitors a year enter under ticketed rules and receive guided access to the building's layered history. They pay admission and consume the experience; individually they have no role in governance and would substitute other destinations if access conditions changed.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_visitors, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, international_visitors, payer).

% Congregations and pious associations in Turkey sought to pray in the building throughout the museum period and were legally barred from congregational worship; individual petitioners attempting prayer inside were removed by security staff. Religious foundations and civil-society groups filed recurring petitions and court applications for restored worship rights, all unsuccessful for the duration of the arrangement.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, muslim_worship_communities, payer,
    organized, generational, trapped, national).

% The Ecumenical Patriarchate and Orthodox faithful regard the building as their founding cathedral; under the museum arrangement no liturgical use was possible and restitution claims found no interlocutor. The community's Istanbul presence contracted sharply over the period, leaving its claim voiced mainly through diaspora institutions.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_ecumenical_patriarchate, payer,
    moderate, generational, trapped, global).

% Descendants of endowment trustees, Islamic-law scholars, and heritage activists holding endowment-based title arguments were never consulted in the 1934 conversion and held no seat in museum-era administration. They circulated legal memoranda and organized annual demonstrations outside the building; their claims were met with security presence rather than adjudication.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, waqf_claimant_networks, excluded,
    moderate, generational, trapped, national).

% Inspected and inscribed the Historic Areas of Istanbul on the World Heritage List in 1985, monitors the property's state of conservation through advisory missions, and registers formal concern when the site's status or setting changes. It issues recommendations but commands no enforcement force over the administering state.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_world_heritage_committee, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, turkish_state_cultural_administration).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a governance form under which a single irreplaceable, multi-claimant sacred structure can be preserved, funded, and opened to visitors of every faith and nation without any governing authority adjudicating between the competing theological claims to it.
% TRANSFER_FUNCTION: Moves admission revenue from international visitors into ministry budgets, the treasury, and the surrounding tourism economy; moves symbolic capital (the image of secular modernity and cosmopolitan prestige) to the Turkish state and its secularist establishment; and moves exclusive access away from Muslim congregants and Orthodox liturgy toward a global ticketed visitor public.
% ABSENT_VOICES: Endowment-based claimants and the Muslim religious establishment were absent from the 1934 decision and from museum-era administration; Orthodox ecclesial voices were largely absent, their Istanbul presence having collapsed mid-century; neighborhood communities around the building had no seat. Their objections surface only in the petition and demonstration record.
% DISAPPEARANCE_RATIONALE: If the museum arrangement vanished overnight, access rules, revenue flows, conservation funding channels, and the secularist identity anchor would all rearrange — as they demonstrably began to when the arrangement was terminated in 2020: worship schedules resumed, revenue accounting changed, and the international heritage apparatus registered the loss.
% FOUNDING_PROBLEM: A newly founded secular republic inherited an imperial worship site carrying overlapping confessional titles and a deteriorating fabric: the 1934 conversion was built to neutralize confessional ownership conflict, fund and professionalize conservation, and signal the republic's secular modernity to itself and to Europe.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: UNESCO advisory-body evaluations and international conservation missions attest the conservation rationale was real and ongoing; the petition, litigation, and demonstration record compiled by Islamic civil society attests that the confessional problem was suppressed rather than resolved; Council of State proceedings and dissenting legal opinions document the dispute from outside the administering seat. No corroborating source attests that the confessional problem was dead.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.72 at interval end) because admission revenue scaled with mass tourism while flowing predominantly to ministry budgets and the surrounding tourism economy rather than proportionally to fabric conservation, and because the arrangement converted a worship space into a priced attraction. Suppression (0.75) reflects the legal prohibition on congregational prayer maintained across the whole interval, security enforcement against worshippers, and the absence of any adjudicating forum for endowment-based claims; suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater ratio (0.35) is moderate: conservation, documentation, and curation are real functions, but a growing share of activity serves the visitor-commodity experience and the arrangement's image function. Accessibility collapse (0.55) is partial: once UNESCO inscribed the property and the universal-heritage frame became the international default, dual-use alternatives were framed as category violations, yet counter-framings persisted in domestic politics and ultimately prevailed. Resistance (0.60) was sustained: recurring petitions, annual demonstrations, court applications, and finally an administrative reversal. The three measurement series share one time grid (t=0,15,30,45,60,75,86). Extractiveness and theater rise monotonically with tourism massification; the suppression series traces enforcement intensity — zealous in the early republican period, normalizing mid-century, hardening again after t=60 as petitions and demonstrations raised the stakes of holding the prohibition. Base properties describe the arrangement's end-state.
 *
 * PERSPECTIVAL GAP:
 *   From the administering seat the arrangement is stewardship: a fragile monument protected, opened to everyone, funded by those who benefit from it. From the tourism and scholarship seats it is infrastructure their livelihoods ride on. From the barred-congregation seat the same ticket hall is a locked prayer space with a price on the door. The engine computes these divergent per-seat classifications from power, exit, and directionality data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The administering administration sits near the beneficiary end: it writes the rules and receives the revenue stream. The tourism sector's directionality is pulled low by direct gain, and its arbitrage-grade exit means its effective burden is minimal while its benefit is maximal. Secularist elites collect symbolic rather than monetary returns; their identity-locked exit makes them the arrangement's most persistent defenders despite bearing little material cost — identity fusion with the secular-republic frame binds them to the arrangement's persistence. Muslim worship communities sit nearest the full-target end: the prohibition falls directly on them, their claim is site-specific and non-relocatable, and they bore the arrangement's principal cost for the entire interval. The Patriarchate shares the target position with lower salience over most of the interval. International visitors sit near symmetric — they pay admission and receive access. UNESCO holds an analytical seat with no material flow.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination would erase the worship prohibition and the revenue asymmetry; reading it as pure occupation would erase the genuine conservation record and the open-access achievement. The tangled-rope classification holds both faces in one structure, which is what prevents mislabeling in either direction. The founding problem was never wholly dead — fabric conservation remained live throughout — so this is not a mandate outliving its function; the arrangement ended by repudiation (an administrative reversal adopting a sibling reading's frame), not by atrophy. The rising theater_ratio tracks commodification drift, not vestigiality: the functions atrophied into performance are the interpretive and access-equality functions, while the enforcement function stayed load-bearing to the end.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_attachment_location,
    'Which element of the substrate carries the site''s legitimacy — the conquest-era endowment lineage, the founding consecration, or a trans-confessional cultural value — and can more than one answer govern at once?',
    'Whichever framework the governing authority adopts becomes operative, as in the 2020 administrative reversal; doctrinal analysis of the competing legal instruments (endowment deeds, conversion decree, World Heritage inscription) maps which attachments each framework recognizes.',
    'Each answer instantiates a different constraint with a different beneficiary set, victim set, and epsilon; this file authors only the trans-confessional answer, and the sibling files author the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_attachment_location, conceptual, 'Where legitimacy attaches in the contested kernel; sibling readings occupy the other answers.').

omega_variable(
    conservation_revenue_allocation,
    'What share of admission revenue actually reached fabric conservation and site maintenance, versus general ministry and treasury budgets?',
    'Ministry budget disclosure and audit records cross-checked against documented conservation expenditure over the interval.',
    'A low conservation share shifts measured extraction further from coordination cost toward captured surplus; a high share supports the stewardship framing and lowers effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_revenue_allocation, empirical, 'Decomposes the ticketing economy into coordination cost versus captured surplus.').

omega_variable(
    dual_use_arrangement_feasibility,
    'Could scheduled congregational worship coexist with museum access and conservation standards, as at comparable multi-use heritage sites, without degrading either function?',
    'Comparative analysis of dual-use sacred heritage sites and engineering assessment of liturgical-use impacts on fabric and visitor throughput.',
    'If feasible, the worship prohibition was separable from the coordination function and counts as pure suppression; if infeasible, part of the measured suppression is the price of the preservation-and-access function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_use_arrangement_feasibility, conceptual, 'Whether the arrangement''s coordination and suppression components were structurally separable.').

omega_variable(
    victim_set_scope_asymmetry,
    'Did the arrangement''s prohibition fall uniquely on Muslim congregational worship, or equivalently on Orthodox liturgical claims whose bearers had largely departed the city?',
    'Archival comparison of petition records, liturgical requests, and administrative responses across both communities over the interval.',
    'A symmetric-prohibition finding widens the victim set and raises measured extraction; the asymmetric finding concentrates the target seat on the Muslim congregations as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_scope_asymmetry, empirical, 'Boundary of the arrangement''s victim set across the two barred communities.').

omega_variable(
    constructed_frame_or_discovered_commons,
    'Is the universal-heritage frame a constructed instrument serving identifiable beneficiaries — ticketing revenue and a secular-modernity signal — or a faithful description of the building''s trans-confessional significance?',
    'Trace the frame''s adoption history against beneficiary interests at each adoption point, and test whether equivalent conservation outcomes survive under non-universalist governance frameworks.',
    'If constructed-for-benefit, the arrangement reads as extraction riding a coordination story and the beneficiary seats dominate classification; if discovered, more of the measured extraction is the price of genuine commons governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_frame_or_discovered_commons, conceptual, 'Naturalness ambiguity of the universal-heritage frame itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 86).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hagi_tr_t15, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(hagi_tr_t30, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(hagi_tr_t45, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 45, 0.27).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(hagi_tr_t75, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 75, 0.33).
narrative_ontology:measurement(hagi_tr_t86, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 86, 0.35).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hagi_be_t15, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(hagi_be_t30, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(hagi_be_t45, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 45, 0.67).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 60, 0.69).
narrative_ontology:measurement(hagi_be_t75, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 75, 0.71).
narrative_ontology:measurement(hagi_be_t86, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 86, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(hagi_su_t15, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(hagi_su_t30, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(hagi_su_t45, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(hagi_su_t60, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(hagi_su_t75, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 75, 0.7).
narrative_ontology:measurement(hagi_su_t86, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 86, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Hagia Sophia's status' covers three structurally distinct legitimacy claims (per the epsilon-invariance principle): conquest-and-endowment sovereignty, founding-cathedral restitution, and trans-confessional universal heritage. Each is authored as its own constraint with its own epsilon, beneficiaries, and victims; together they form the hagia_sophia_substrate family. The universal-heritage reading was authored against both siblings in 1934, and the 2020 reversal shows the islamic_sovereignty_reading displacing this one; edges here record family membership, and each sibling file mirrors this note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
