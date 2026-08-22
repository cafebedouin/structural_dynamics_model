% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia as Sovereign Waqf Mosque Under Turkish State Authority (Islamic Sovereignty Reading)
 *   domain: cultural heritage/sovereignty/religious authority
 *
 * SUMMARY:
 *   Since July 2020 the Hagia Sophia has operated as a functioning mosque
 *   under Diyanet administration, following a Council of State annulment of
 *   the 1934 museum decision and a presidential decree, on the legitimacy
 *   claim that the 1453 conquest lawfully established the site as Islamic
 *   endowment property whose worship purpose was unlawfully interrupted. The
 *   arrangement delivers real worship coordination while imposing asymmetric
 *   costs: secularist Turks lost a republican civic symbol, non-Muslim
 *   visitors face scheduled and chaperoned access, the Ecumenical
 *   Patriarchate's requests for shared use are declined, and the UNESCO
 *   regime was refused consultation and denied management jurisdiction.
 *   CONSTRAINT FAMILY NOTE: this file instantiates ONE reading of the
 *   hagia_sophia_substrate kernel. The sibling stories
 *   (universal_heritage_reading, orthodox_restitution_reading) are separate
 *   constraints with their own epsilon values and stakeholder structures —
 *   under the universal-heritage reading the standing arrangement appears as
 *   a stewardship failure with low inherent extraction; under the
 *   orthodox-restitution reading the beneficiary and victim sets invert. This
 *   file's epsilon (0.64) is authored for THIS reading's referent only: the
 *   standing arrangement as sovereign Islamic worship space, assessed by this
 *   reading's own lights, which register both the fulfilled endowment and the
 *   political-consolidation operation riding on it.
 *
 * KEY AGENTS:
 *   - akp_political_coalition: Agenda setter (institutional/arbitrage) — drove the reversal, collects the political consolidation, retains levers to alter the arrangement again
 *   - diyanet_administration: Administrator and secondary beneficiary (institutional/constrained) — runs the site day to day, expanded mandate
 *   - turkish_islamic_constituency: Primary coordinated beneficiary (organized/mobile) — regained worship access and public standing
 *   - global_sunni_ummah: Symbolic beneficiary (moderate/mobile) — receives the site as a sovereignty emblem, no material dependence
 *   - turkish_secularists: Primary domestic payer (organized/constrained) — lost a shared civic symbol, no counter-symbol, no exit
 *   - non_muslim_visitors: Payer with shallow exit (powerless/mobile) — scheduled, chaperoned access; can substitute other destinations
 *   - unesco_world_heritage_regime: Excluded payer (institutional/trapped) — consultation refused, jurisdiction denied, locked into the inscription relationship
 *   - ecumenical_patriarchate: Identity-locked payer (moderate/identity_locked) — denied liturgical access, canonically bound to the city
 *   - academic_byzantinist_community: Analytical observer — sees the full structure across all phases of the building's history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.64).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.58).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia as Sovereign Waqf Mosque Under Turkish State Authority (Islamic Sovereignty Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural heritage/sovereignty/religious authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '83ea39a4-ef17-45c3-90dc-d83c9b8206d6').
narrative_ontology:cs_kernel_codification('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', fixed_text).
narrative_ontology:cs_authority_grounding('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', lineage).
narrative_ontology:cs_interpretation_layer_present('83ea39a4-ef17-45c3-90dc-d83c9b8206d6').
narrative_ontology:cs_reading_relation('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', foundational, conquest_establishes_lawful_title).
narrative_ontology:cs_axiom_status(conquest_establishes_lawful_title, holdable).
narrative_ontology:cs_axiom_grounding('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', conquest_establishes_lawful_title, conventional).
narrative_ontology:cs_axiom('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', foundational, waqf_deed_binds_successor_state).
narrative_ontology:cs_axiom_status(waqf_deed_binds_successor_state, holdable).
narrative_ontology:cs_axiom_grounding('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', waqf_deed_binds_successor_state, conventional).
narrative_ontology:cs_reference_frame('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', sovereign_waqf_mosque_under_state_trusteeship).
narrative_ontology:cs_drift_state('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', post_2020_restoration, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('83ea39a4-ef17-45c3-90dc-d83c9b8206d6', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, global_sunni_ummah).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_secularists).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, ecumenical_patriarchate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governing political movement that campaigned to undo the 1934 museum settlement, secured the Council of State annulment and the July 2020 presidential decree reopening the building for prayer, and now cites the restoration in rallies and campaign messaging. Collects the mobilization value of the symbol and retains the legal and administrative levers to alter the arrangement again.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% State directorate of religious affairs that received the site into its mosque portfolio: appoints imams, sets prayer schedules, staffs the visitor galleries, and enforces the rules separating worship from tourism. Its national mandate, staffing, and visibility grew with custody of the country's most visited monument.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration, beneficiary).

% Religiously observant voters and congregations for whom the reopening returned a landmark of the conquest era to active worship. Many attend Friday prayers there; the benefit is partly devotional and partly the public standing the symbol confers. Daily worship needs are met at many other mosques, so nothing in ordinary religious life depends on this particular building.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% Worldwide Sunni community that receives the site as a symbol of restored Islamic sovereignty over a former imperial capital. The connection is ceremonial and mediated through broadcasts and anniversary observances, and carries no material dependence on the building's management.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, global_sunni_ummah, beneficiary,
    moderate, civilizational, mobile, global).

% Kemalist and republican-leaning citizens and opposition voters who regarded the museum settlement as a pillar of the republic's secular order. They lost a civic symbol they considered collectively owned, read the reversal as a partisan appropriation of a shared monument, protested the decision, and remain inside the polity with no comparable counter-symbol to mobilize.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_secularists, payer,
    organized, generational, constrained, national).

% Tourists and pilgrims of other faiths who visit outside prayer hours, view the interior from a designated gallery, and cannot enter the prayer hall during the five daily services or attend any liturgy. Admission is free, but access is scheduled around worship and chaperoned; those deterred by the restrictions can spend their itinerary elsewhere.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, mobile, global).

% The World Heritage system within which the property is inscribed as part of the Historic Areas of Istanbul. Its committee asked to be consulted before the status change, was refused, and has since been unable to review or shape the site's management plan; its leverage is limited to monitoring reports and listing deliberations it cannot cheaply walk away from.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime, excluded,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime, payer).

% The Istanbul-based see of Eastern Orthodoxy, whose cathedral the building originally was. It has asked that Christians be permitted to worship there alongside Muslims, has been declined, and remains anchored in the city by its own fifteen-century history and canon law; its petitions pass through a state apparatus it does not control and cannot relocate away from.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, ecumenical_patriarchate, payer,
    moderate, generational, identity_locked, global).

% Scholars of Byzantine and Ottoman art and architecture who study the building across all its phases. They publish on its mosaics, inscriptions, and structural history, advise heritage bodies informally, and hold no administrative power over how the site is used.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, academic_byzantinist_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a monumental, centrally located worship space for Istanbul's Muslim population, schedules tourist access around the five daily prayers, and concentrates administration of the country's most visited monument under a single religious directorate.
% TRANSFER_FUNCTION: Moves symbolic capital and political consolidation to the governing coalition and administrative scope to the religious directorate; moves unrestricted access away from non-Muslim visitors, liturgical presence away from the Orthodox patriarchate, and management jurisdiction away from the international heritage regime; moves the site's civic meaning from a shared republican monument to sovereign Islamic worship space.
% ABSENT_VOICES: Turkish secularists protested publicly but held no seat in the decision; the Ecumenical Patriarchate's request for shared Christian use was declined without negotiation; UNESCO's request for pre-decision consultation was refused; non-Muslim visitors had no procedural voice at all. Unanimity behind the arrangement exists only because the opposing seats were kept outside the room in which it was made.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight and the building reverted to museum status, prayer schedules and Diyanet staffing would withdraw, the visitor regime would reorganize around ticketed full access, the coalition would lose a recurring mobilization asset and pivot to other symbols, diplomatic objections would subside, and the heritage regime would resume management-plan review — the surrounding institutional landscape would visibly rearrange.
% FOUNDING_PROBLEM: In this reading's genealogy, the 1934 museum conversion severed the building from the purpose of its fifteenth-century endowment; the arrangement restores the conqueror's waqf to worship and reasserts Islamic sovereignty over the symbolic center of the former imperial capital.
% FOUNDING_PROBLEM_CORROBORATION: The existence and terms of the Ottoman endowment deed are corroborated by surviving vakfiye documents and independent Ottoman legal historians — outside the benefiting parties. But whether that deed remains binding on the successor republic is attested only within the Islamic-legal tradition and by the coalition's own organs (the Council of State majority, the presidency); no external party — not UNESCO, not the patriarchate, not independent international lawyers — attests the continuing bindingness, and several expressly deny it. The historical substrate is corroborated; the living obligation is not.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-high (0.64) because the arrangement's costs fall on identifiable parties — ideological defeat for secularists, restricted access for non-Muslims, jurisdiction denial for the heritage regime, liturgical exclusion for the patriarchate — while the gains concentrate on the governing coalition as a reusable mobilization asset. Suppression (0.58) is structural-dominant: jurisdictional exclusion, scheduling and chaperoning of access, policing of protests at the 2020 opening, and refusal of consultation account for most of it; a smaller internalized component persists as resignation and self-censorship among secularist constituencies (roughly four-fifths structural, one-fifth internalized). Theater ratio (0.30) reflects a growing performative share — televised openings, broadcast calls to prayer, anniversary ceremonies aimed at domestic audiences — atop a worship function that is genuinely practiced daily. Accessibility collapse is low (0.32): substitute mosques, museums, and heritage sites abound, and nothing in daily life depends on this particular building. Resistance (0.58) is sustained: domestic opposition, Greek and patriarchal objections, and repeated World Heritage Committee concern. The three measurement series run on one shared annual grid (months since the July 2020 conversion); intra-year pulsation driven by Ramadan surges and election calendars is smoothed by annual sampling, and the slow monotonic rise in all three series tracks the symbol's compounding reuse across successive campaign cycles rather than a discrete ratchet event.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat the arrangement is a restored order — the reading's own reference frame achieved after an eighty-six-year interruption — and its costs register as the ordinary price of correcting a historical wrong. From the payer seats the same structure operates as exclusion administered by the party that benefits from it. The two institutional outsiders diverge further: the heritage regime experiences jurisdiction denial as a procedural injury it is trapped inside, while the patriarchate experiences the same arrangement as a continuation of a dispossession it cannot leave. The analytical observer sees a hybrid: a working worship function carrying a growing political-performance layer and a stable architecture of asymmetric access.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the coalition, the directorate, the Islamic constituency, and the ummah near the subsidized end: the arrangement pays them symbolic and administrative returns. Victim declarations place the secularists, visitors, heritage regime, and patriarchate near the target end. Exit modulation then differentiates within each pole. The mobile visitor seat's effective burden is damped — its harm is real but shallow, purchasable around with an alternate itinerary. The identity-locked patriarchate and the trapped heritage regime sit nearer the full-target end: neither can exit the relationship that binds it to the site. The constrained secularist seat is amplified by its inability to generate a counter-symbol or leave the polity. No directionality overrides were needed: beneficiary/victim declarations plus exit options reproduce the true structural relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the waqf interrupted in 1934 — is contested rather than dead: the reading's holders treat the endowment obligation as perpetual, while opponents hold the problem was settled by the republic and has been manufactured for mobilization. The mismatch consumer therefore reads contested-status against a world_rearranges verdict and finds no zombie flag. But the resolution paths matter: if the bindingness omega resolves negatively, the mandate dies while the arrangement persists on mobilization value alone, and the trajectory bends toward theatrical maintenance (piton) or, if the coalition's capture of the gains hardens, toward a mobilization snare. Conversely, if devotion proves the dominant driver, the coordination function is sturdier than the theater trend suggests and the arrangement stabilizes as a durable hybrid. The tangled-rope classification is what keeps both misreadings blocked: the genuine daily worship function defeats a pure-extraction reading, and the enforced asymmetries — who is coordinated and who pays through the same structure — defeat a pure-coordination reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the islamic_sovereignty_reading of the hagia_sophia_substrate kernel; the disagreement among readings is located in which source grounds the site''s legitimate governance — the conquest-and-waqf lineage (this reading), the Byzantine founding (orthodox_restitution_reading), or transnational heritage stewardship (universal_heritage_reading). What follows structurally if a sibling reading governed instead?',
    'Forum competition: whichever warrant prevails — Turkish constitutional organs, UNESCO deliberation, or ecclesiastical diplomacy — selects the governing reading and with it the operative constraint.',
    'Under the universal-heritage reading the victim set contracts (consultation rights restored, access liberalized) and epsilon falls toward shared-stewardship levels; under the orthodox-restitution reading the beneficiary and victim sets invert, with the coalition becoming a payer and the patriarchate a beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings instantiate different constraints with different epsilon and stakeholder structures.').

omega_variable(
    waqf_bindingness_on_successor_state,
    'Does a fifteenth-century sultanic endowment deed impose live obligations on the modern Turkish republic as successor to Ottoman sovereign rights?',
    'Comparative state-succession doctrine applied to religious foundations, the Council of State plenary reasoning, and the scholarly consensus of Ottoman legal historians operating outside the benefiting coalition.',
    'If the deed does not bind the successor state, the arrangement''s coordination justification collapses into political symbolism alone and the constraint drifts toward extraction maintained by inertia and mobilization value; if it binds, a substantial part of the measured extraction is the price of honoring a standing obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_bindingness_on_successor_state, conceptual, 'Whether the founding obligation survives the transition from empire to republic.').

omega_variable(
    devotion_vs_consolidation_weight,
    'Is the primary driver of the arrangement religious fulfillment of the endowment or electoral-political consolidation?',
    'Revealed-preference analysis: budget allocation between worship services and ceremony and media production, timing of policy moves against the election calendar, and behavior in episodes where devotional and mobilization interests diverge.',
    'If consolidation dominates, the theater ratio is understated and the arrangement trends toward a mobilization asset whose maintenance is increasingly performance; if devotion dominates, the coordination function is sturdier than the theater trend suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devotion_vs_consolidation_weight, empirical, 'Relative weight of religious and political drivers in the arrangement''s operation.').

omega_variable(
    visitor_restriction_materiality,
    'How material are the costs borne by non-Muslim visitors, given free admission, gallery access outside prayer hours, and abundant substitute destinations?',
    'Visitor-flow and survey data comparing the pre- and post-2020 experience, plus complaint and incident records at the galleries.',
    'If the restrictions are largely nominal, the visitor seat''s contribution to measured extraction thins and the arrangement sits closer to a coordination arrangement with symbolic frictions; if hours are cut or galleries closed, extraction deepens and the visitor seat''s effective burden rises despite its mobile exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(visitor_restriction_materiality, empirical, 'Material severity of the access restrictions imposed on non-Muslim visitors.').

omega_variable(
    precedent_cascade_risk,
    'Does annulling the 1934 settlement on waqf-lineage grounds expose other early-republic settlements — converted institutions, secularized foundations, other monument statuses — to the same reversal logic?',
    'Track subsequent litigation invoking the Council of State''s reasoning and any legislative attempts to entrench the remaining settlements against it.',
    'If a cascade materializes, the stakes carried by the secularist seat inflate well beyond this single site and the arrangement functions as the leading edge of a broader revision program; if contained, it remains a bounded exception with limited spillover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precedent_cascade_risk, conceptual, 'Whether the reversal logic generalizes beyond this site to the wider early-republic settlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hagi_tr_t12, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(hagi_tr_t24, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(hagi_tr_t36, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 36, 0.26).
narrative_ontology:measurement(hagi_tr_t48, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 48, 0.28).
narrative_ontology:measurement(hagi_tr_t60, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 0, 0.56).
narrative_ontology:measurement(hagi_be_t12, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(hagi_be_t24, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(hagi_be_t36, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 36, 0.61).
narrative_ontology:measurement(hagi_be_t48, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement(hagi_be_t60, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 60, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hagi_su_t12, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(hagi_su_t24, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(hagi_su_t36, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 36, 0.56).
narrative_ontology:measurement(hagi_su_t48, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 48, 0.57).
narrative_ontology:measurement(hagi_su_t60, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Hagia Sophia question' conflates three structurally distinct claims about where the site's legitimate governance derives from; per the epsilon-invariance principle they are modeled as a three-story constraint family sharing the hagia_sophia_substrate kernel. This story carries the islamic_sovereignty_reading (epsilon 0.64, tangled rope: real worship coordination carrying enforced asymmetric exclusion). The universal_heritage_reading story carries the shared-stewardship claim (low extraction, rope-flavored, its epsilon computed against a consultative-management referent). The orthodox_restitution_reading story carries the founding-title claim (its beneficiary and victim sets invert relative to this file). This reading structurally influences both siblings: its 2020 victory changed the legitimacy conditions and resource availability under which they operate, and its conquest-title axiom forecloses the restitution axiom within any single legal framework while coexisting with the heritage claim across competing forums.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
