% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Hagia Sophia Sovereign Islamic Worship Arrangement (Islamic Sovereignty Reading)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   Since the July 2020 presidential decree — validated by the Council of
 *   State's annulment of the 1934 museum decision — the building functions as
 *   a congregational mosque administered by the Directorate of Religious
 *   Affairs, and its legitimacy narrative runs through the 1453 conquest and
 *   the continuous Mehmed II endowment. This story instantiates ONE reading
 *   of the hagia_sophia_substrate kernel: the islamic_sovereignty_reading,
 *   for which that narrative is the site's actual title. Per the ε-referent
 *   rule, ε is authored over the standing post-2020 arrangement (the
 *   arrangement the story is about), assessed by this reading's own lights:
 *   the reading holds the worship allocation itself legitimate and therefore
 *   cannot count it as extraction, but it does count the enforced
 *   exclusivity's collateral burdens — visitor restrictions, denied
 *   multilateral oversight, the defeated secular settlement, diplomatic
 *   friction — yielding moderate-high ε (0.62) rather than the higher values
 *   the sibling readings would author over the identical referent. The claim
 *   (tangled_rope) and the metrics are independent authored facts: the type
 *   is asserted from structure (a real, daily-performed worship coordination
 *   function carrying asymmetric, actively enforced burdens), and the metrics
 *   describe observed operation without being tuned to any predicted engine
 *   output. KEY AGENTS (by structural relationship): -
 *   akp_political_coalition: Agenda-setting beneficiary
 *   (institutional/arbitrage) — decrees and defends the arrangement; collects
 *   its political and symbolic proceeds - diyanet_administration: Operating
 *   beneficiary (institutional/constrained) — administers worship, staffing,
 *   and access rules - turkish_islamic_constituency: Primary beneficiary
 *   (organized/mobile) — regains sovereign worship space -
 *   sunni_ummah_global: Symbolic beneficiary (moderate/mobile) — reads the
 *   site's status as confessional victory - non_muslim_visitors: Target
 *   (powerless/mobile) — bear prayer-time closures, concealed mosaics,
 *   gallery limits - unesco_world_heritage_regime: Target
 *   (institutional/trapped) — denied oversight of a listed property's use -
 *   secularist_turkish_citizens: Target (organized/trapped) — bear the
 *   ideological reversal of the founding settlement -
 *   ecumenical_patriarchate: Target (moderate/trapped) — barred from any
 *   custodial path; objections carry no weight - hellenic_state: Secondary
 *   target (institutional/mobile) — bears diplomatic friction -
 *   byzantine_heritage_scholarship: Analytical observer
 *   (analytical/analytical) — documents conservation and access effects
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.55).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Sovereign Islamic Worship Arrangement (Islamic Sovereignty Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, 'cd21c93e-86e2-4571-b4c3-6d814555f22b').
narrative_ontology:cs_kernel_codification('cd21c93e-86e2-4571-b4c3-6d814555f22b', fixed_text).
narrative_ontology:cs_authority_grounding('cd21c93e-86e2-4571-b4c3-6d814555f22b', lineage).
narrative_ontology:cs_interpretation_layer_present('cd21c93e-86e2-4571-b4c3-6d814555f22b').
narrative_ontology:cs_reading_relation('cd21c93e-86e2-4571-b4c3-6d814555f22b', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('cd21c93e-86e2-4571-b4c3-6d814555f22b', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('cd21c93e-86e2-4571-b4c3-6d814555f22b', foundational, waqf_deed_confers_perpetual_worship_title).
narrative_ontology:cs_axiom_status(waqf_deed_confers_perpetual_worship_title, holdable).
narrative_ontology:cs_axiom_grounding('cd21c93e-86e2-4571-b4c3-6d814555f22b', waqf_deed_confers_perpetual_worship_title, conventional).
narrative_ontology:cs_axiom('cd21c93e-86e2-4571-b4c3-6d814555f22b', foundational, state_successor_custody_legitimate).
narrative_ontology:cs_axiom_status(state_successor_custody_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('cd21c93e-86e2-4571-b4c3-6d814555f22b', state_successor_custody_legitimate, conventional).
narrative_ontology:cs_axiom('cd21c93e-86e2-4571-b4c3-6d814555f22b', secondary, museum_era_invalid_alienation).
narrative_ontology:cs_axiom_status(museum_era_invalid_alienation, holdable).
narrative_ontology:cs_axiom_grounding('cd21c93e-86e2-4571-b4c3-6d814555f22b', museum_era_invalid_alienation, conventional).
narrative_ontology:cs_reference_frame('cd21c93e-86e2-4571-b4c3-6d814555f22b', conquest_waqf_settlement).
narrative_ontology:cs_drift_state('cd21c93e-86e2-4571-b4c3-6d814555f22b', contemporary_post_restoration, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('cd21c93e-86e2-4571-b4c3-6d814555f22b', '2026-08-20T12:00:00Z').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_global).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turkish_citizens).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, hellenic_state).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, waqf_perpetuity_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__islamic_sovereignty_reading, conquest_title_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governing coalition that campaigned to reinstate worship at the site, executed the reversal by presidential decree after the Council of State annulled the 1934 museum decision, and presides over the resulting arrangement. It receives the political consolidation: a signature promise kept for its religious base, a confessional identity signal at home and across the Sunni world, and a durable electoral asset. Its exit is effectively free — it wrote the current rules and can adjust access policy, ceremony, or administration at will.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% State directorate of religious affairs that operates the mosque: appoints clergy, sets prayer schedules, manages visitor routing and prayer-time closures, and stages major religious events. It gains its most prestigious custodianship and a corresponding budget and staffing footprint. It is bound into the state structure it serves and executes access policy decided above it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__islamic_sovereignty_reading, diyanet_administration, agenda_setter).

% Religiously observant Turkish citizens for whom the reconversion fulfilled a long-standing grievance against the secular settlement. They attend prayers in large numbers, especially on Fridays and in Ramadan, and read the building's status as public respect for their faith. Many mosques exist; attachment concentrates on this one, but nothing binds them to it materially.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% Worldwide Sunni faithful who follow the site's status as a confessional symbol; coverage of the reconversion resonated across Muslim-majority countries. The benefit is identificatory rather than material — no resources flow to them, and their relationship to the building is distant and voluntary.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_global, beneficiary,
    moderate, civilizational, mobile, global).

% Tourists and pilgrims of other faiths or none who visit the building. Since the reconversion they encounter prayer-time closures, mosaics and frescoes concealed behind curtains during services, restricted gallery access, and at times a segregated visitor protocol. Entry is free, visits outside prayer hours remain possible, and individual travelers can substitute other destinations — but the unhindered viewing experience the museum era offered no longer exists anywhere.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, mobile, global).

% The World Heritage system, which inscribed the property in 1985 within Historic Areas of Istanbul on account of its universal significance. The change of use proceeded without the consultation the system's procedures contemplate; the body expressed serious concern and sought dialogue, while the state maintains that interior use arrangements fall outside the inscription's reach. The regime cannot relinquish its mandate without unwinding the listing framework itself, and it holds no alternative jurisdiction over the building.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_world_heritage_regime, payer,
    institutional, generational, trapped, global).

% Citizens attached to the republic's founding settlement, who regard the 1934 museum decision as a deliberate act of the state's founder and the 2020 reversal as erasure of a founding legacy. They opposed the change through parties, petitions, and courts and were outvoted; the defeat is carried as an ideological and identitarian loss in the country they live in, with no relocation that would relieve it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secularist_turkish_citizens, payer,
    organized, generational, trapped, national).

% The Istanbul-based see of Orthodox Christianity, heir of the Byzantine rite the building housed for nine centuries. It stated publicly that the reconversion would sadden millions of Christians, and it holds no path to any custodial role; its objections received no procedural weight, and its seat is canonically fixed in Istanbul under the Turkish state's minority framework.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, ecumenical_patriarchate, payer,
    moderate, civilizational, trapped, global).

% Neighboring state and heir of much of the Byzantine inheritance, which protested the reconversion at head-of-state level, aligns its objection with the Patriarchate, and absorbs the friction in bilateral relations already strained over Aegean and minority questions. It can escalate or de-escalate diplomatically but cannot affect the building's status unilaterally.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, hellenic_state, payer,
    institutional, generational, mobile, national).

% International community of archaeologists, art historians, and conservators who study the building's fabric and mosaics. They document the conservation and access consequences of the change of use, advise on preservation, and hold no decision power; their stake is evidential and advisory.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, byzantine_heritage_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates congregational Islamic worship at the site under a single administrative authority: prayer scheduling, crowd management, conservation, and clerical staffing are coordinated centrally by the religious directorate instead of negotiated among claimants. The arrangement also settles, by exclusive allocation, who may use the building and when.
% TRANSFER_FUNCTION: Moves exclusive use and custody of the building from a plural-access museum regime to Islamic worship allocation administered by the state religious directorate; moves symbolic capital — sovereign legitimacy, confessional prestige, electoral mobilization — toward the governing coalition and religious constituency; moves costs (restricted visiting hours, concealed mosaics during services, denied international oversight, reversal of the secular settlement) onto non-worshipping visitors, the heritage regime, and secularist citizens.
% ABSENT_VOICES: Non-Muslim visitors had no consultative seat in the decree process; UNESCO was notified rather than consulted and its objections attach to no procedural hook; the Ecumenical Patriarchate and Turkey's non-Muslim minorities objected publicly through no formal channel; secularist opposition spoke in parliament but held no vote in the Council of State process. All of these seats sit outside the executive-judicial channel through which the arrangement was reinstated.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, congregational worship at the site would cease, the governing coalition would lose a signature legitimacy asset, diplomatic protests would subside, UNESCO oversight conversations would resume, and the secularist constituency would read the reversal as restoration. Access rules, political signaling, and international heritage governance all currently organize around the arrangement's existence.
% FOUNDING_PROBLEM: After the 1453 conquest the imperial cathedral was converted and endowed as a waqf to fund perpetual Islamic worship at the conquered capital's principal sanctuary; the founding problem was expressing and administering sovereign Islamic worship there. The modern arrangement answers a successor form of that problem: the 1934 museum conversion left the endowment's terms unfulfilled while the foundation legally persisted, and reinstating worship resolves that outstanding obligation.
% FOUNDING_PROBLEM_CORROBORATION: The endowment's documentary continuity is corroborated outside the beneficiary set by Ottoman archival records, the state foundation registry's holdings, the Council of State ruling's own documentary findings, and academic historiography of Ottoman endowments. However, the normative claim that the endowment obligates today's exclusive arrangement is attested only within the beneficiary coalition and sympathetic jurists; no disinterested party — UNESCO, the Patriarchate, foreign governments, the domestic opposition — attests that the founding problem remains live. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate-high (0.62) because the arrangement's burdens are real and concentrated on identifiable seats — restricted visitor access, extinguished multilateral oversight, an overturned domestic settlement — while the coordination core (daily worship) is genuine and freely performed. Suppression (0.55) is a raw structural property, unscaled by power or scope: the arrangement persists through state authority, a court reversal, police-managed access logistics, and the closure of rival claimants' procedural paths, but not through repression of speech — opponents argue, publish, and litigate and simply lose. Theater ratio (0.30) reflects a real worship function overlaid with substantial performative political usage (televised first prayers, ministerial appearances, campaign imagery); the 2023 hump tracks the republic's centenary and general election, an electoral-calendar oscillation rather than intermittent reinforcement, and it subsides in 2024 as routine worship re-dominate. Accessibility collapse (0.60): once the exclusive designation is understood, alternatives partially collapse — off-hours visits persist and entry is free, but the full museum-era experience exists nowhere, and the 2020 conversion of the Chora church shrank the substitute set. Resistance (0.60): sustained objection from Greece, the United States, UNESCO, the Patriarchate, and domestic opposition, real but electorally and juridically ineffective. The measurement series run on one shared five-point grid (2020–2024) with all three metrics authored at every point; suppression_requirement is authored because enforcement capacity visibly changed over the interval (assembly of access-control machinery, then normalization), not merely shifted extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the coalition's position the arrangement is a restored right it delivered against long odds — coordination it built and legitimately owns; from the trapped payer seats (UNESCO regime, secularist citizens, Patriarchate) the same structure operates as enforced exclusion of their claims from the only venue that matters. The mobile seats (visitors, ummah, Hellenic state) sit between: real burdens, substitutable engagement. The sibling readings are the framework-level version of this divergence — the universal_heritage and orthodox_restitution seats would author materially higher ε over the identical referent — and the engine computes per-seat classifications from the structural data authored here; the divergence is the measurement, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the coalition (rule-writing collector), the directorate (custodian gaining prestige and budget), the constituency (primary worship beneficiary), and the ummah (diffuse symbolic beneficiary) all derive d near the subsidized end. Victim declarations drive high directionality, modulated by exit: the UNESCO regime (trapped, institutional) and the Patriarchate (trapped, civilizational horizon) sit nearest the full-target end; secularist citizens (trapped by citizenship and identity) likewise; non-Muslim visitors are declared victims but their mobile exit damps effective extraction toward the middle — the harm is opportunity loss, not entrapment; the Hellenic state bears real friction with diplomatic mobility. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the intended per-seat relationships, and the dual-positioned directorate (beneficiary running operations) is handled through its secondary_role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   By this reading's own doctrine the founding problem is perpetual — a waqf binds forever, so the mandate cannot atrophy while the endowment stands; founding_problem_status is live and the disappearance verdict is world_rearranges, so the mismatch consumer finds no dead-mandate-plus-dependence flag. The classification guards against mislabeling in both directions: calling the arrangement a snare would erase the genuine, daily-performed worship coordination that anchors it; calling it a rope would erase the enforced asymmetry — excluded visitors, a denied oversight regime, a defeated domestic constituency — that requires active state enforcement to hold. The live risk vector is drift, not atrophy: if political and ceremonial usage compounds while worship routine stabilizes, theater_ratio climbs toward proxy-goal territory; the 2020–2024 series (peak 0.33 in the centenary/election year, easing to 0.30) is the baseline against which that drift would register.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (islamic_sovereignty_reading) of the hagia_sophia_substrate kernel; would the sibling readings (universal_heritage_reading, orthodox_restitution_reading), assessing the same post-2020 arrangement, author materially different epsilon and victim sets?',
    'Compile the sibling stories and compare per-seat classifications and epsilon over the identical referent; the cross-reading spread measures the intensity of the kernel contest.',
    'A large spread confirms the kernel is genuinely contested rather than settled; convergence would indicate one reading has de facto displaced the others and this file''s reading-indexed values should be re-baselined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed epsilon over a shared kernel referent, with sibling readings as separate constraints.').

omega_variable(
    waqf_continuity_empirical,
    'Is the legal continuity of the Mehmed II endowment (the 1453 deed through the present foundation) robust enough to ground the reading''s title claim?',
    'Ottoman archival scholarship, state foundation registry records, and the documentary findings recited in the Council of State''s annulment ruling.',
    'Robust continuity anchors the reading''s conventional axioms; a demonstrated break would reduce the title claim to political assertion and raise effective extraction on every excluded seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_continuity_empirical, empirical, 'Documentary continuity of the endowment underwriting the sovereignty claim.').

omega_variable(
    symbolic_material_extraction_mix,
    'How much of the burden borne by payer seats is material (access restrictions, denied oversight) versus symbolic (ideological defeat, diplomatic friction)?',
    'Seat-by-seat harm decomposition: visitor-flow and access-log analysis for material costs; survey and diplomatic-record analysis for symbolic costs.',
    'If burdens are predominantly symbolic they concentrate on identity-bound seats, and the arrangement''s identity-coordination framing warrants heightened cover-story scrutiny; if material, standard extraction remedies apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_material_extraction_mix, conceptual, 'Material versus symbolic composition of the payer-seat burdens.').

omega_variable(
    judiciary_independence_question,
    'Did the Council of State''s 2020 annulment of the 1934 decision reflect independent juridical evaluation of the endowment deed, or alignment with the executive''s publicly announced objective?',
    'Comparative analysis of the chamber''s reasoning against its prior case law, any dissenting opinions, and the sequencing between executive statements and the ruling.',
    'Independent validation supports the lineage authority grounding declared in cs_structure; demonstrated capture would shift the authority structure toward extraction and raise suppression assessments across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_independence_question, empirical, 'Validity versus capture in the court reversal that validates the arrangement''s authority.').

omega_variable(
    enforcement_normalization_trajectory,
    'Will site enforcement harden (expanded prayer-time closures, further mosaic concealment, tighter gallery control) or stabilize into routine worship administration?',
    'Track directorate access regulations, closure-hour expansions, and conservation protocols from 2025 onward.',
    'Continued hardening would push the arrangement toward snare-flavored enforcement dynamics and date a type transition; stabilization supports the tangled_rope reading and the current metric plateau.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_normalization_trajectory, empirical, 'Future trajectory of enforcement intensity at the site.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement_basis(hagi_tr_t2020, observed).
narrative_ontology:measurement(hagi_tr_t2021, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2021, 0.26).
narrative_ontology:measurement_basis(hagi_tr_t2021, observed).
narrative_ontology:measurement(hagi_tr_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2022, 0.29).
narrative_ontology:measurement_basis(hagi_tr_t2022, observed).
narrative_ontology:measurement(hagi_tr_t2023, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2023, 0.33).
narrative_ontology:measurement_basis(hagi_tr_t2023, observed).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 2024, 0.3).
narrative_ontology:measurement_basis(hagi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement_basis(hagi_be_t2020, observed).
narrative_ontology:measurement(hagi_be_t2021, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement_basis(hagi_be_t2021, observed).
narrative_ontology:measurement(hagi_be_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement_basis(hagi_be_t2022, observed).
narrative_ontology:measurement(hagi_be_t2023, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement_basis(hagi_be_t2023, observed).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(hagi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement_basis(hagi_su_t2020, observed).
narrative_ontology:measurement(hagi_su_t2021, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2021, 0.52).
narrative_ontology:measurement_basis(hagi_su_t2021, observed).
narrative_ontology:measurement(hagi_su_t2022, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2022, 0.54).
narrative_ontology:measurement_basis(hagi_su_t2022, observed).
narrative_ontology:measurement(hagi_su_t2023, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2023, 0.56).
narrative_ontology:measurement_basis(hagi_su_t2023, observed).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(hagi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the hagia_sophia_substrate kernel decomposes into three reading-constraints sharing one referent (the post-2020 standing arrangement) with reading-indexed epsilon. This file instantiates islamic_sovereignty_reading (epsilon 0.62 by its own lights; victims: restricted visitors, denied UNESCO oversight, ideologically defeated secularist citizens). The universal_heritage_reading would author higher epsilon over the same referent (denial of shared-heritage access and multilateral oversight), and the orthodox_restitution_reading higher still (a consecrated church whose restitution path is closed). Same referent, different readings, different epsilon per OQ-26; the stories are linked per the family rule rather than merged, because forcing one story to span the readings would break epsilon-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
