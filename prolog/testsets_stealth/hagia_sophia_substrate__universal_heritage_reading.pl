% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Heritage Museum (Universal-Heritage Reading)
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   Hagia Sophia's legitimacy basis is a contested kernel: one building,
 *   three live readings, each instantiating a different constraint. This
 *   story instantiates the universal_heritage_reading — the arrangement
 *   codified by the 1934 Council of Ministers decree and later framed by the
 *   UNESCO World Heritage regime, under which the site is a museum whose
 *   legitimacy derives from shared human cultural heritage transcending any
 *   single religious or national claim. The standing arrangement this story
 *   is about is the technocratic museum regime of 1934-2020: open to visitors
 *   and scholars, closed to congregational worship, monetized through
 *   admission, and deployed as an ideological signal of secular Turkish
 *   modernity. The reading's own transcendence standard is what makes its
 *   epsilon high by its own lights: an arrangement claiming to transcend
 *   particular claims uniquely barred one claim-set (worship) while expanding
 *   every use that competed with the state's preferred ones. This is one
 *   member of a three-story constraint family; the sibling files carry their
 *   own epsilon, beneficiary/victim structure, and classification, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - technocratic_museum_administration: agenda-setter (institutional/constrained) — administers the site, enforces the worship prohibition, remits revenue to the state
 *   - secularist_turkish_elites: dual-positioned beneficiary and founding agenda-setter (institutional/identity_locked) — captured the site's symbolic capital; fused with the settlement they authored
 *   - global_tourism_sector: primary commercial beneficiary (organized/mobile) — collects access to a premier attraction; exit is mobile
 *   - heritage_scholarship_community: beneficiary (organized/mobile) — decades of research access under museum rules
 *   - international_heritage_regime: beneficiary and monitor (institutional/mobile) — the arrangement validated its universal-value doctrine
 *   - muslim_worship_claimants: primary target (organized/trapped) — bore the worship prohibition; the claim cannot be relocated
 *   - ottoman_waqf_successors: secondary target (moderate/trapped) — the endowment's designated purpose was nullified without compensation
 *   - orthodox_ecclesiastical_claimants: excluded voice (organized/trapped) — restitution claim never had a forum
 *   - secular_constitutional_judiciary: analytical observer (institutional/analytical) — adjudicated the arrangement's legality, culminating in the 2020 annulment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.72).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Heritage Museum (Universal-Heritage Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '7188644b-1ab8-4eb8-80d3-331670e858ed').
narrative_ontology:cs_kernel_codification('7188644b-1ab8-4eb8-80d3-331670e858ed', formalized).
narrative_ontology:cs_authority_grounding('7188644b-1ab8-4eb8-80d3-331670e858ed', lineage).
narrative_ontology:cs_interpretation_layer_present('7188644b-1ab8-4eb8-80d3-331670e858ed').
narrative_ontology:cs_reading_relation('7188644b-1ab8-4eb8-80d3-331670e858ed', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('7188644b-1ab8-4eb8-80d3-331670e858ed', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('7188644b-1ab8-4eb8-80d3-331670e858ed', foundational, legitimacy_transcends_single_tradition_claims).
narrative_ontology:cs_axiom_status(legitimacy_transcends_single_tradition_claims, holdable).
narrative_ontology:cs_axiom_grounding('7188644b-1ab8-4eb8-80d3-331670e858ed', legitimacy_transcends_single_tradition_claims, deontological).
narrative_ontology:cs_axiom('7188644b-1ab8-4eb8-80d3-331670e858ed', secondary, heritage_access_supersedes_exclusive_worship).
narrative_ontology:cs_axiom_status(heritage_access_supersedes_exclusive_worship, holdable).
narrative_ontology:cs_axiom_grounding('7188644b-1ab8-4eb8-80d3-331670e858ed', heritage_access_supersedes_exclusive_worship, conventional).
narrative_ontology:cs_reference_frame('7188644b-1ab8-4eb8-80d3-331670e858ed', universal_heritage_museum_settlement).
narrative_ontology:cs_drift_state('7188644b-1ab8-4eb8-80d3-331670e858ed', pre_reconversion_contestation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7188644b-1ab8-4eb8-80d3-331670e858ed', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_community).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, muslim_worship_claimants).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, ottoman_waqf_successors).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, kemalist_secularist_settlement).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, unesco_outstanding_universal_value_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The directorate under the Ministry of Culture that ran the site day to day: conservation programs, ticketing, visitor flow, security. It enforced the prohibition on worship inside the building, collected admission revenue for the state treasury, and absorbed international criticism on the state's behalf. Its staff are career professionals whose positions exist only within the arrangement.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, technocratic_museum_administration, agenda_setter,
    institutional, biographical, constrained, national).

% Tour operators, hoteliers, guides, and cruise lines that sell Istanbul itineraries built around the site. Admission-based access at scale made the building one of the world's most visited monuments, and the sector's revenue depends on the site remaining open to mass visitation. If the arrangement changed, the sector would reroute itineraries: costly but feasible.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    organized, biographical, mobile, global).

% The republican state class — military, judiciary, cultural ministries, urban intelligentsia — that authored the 1934 conversion and treated the museum status as proof of a secular, Western-facing republic. The site's symbolic capital flowed to this class for generations; conceding the museum settlement would mean conceding the founding narrative of their own political identity.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter).

% Byzantine and Ottoman historians, art historians, archaeologists, and conservation scientists, domestic and international, who worked on and in the building under museum access rules. They gained research access and publication opportunities unavailable under exclusive religious use, and their professional output depends on continued scholarly access.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_community, beneficiary,
    organized, generational, mobile, global).

% UNESCO and the treaty-based World Heritage system, which inscribed the Historic Areas of Istanbul and treated the museum arrangement as a model of universal-value stewardship. The arrangement's existence validated the regime's governing doctrine; its 2020 end drew formal regret and monitoring.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_heritage_regime, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, international_heritage_regime, observer).

% Turkish Muslims and Islamic civil-society associations asserting the right to congregational prayer in the building that served as Istanbul's first congregational mosque for nearly five centuries. Under the arrangement they could enter as visitors but not worship; prayer inside was barred and policed. Their claim attaches to this building specifically and cannot be satisfied at any other site; they organized associations, annual demonstrations, and legal petitions across decades.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, muslim_worship_claimants, payer,
    organized, generational, trapped, national).

% The legal and customary successors to the Ottoman endowment that maintained the building as a mosque. The 1934 conversion nullified the endowment's designated purpose without consultation or compensation; their claim, like the worshippers', attaches to this building alone.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, ottoman_waqf_successors, payer,
    moderate, generational, trapped, national).

% The Ecumenical Patriarchate and Greek Orthodox institutions asserting continuity with the building's cathedral era. They were never given a seat in the museum governance; their claim to restitution or neutral commemoration had no forum inside the arrangement, and their position within Turkey is legally constrained.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_ecclesiastical_claimants, excluded,
    organized, generational, trapped, continental).

% The Council of Ministers that issued the 1934 decree and the Council of State that held jurisdiction over its legality, culminating in the 2020 annulment. It adjudicated what the arrangement permitted, treated the founding decree as the controlling text, and ultimately dissolved the arrangement's legal basis.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secular_constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem: a 1,500-year-old structure at the center of competing religious and national claims requires a single administrative authority to fund conservation, manage visitor flow, and prevent exclusive appropriation by any one claimant. The museum arrangement provided centralized conservation (successive dome and mosaic restorations), open access on equal terms to all visitors, and a funding basis independent of any religious body.
% TRANSFER_FUNCTION: Moves three things: admission revenue from millions of visitors to the state treasury and the surrounding tourism economy; research access and publication value to the scholarship community; and the site's symbolic capital to the secularist state class as an ideological signal of secular modernity. The offsetting cost — exclusion of congregational worship — was borne entirely by Muslim worship claimants and the waqf's successors, who received nothing in exchange.
% ABSENT_VOICES: Muslim worship claimants had no seat in the governance that adjudicated their exclusion — the arrangement was created by decree without consulting them, and decades of petitions were answered by the same state apparatus that ran the museum. Orthodox ecclesiastical claimants likewise had no forum: their claim was never entertained. Part of the arrangement's international unanimity of endorsement reflects that the seats most affected by the worship prohibition were never in the room.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight the site would not revert to a neutral default: the competing claimants would immediately contest possession — the 2020 reconversion shows the claimant apparatus was intact and mobilized — tourism access and conservation funding would need a new basis, and the international heritage regime would lose its flagship case. The world rearranges around whichever claimant captures the building.
% FOUNDING_PROBLEM: The contested status of a monument held sacred by successive civilizations in a newly secular nation-state: after the republic's founding, the question of who may possess and use Hagia Sophia — cathedral heirs, mosque claimants, the state — threatened both inter-communal stability and the new state's secular identity. The 1934 decree resolved it by removing the building from religious contestation entirely and re-founding its legitimacy as universal heritage under state administration.
% FOUNDING_PROBLEM_CORROBORATION: The beneficiary set (museum administration, heritage regime, secularist elites) attested for decades that the problem was permanently solved. Outside that set: Muslim worship claimants attested by continuous mobilization — associations from the 1990s, annual conquest-anniversary demonstrations, and the 2005 lawsuit — that the contest was live; the Turkish Council of State's 2020 annulment ruling formally attested the same; and the scale of popular support for the 2020 reconversion confirmed the founding problem had never gone dormant. Corroboration from outside the benefiting parties is abundant.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness 0.72 (the series end-state): the arrangement monetized access at growing scale while uniquely barring worship — the bar was not applied symmetrically to all uses, only to the claim-set competing with the state's preferred use. Suppression 0.68 is a raw structural property authored unscaled (only extractiveness is scaled by directionality and scope in the engine): a standing legal prohibition, police removal of worshippers, full state backing. Theater 0.35: conservation and open access were real and continuous, but the performative dimension — the museum as ideological showcase of secular modernity, defended ritually as domestic consensus eroded — grew across the interval. Accessibility collapse 0.30: the rival readings never collapsed; the Islamic sovereignty reading stayed legally and politically live for the entire interval and captured the state in 2020. Resistance 0.60: decades of organized contestation — associations, annual May 29 demonstrations, litigation — met the arrangement. All three tracked metrics run on one shared eight-point grid (1934-2020) so no metric's series borrows another's end-state at earlier times. The trajectories are monotonic trends with periodic flare-ups (conquest-anniversary demonstrations, petition cycles) that I treat as secondary oscillation around the trend rather than a self-sustaining cycle: the trend, not the oscillation, is the operative mechanism, and the base_properties values were measured at the interval end, the trend's peak. Claim/metric independence: claimed_type tangled_rope is my independent structural judgment — the same arrangement that genuinely coordinated conservation and equal-terms access also carried asymmetric, actively enforced costs; I did not reconcile the claim to the metrics or to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different types from the same structure. From the agenda-setter and elite seats, the arrangement is the settlement that took the building out of inter-communal contestation, funded its survival, and opened it to everyone on equal terms — by their lights the only arrangement under which no claimant loses the building entirely. From the worship-claimant seat, the same structure is eighty-six years of enforced exclusion from their own former congregational mosque while others monetized it — a settlement whose costs were allocated entirely to the claim-set that had no seat at the founding. The excluded orthodox seat sees a third structure again: an arrangement that neutralized their claim without ever acknowledging it. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: the arrangement directs access, revenue, and symbolic capital toward them. Mobile exit (tourism, scholarship, the heritage regime) damps their effective burden further — they could reroute if the arrangement changed. The elite seat is identity-locked rather than mobile: its position is fused with the Kemalist settlement, so it cannot arbitrage away, but it also cannot concede the arrangement without dissolving its own founding narrative. Victims sit near the full-target end: the worship claim is site-specific and cannot be relocated (trapped), so the prohibition lands at full force on the claimants and the waqf successors. No directionality overrides are authored: the override mechanism is keyed to power atoms, and this story's institutional seats (secular elites, heritage regime, museum administration, constitutional judiciary) hold genuinely different structural relationships that the beneficiary/victim declarations and exit options already differentiate; a single institutional-atom override would misapply one d-value across all four. The museum administration's seat — agenda-setter, neither declared beneficiary nor victim — is left to the canonical fallback; it executed the arrangement and remitted its proceeds to the state rather than capturing them as its own.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetric mislabels. Reading the arrangement as pure coordination (the beneficiary framing: the museum saved the building for everyone) would erase the suppressed victim set and the monetized asymmetry; reading it as pure capture (the rival readings' framing: the museum was confiscation) would erase the real conservation record — successive dome and mosaic restorations — and the equal-terms access no exclusive-worship arrangement provided. The R5 interview shows no mandatrophy: the founding problem (possession of a multi-claimant sacred site in a secular state) is still live — the 2020 reconversion re-litigated it rather than proving it obsolete. The arrangement did not outlive its function; it lost a contest over whose function the building serves. The receipt facts are consistent with capture: gains accrued to the elite-state seat, and fixing was prohibitive for the arrangement's own fixers across the entire interval — the fix became cheap only when the rival coalition captured the fixer seat in 2020, which is a change of hands, not a change in the structure's cost profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the hagia_sophia_substrate kernel — what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Author the sibling stories (islamic_sovereignty_reading, orthodox_restitution_reading) and compare beneficiary/victim sets, authority seats, and epsilon. The disagreement is located in the derivation of legitimacy itself — conquest-and-endowment vs founding-as-cathedral vs transcendence-of-particular-claims — which reassigns the victim and beneficiary sets wholesale: under the islamic reading the worship claimants become the collecting seats and mass tourism access becomes the constrained party; under the orthodox reading ecclesiastical control replaces state administration.',
    'Every directional assignment, the payer and beneficiary seats, and the classification are reading-relative; a sibling instantiation would flip who bears the arrangement''s costs and who coordinates it. This file''s numbers are valid only for the universal-heritage reading and must never be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: the classification is relative to one reading of a three-reading kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of Islamic worship claims under the museum arrangement structural (state prohibition and policing) or internalized (genuine dormancy of the claim among the population)?',
    'Compare post-2020 worship intensity and mobilization scale against the 1934-2020 record: the scale of congregational response after reconversion indicates the claim persisted beneath the enforcement rather than having dissolved.',
    'If the suppression was structural, the arrangement''s stability rested entirely on state enforcement capacity and the authored suppression is correctly high; if partly internalized, part of the measured suppression was consent and the arrangement was less coercive than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized mechanism of the worship prohibition.').

omega_variable(
    transcendence_framing_asymmetry,
    'Does the universal-heritage framing actually transcend particular claims symmetrically, or does it operate as a secular-national framing that privileges heritage consumption and secular identity over worship claims?',
    'Compare the arrangement''s treatment of competing use-claims: if all religious claims were equally subordinated while non-religious uses (tourism, state ceremony) proceeded freely, the framing is symmetric. The record shows worship claims uniquely barred while revenue uses expanded — an asymmetry.',
    'If asymmetric, the transcendence premise functions as cover and the arrangement is more extractive than a genuinely symmetric heritage regime would be; if symmetric, part of the authored epsilon reflects the inherent cost of any exclusive-use regime rather than capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transcendence_framing_asymmetry, conceptual, 'Whether the transcendence premise is symmetric or cover for asymmetric operation.').

omega_variable(
    conservation_worship_separability,
    'Is the conservation function of the museum arrangement separable from the worship prohibition — could the building be conserved and open while hosting congregational prayer?',
    'The post-2020 arrangement conserves the building while hosting worship, and comparable co-managed heritage mosques show conservation and worship coexisting; if conservation outcomes hold under worship, the functions are separable and the prohibition was never a conservation necessity.',
    'If separable, the prohibition component of epsilon is pure capture riding on a real coordination function; if inseparable, part of epsilon is the price of the conservation itself and the coordination reading strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conservation_worship_separability, empirical, 'Separability of the conservation function from the worship prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1934, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_universal_heritage_tr_t1934, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1934, 0.12).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t1934, observed).
narrative_ontology:measurement(hagia_universal_heritage_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t1950, observed).
narrative_ontology:measurement(hagia_universal_heritage_tr_t1965, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t1965, observed).
narrative_ontology:measurement(hagia_universal_heritage_tr_t1980, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t1980, observed).
narrative_ontology:measurement(hagia_universal_heritage_tr_t1990, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1990, 0.26).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t1990, observed).
narrative_ontology:measurement(hagia_universal_heritage_tr_t2000, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t2000, observed).
narrative_ontology:measurement(hagia_universal_heritage_tr_t2010, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2010, 0.33).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t2010, observed).
narrative_ontology:measurement(hagia_universal_heritage_tr_t2020, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2020, 0.35).
narrative_ontology:measurement_basis(hagia_universal_heritage_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(hagia_universal_heritage_be_t1934, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1934, 0.45).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t1934, observed).
narrative_ontology:measurement(hagia_universal_heritage_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t1950, observed).
narrative_ontology:measurement(hagia_universal_heritage_be_t1965, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1965, 0.54).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t1965, observed).
narrative_ontology:measurement(hagia_universal_heritage_be_t1980, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t1980, observed).
narrative_ontology:measurement(hagia_universal_heritage_be_t1990, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t1990, observed).
narrative_ontology:measurement(hagia_universal_heritage_be_t2000, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t2000, observed).
narrative_ontology:measurement(hagia_universal_heritage_be_t2010, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t2010, observed).
narrative_ontology:measurement(hagia_universal_heritage_be_t2020, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement_basis(hagia_universal_heritage_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(hagia_universal_heritage_su_t1934, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1934, 0.4).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t1934, observed).
narrative_ontology:measurement(hagia_universal_heritage_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.44).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t1950, observed).
narrative_ontology:measurement(hagia_universal_heritage_su_t1965, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t1965, observed).
narrative_ontology:measurement(hagia_universal_heritage_su_t1980, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t1980, observed).
narrative_ontology:measurement(hagia_universal_heritage_su_t1990, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t1990, observed).
narrative_ontology:measurement(hagia_universal_heritage_su_t2000, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t2000, observed).
narrative_ontology:measurement(hagia_universal_heritage_su_t2010, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t2010, observed).
narrative_ontology:measurement(hagia_universal_heritage_su_t2020, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(hagia_universal_heritage_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% One substrate (the Hagia Sophia building and its accumulated legitimacy claims), three readings, three constraints: islamic_sovereignty_reading, orthodox_restitution_reading, and this file (universal_heritage_reading). Each carries its own epsilon, beneficiary/victim structure, and classification; no story averages across readings. The universal-heritage reading is upstream in legitimacy discourse — the UNESCO universal-value framing is cited against both exclusive claims — so its network edges run to both siblings, while its own epsilon is assessed on the museum arrangement by its own transcendence standard, never on the arrangements its rivals would install.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
