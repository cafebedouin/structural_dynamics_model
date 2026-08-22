% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Press-Reformation Mutual-Shaping Coupling
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the mutual_shaping reading of the
 *   press_reformation_causation kernel: between Gutenberg's bible (c. 1455)
 *   and the settled confessional print regimes of 1600, printing technology
 *   and religious agency co-evolved — the press opened possibility space
 *   (cheap vernacular pamphlets, rapid reprinting, evasion of territorial
 *   censorship) that reformers exploited, and the explosion of reform demand
 *   in turn redirected the print industry itself: product mix shifted to
 *   short vernacular quartos and broadsheets, edition cycles shortened, new
 *   press centers like Wittenberg, Strasbourg, and Geneva grew around reform
 *   catalogs, and printers learned to move type and stock ahead of hostile
 *   authorities. Neither pole was fixed: the technology was not a neutral
 *   tool awaiting use, and the outcome was not technologically inevitable.
 *   The arrangement was transitional — an enabling window whose very success
 *   summoned the counter-mobilization (imperial bans, the Stationers' Company
 *   charter of 1557, the Roman Index of 1559, territorial licensing) that
 *   closed it. Family note (epsilon-invariance): the sibling readings are
 *   separate stories with separate epsilon values, linked by
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   commercial_printers: agenda-setting beneficiary (organized/mobile) —
 *   administers what is published, collects the boom rents, exits hostile
 *   cities by relocating - evangelical_pamphleteers: beneficiary-payer
 *   (moderate/identity_locked) — exploits the channel at the price of ban,
 *   exile, and martyrdom risk - vernacular_lay_readers: beneficiary
 *   (moderate/mobile) — gains unmediated access to scripture and polemic -
 *   scribal_manuscript_workers: payer (powerless/constrained) — the
 *   manuscript economy their craft served is liquidated beneath them -
 *   latin_orthodox_establishment: payer and counter-administrator
 *   (institutional/identity_locked) — loses gatekeeping control, answers with
 *   Index and licensing - territorial_rulers_and_city_councils:
 *   agenda-setting beneficiary (institutional/constrained) — set the local
 *   rules the coupling ran through - rural_illiterate_majority: excluded
 *   (powerless/trapped) — governed by the outcome, absent from the medium -
 *   scholarly_analysts: analytical observer — reconstructs the record, holds
 *   no sixteenth-century stake
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.3).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.22).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.3).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Press-Reformation Mutual-Shaping Coupling").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'e6c0dca5-7138-41ce-b884-db1ef4159984').
narrative_ontology:cs_kernel_codification('e6c0dca5-7138-41ce-b884-db1ef4159984', distributed).
narrative_ontology:cs_authority_grounding('e6c0dca5-7138-41ce-b884-db1ef4159984', expertise).
narrative_ontology:cs_interpretation_layer_present('e6c0dca5-7138-41ce-b884-db1ef4159984').
narrative_ontology:cs_reading_relation('e6c0dca5-7138-41ce-b884-db1ef4159984', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('e6c0dca5-7138-41ce-b884-db1ef4159984', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_axiom('e6c0dca5-7138-41ce-b884-db1ef4159984', foundational, causation_is_bidirectional_constitutive).
narrative_ontology:cs_axiom_status(causation_is_bidirectional_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('e6c0dca5-7138-41ce-b884-db1ef4159984', causation_is_bidirectional_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('e6c0dca5-7138-41ce-b884-db1ef4159984', secondary, enabling_windows_are_self_limiting).
narrative_ontology:cs_axiom_status(enabling_windows_are_self_limiting, holdable).
narrative_ontology:cs_axiom_grounding('e6c0dca5-7138-41ce-b884-db1ef4159984', enabling_windows_are_self_limiting, empirically_contingent).
narrative_ontology:cs_reference_frame('e6c0dca5-7138-41ce-b884-db1ef4159984', reciprocal_media_agency_feedback).
narrative_ontology:cs_drift_state('e6c0dca5-7138-41ce-b884-db1ef4159984', contemporary_commercial_turn_historiography, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e6c0dca5-7138-41ce-b884-db1ef4159984', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, commercial_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, evangelical_pamphleteers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_lay_readers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, scribal_manuscript_workers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, latin_orthodox_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, territorial_rulers_and_city_councils).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, evangelical_pamphleteers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Master printers decide which manuscripts become books, finance editions, and set prices; they gate what reaches the public. The reform controversy handed them the fastest-selling product line the trade had ever seen, and boom profits concentrated in shops with reform catalogs. When a city's authorities turned hostile, a printer crated his type and press and reopened elsewhere — Basel, Strasbourg, Wittenberg, Geneva, and Emden all grew by receiving such migrants. Their vulnerability was capital: unsold stock and confiscated equipment ruined shops that backed losing confessional sides.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, commercial_printers, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, commercial_printers, beneficiary).

% Preachers, ex-monks, and publicists wrote the short German and Latin tracts that filled the presses after 1517. The new channel let one author reach tens of thousands of readers in weeks — an audience no pulpit or university disputation could assemble. The same visibility made them targets: imperial bans, excommunication, exile, and occasionally execution followed publication. Most could not stop writing without abandoning the vocation that defined them; Luther himself survived under princely protection that few others enjoyed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, evangelical_pamphleteers, beneficiary,
    moderate, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, evangelical_pamphleteers, payer).

% Urban artisans, merchants, and clergy-in-training bought or borrowed inexpensive quartos and broadsheets, gaining direct access to scripture translations and polemic previously filtered through clerical intermediaries. Reading choices stayed mostly free during the early decades; as territories confessionalized, owning the wrong books became legally dangerous, and household libraries turned into evidence.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_lay_readers, beneficiary,
    moderate, biographical, mobile, continental).

% Scriptoria, professional copyists, scriveners, and illuminators had supplied the entire manuscript book trade; within two generations the commercial bottom fell out of their market. Some found places in the new trade as compositors, proofreaders, or stationers' employees; the rest watched a craft economy their families had served for centuries shrink to administrative copying and luxury commissions. No one consulted them about print privileges.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, scribal_manuscript_workers, payer,
    powerless, generational, constrained, continental).

% The papacy, bishops, university theology faculties, and monastic orders had controlled authoritative text production and licensed preaching. Cheap vernacular print stripped the gate: unauthorized Bibles, sermons, and invective circulated faster than any response could be mounted. The institutions answered with the tools they had — book burnings, the Index of Prohibited Books, licensing and prior censorship where their jurisdiction ran — and where they governed territory directly they administered those controls themselves. Abandoning their doctrinal claims was unthinkable; the institution's authority was those claims.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, latin_orthodox_establishment, payer,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, latin_orthodox_establishment, agenda_setter).

% Princes and magistrates granted printing privileges, admitted or expelled pressmen, and decided whether reform teaching could be sold in their markets. Toleration attracted printers, paper mills, and the prestige of hosting the controversy; protection of favored preachers extended princely power over church affairs. No territory could opt out of the communication revolution altogether — choosing a posture was unavoidable, and the Peace of Augsburg later froze those postures into law.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, territorial_rulers_and_city_councils, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, territorial_rulers_and_city_councils, beneficiary).

% Most of Europe's population could not read and lived outside the towns where presses and pamphlet markets operated. The confessional struggle reached them through preached decrees, visitations, and eventually compulsory conformity, none of which they chose. They had no voice in what was printed, licensed, or banned, though the settlement of the controversy would govern their worship for centuries.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, rural_illiterate_majority, excluded,
    powerless, generational, trapped, continental).

% Book historians, archivists, and historical sociologists reconstruct print runs, prices, and circulation from colophons, inventories, and surviving editions, and weigh how far the medium's development tracked religious demand. They take no side in the confessional questions and hold no stake in the sixteenth-century arrangements; their disagreements concern causal weighting.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, scholarly_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, commercial_printers).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Connected a new low-cost reproduction technology to dispersed, suddenly-activated demand for religious content: standardized short formats (flugschriften) let arguments travel between cities faster than authorities could reply, and printer networks coordinated supply to demand spikes no manuscript economy could have met.
% TRANSFER_FUNCTION: Moved communicative capacity from clerical gatekeepers to lay publics and publicist-authors; moved money from readers and patrons to printer-publishers; moved status from university faculties and episcopal pulpits to the writers and shops who could feed the presses.
% ABSENT_VOICES: The rural illiterate majority, most women, and the scribal workforce being displaced had no seat in decisions about privileges, licenses, or what was printed; parish clergy caught between bishop and press spoke only through their superiors. They appear nowhere in the correspondence of the printers, princes, and theologians who ran the arrangement — their interests surface only in visitation records and complaint literature after the fact.
% DISAPPEARANCE_RATIONALE: Remove the coupling overnight — say, print never meets reform — and both sides rearrange: the Reformation becomes a slower, regional university-and-pulpit movement that authorities can plausibly contain, while the print industry develops around legal, classical, and devotional steady-state demand instead of controversy booms; no Wittenberg press center, no flugschriften genre, and a very different confessional map.
% FOUNDING_PROBLEM: No one designed the coupling; it emerged. Retroactively described, the problem it formed around was the reproduction bottleneck: the manuscript economy could not scale text production to meet expanding literacy and the demand detonated by the religious crisis, and print's falling marginal costs met that demand.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: quantitative book-history reconstruction (the Febvre-Martin tradition; Pettegree's print-run and edition studies) attests the reproduction bottleneck and its solution from archival colophons, inventories, and price data; the displaced party's own complaint literature (scribal and stationer petitions) attests the bottleneck's closing from the losing side. No living party depends on the founding problem remaining unsolved.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is scaffold, authored independently of the metrics: the coupling's function was to carry a transition — Europe's communication order from manuscript scarcity to print abundance, and its religious order from unified Latin gatekeeping to plural vernacular confession — and the arrangement was self-terminating, because the success that defined it summoned the enforcement that closed the window. The sunset clause is emergent rather than written: no founder declared it, but the configuration could not survive its own victory, and by 1600 the coupling survives only inside machinery erected to govern it. Metrics are authored as descriptive truths: extractiveness 0.30 — real but bounded rents (scarcity pricing on controversy titles, privilege monopolies) within a predominantly enabling arrangement; suppression 0.22 — the coupling itself coerced almost no one (manuscript culture declined by competition, not prohibition; readers chose freely in the open decades), which is deliberately NOT the same quantity as the suppression_requirement series, which tracks the external enforcement machinery that rose to close the window (0.05 to 0.74 across the interval); theater_ratio 0.12 — the activity was overwhelmingly functional, with mild ritualization (privilege formulas, imprimatur formalities) appearing only as the window shut; accessibility_collapse 0.42 — manuscript and oral alternatives receded economically but never vanished; resistance 0.48 — sustained counter-mobilization that redirected rather than stopped the coupling. All three series share one seven-point grid (1450-1600) so no metric is sampled against another's gaps.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently, and the engine computes that divergence from the structural data. From the printer's seat the arrangement is an administered opportunity: they set the publication agenda, collected the rents, and could always crate the press and leave — a coordination they ran. From the pamphleteer's seat it is an enabling channel worth dying for: enormous benefit, heavy cost, no exit from the vocation. From the establishment's seat the same structure is dispossession — gatekeeping authority stripped by a machine it could not outlaw fast enough, with identity-lock making accommodation unthinkable. From the scribal worker's seat it is liquidation without consultation. From the reader's seat, emancipation. One arrangement, five experiences; the per-seat computation is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for commercial_printers (administers and collects, with relocation-grade exit), evangelical_pamphleteers (net beneficiaries despite heavy costs — they fought for the arrangement's existence), and vernacular_lay_readers (gains, mobile exit). Victim declarations drive high directionality for scribal_manuscript_workers (constrained exit — skills partially convertible, livelihoods not) and latin_orthodox_establishment (identity_locked — the institution could not abandon its claims without dissolving, placing it near the full-target end despite institutional power). Territorial rulers derive low-to-mid directionality: they administered the local terms and collected alignment benefits. No directionality overrides were needed: the beneficiary/victim data plus exit options already place each seat correctly, and the override mechanism keys on power atoms, which would smear corrections across differently-positioned institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reproducing authoritative text at scale beyond scribal limits — is dead, solved so thoroughly that its solution is invisible. The mismatch consumer will read founding_problem_status=dead against disappearance_verdict=world_rearranges and raise a capture/zombie flag; the flag should find no corroboration in the computed path, because this is the opposite of a piton: the theater ratio stayed low, and the arrangement did not outlive its function as performance — it completed its transition and was dismantled into the successor licensing regime, which is what a scaffold that works looks like. The classification prevents two mislabelings: reading the coupling as pure rope (tool coordination) erases the asymmetric costs — a scribal workforce liquidated and a clerical gatekeeping rent destroyed through the same structure that subsidized printers and readers; reading it as snare erases the genuine enabling function and the fact that most participants were net beneficiaries who would have fought to keep it. Scaffold holds both halves and encodes the termination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'The same archival record is claimed by three readings of the press_reformation_causation kernel — that the press unilaterally determined the Reformation, that agents neutrally deployed a fixed tool, or that medium and agency co-constituted each other. Which causal structure does the record actually support?',
    'Comparative print-history analysis: compare industry development (format innovation, market geography, product mix) in regions with high versus low reform demand; systematic divergence of print trajectories tracking reform exposure supports co-evolution, while uniform trajectories independent of demand support the determinism or neutrality readings.',
    'Resolving toward technological_determinism would reclassify this arrangement toward an inevitable-force profile with negligible extraction; resolving toward strategic_deployment would strip the feedback half and read the arrangement as pure tool-use coordination. This story''s scaffold classification stands only under the mutual-shaping resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the press-Reformation causation kernel the historical record supports.').

omega_variable(
    sunset_emergence_vs_design,
    'Was the permissive window''s closure a structural property of the arrangement (any successful mass-medium challenge to authorities triggers counter-mobilization, so the window was always self-limiting) or a contingency of specific reactions (imperial edicts, the 1557 Stationers'' charter, the 1559 Index)?',
    'Cross-media comparison: examine whether comparable permissive windows (early radio, early internet) closed through similar authority counter-mobilization patterns, or persisted absent specific political contingencies.',
    'If structural, the sunset clause is a genuine property of the arrangement and the scaffold classification holds; if contingent, the arrangement is better modeled as an open-ended coordination that particular politics happened to terminate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_emergence_vs_design, conceptual, 'Whether the window''s closure was inherent to the arrangement or historically contingent.').

omega_variable(
    extraction_attribution_coupling_vs_regime,
    'How much of the measured extraction (printer rents, privilege monopolies, licensing fees) belongs to the press-reform coupling itself versus the successor licensing-and-censorship regime that replaced it?',
    'Separate accounting of rent streams before and after 1555: boom-era margins on controversy titles versus post-confessionalization privilege rents and official licensing fees.',
    'If most extraction is successor-regime rent, this arrangement''s true epsilon is lower and the scaffold reading strengthens; if boom-era rents dominate, the coupling carried more extraction than this reading''s own lights assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution_coupling_vs_regime, empirical, 'Attribution of measured extraction between the coupling and its successor regime.').

omega_variable(
    scribal_displacement_attribution,
    'Does the scribal workforce''s displacement count as a cost of the press-reform coupling, or of print adoption generally — which the determinism reading says would have proceeded identically without any reform?',
    'Model manuscript-economy decline curves against print diffusion rates region by region; displacement timed to reform-driven output surges attributes it to the coupling, while uniform decline tracks print adoption generally.',
    'Attributing displacement to the coupling raises its effective extraction and pushes classification toward tangled-rope territory; attributing it to print adoption generally thins the victim set and supports the scaffold reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scribal_displacement_attribution, conceptual, 'Whether scribal displacement is attributable to the coupling or to print adoption per se.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reform_mutual_shaping_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.04).
narrative_ontology:measurement(press_reform_mutual_shaping_tr_t1480, press_reformation_causation__mutual_shaping, theater_ratio, 1480, 0.05).
narrative_ontology:measurement(press_reform_mutual_shaping_tr_t1500, press_reformation_causation__mutual_shaping, theater_ratio, 1500, 0.06).
narrative_ontology:measurement(press_reform_mutual_shaping_tr_t1517, press_reformation_causation__mutual_shaping, theater_ratio, 1517, 0.06).
narrative_ontology:measurement(press_reform_mutual_shaping_tr_t1530, press_reformation_causation__mutual_shaping, theater_ratio, 1530, 0.08).
narrative_ontology:measurement(press_reform_mutual_shaping_tr_t1555, press_reformation_causation__mutual_shaping, theater_ratio, 1555, 0.11).
narrative_ontology:measurement(press_reform_mutual_shaping_tr_t1600, press_reformation_causation__mutual_shaping, theater_ratio, 1600, 0.12).

% Extraction over time
narrative_ontology:measurement(press_reform_mutual_shaping_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.14).
narrative_ontology:measurement(press_reform_mutual_shaping_be_t1480, press_reformation_causation__mutual_shaping, base_extractiveness, 1480, 0.19).
narrative_ontology:measurement(press_reform_mutual_shaping_be_t1500, press_reformation_causation__mutual_shaping, base_extractiveness, 1500, 0.23).
narrative_ontology:measurement(press_reform_mutual_shaping_be_t1517, press_reformation_causation__mutual_shaping, base_extractiveness, 1517, 0.31).
narrative_ontology:measurement(press_reform_mutual_shaping_be_t1530, press_reformation_causation__mutual_shaping, base_extractiveness, 1530, 0.37).
narrative_ontology:measurement(press_reform_mutual_shaping_be_t1555, press_reformation_causation__mutual_shaping, base_extractiveness, 1555, 0.34).
narrative_ontology:measurement(press_reform_mutual_shaping_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(press_reform_mutual_shaping_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.05).
narrative_ontology:measurement(press_reform_mutual_shaping_su_t1480, press_reformation_causation__mutual_shaping, suppression_requirement, 1480, 0.08).
narrative_ontology:measurement(press_reform_mutual_shaping_su_t1500, press_reformation_causation__mutual_shaping, suppression_requirement, 1500, 0.12).
narrative_ontology:measurement(press_reform_mutual_shaping_su_t1517, press_reformation_causation__mutual_shaping, suppression_requirement, 1517, 0.18).
narrative_ontology:measurement(press_reform_mutual_shaping_su_t1530, press_reformation_causation__mutual_shaping, suppression_requirement, 1530, 0.38).
narrative_ontology:measurement(press_reform_mutual_shaping_su_t1555, press_reformation_causation__mutual_shaping, suppression_requirement, 1555, 0.62).
narrative_ontology:measurement(press_reform_mutual_shaping_su_t1600, press_reformation_causation__mutual_shaping, suppression_requirement, 1600, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, resource_allocation).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'the printing press caused the Reformation' into three epsilon-invariant constraints: technological_determinism (unilateral force; negligible extraction — nobody collects from a natural process), strategic_deployment (aimed neutral tool; low extraction — coordination overhead only), and this file (mutual_shaping; epsilon 0.30 — boom rents to printer-publishers against scribal and gatekeeping losses). The determinism reading functions as the popular upstream claim this reading partially absorbs and partially refutes; all three cite the same archival base, so contamination propagates across the family — a purity failure in one reading (e.g., cherry-picked print-run data) degrades the evidential base of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
