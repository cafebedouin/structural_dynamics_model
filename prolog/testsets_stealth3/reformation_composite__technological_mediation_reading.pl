% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Print Mediation Regime of the Early Reformation (Technological-Mediation Reading)
 *   domain: historical epistemology/religious history/political economy
 *
 * SUMMARY:
 *   This story instantiates the technological_mediation_reading of the
 *   reformation_composite kernel: the claim that the decisive transformation
 *   of the Reformation ran through the medium — that movable-type replication
 *   converted local theological dissent, which manuscript-era Europe had
 *   absorbed for centuries, into a continental mass movement by making
 *   identical text cheap, fast, and border-crossing. The constraint under
 *   authorship is the print-mediation regime itself (c. 1450-1555): the
 *   standing arrangement of presses, paper supply, edition economics, and
 *   reading publics, assessed by this reading's own lights as enabling
 *   infrastructure rather than extraction machinery. KEY AGENTS (by
 *   structural relationship): - commercial_printers_publishers:
 *   agenda-setting operator (institutional/mobile) — runs the replication
 *   infrastructure, decides what enters circulation, collects the trade
 *   surplus - reformist_pamphleteers: primary beneficiary
 *   (organized/identity_locked) — collects reach and institution-building
 *   capacity from replication they do not operate -
 *   literate_urban_lay_readers: beneficiary (moderate/mobile) — gains direct
 *   textual access previously gated by clergy -
 *   traditional_manuscript_trades: payer (powerless/trapped) — bears
 *   displacement as commissioned work migrates to print -
 *   ecclesiastical_hierarchy: payer (institutional/constrained) — loses its
 *   informational gatekeeping position faster than countermeasures rebuild it
 *   - territorial_censorship_offices: agenda-setter
 *   (institutional/constrained) — administers licensing and prohibition at
 *   the regime's edges - historians_of_the_book: analytical observer —
 *   reconstructs output and literacy series from the documentary record.
 *   FAMILY RELATIONSHIP AND EPSILON DIFFERENCES: the colloquial label 'the
 *   Reformation' decomposes, per the epsilon-invariance principle, into three
 *   structurally distinct claims sharing one kernel. This file authors
 *   epsilon (0.26) for the mediation layer — the replication arrangement,
 *   whose costs fall mainly on displaced manuscript labor and whose rents
 *   accrue to the print trade. The theological_fragmentation_reading authors
 *   epsilon against a different arrangement (doctrinal exclusion and
 *   prosecution structures, different victim set); the
 *   political_realignment_reading authors epsilon against the
 *   licensing-sovereignty machinery (princes and councils as beneficiaries,
 *   distinct extraction surface). Same label, three constraints, three files,
 *   linked by network edges; this reading's observables (publication rates,
 *   literacy) are cited as evidence by both siblings, so influence runs
 *   outward from this file.
 *
 * KEY AGENTS:
 *   - - commercial_printers_publishers: agenda-setting operator (institutional/mobile) — operates the presses, decides edition-by-edition what circulates, collects the margin and the controversy windfalls
 *   - - reformist_pamphleteers: primary beneficiary (organized/identity_locked) — converts replication capacity into reach, reputation, and new institutions; cannot retract without ruin
 *   - - literate_urban_lay_readers: beneficiary (moderate/mobile) — acquires direct access to scripture and polemic; exit is cheap but the acquired habit persists
 *   - - traditional_manuscript_trades: payer (powerless/trapped) — copyists and scriptoria whose skills do not transfer and whose guild standing carries no claim on the new trade
 *   - - ecclesiastical_hierarchy: payer (institutional/constrained) — doctrinal gatekeeper whose chokepoint dissolves faster than prohibitions and counter-printing can respond
 *   - - territorial_censorship_offices: agenda-setter (institutional/constrained) — licenses, taxes, and prohibits at the regime's edge; jurisdiction-bound while the trade is not
 *   - - historians_of_the_book: analytical observer (analytical/analytical) — reconstructs the output and literacy series from colophons, inventories, and library records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.26).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.12).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.14).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Print Mediation Regime of the Early Reformation (Technological-Mediation Reading)").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical epistemology/religious history/political economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '38b81b5b-5e27-4c22-b9cf-ef70dee7a715').
narrative_ontology:cs_kernel_codification('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', distributed).
narrative_ontology:cs_authority_grounding('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', expertise).
narrative_ontology:cs_interpretation_layer_present('38b81b5b-5e27-4c22-b9cf-ef70dee7a715').
narrative_ontology:cs_reading_relation('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', foundational, mediation_capacity_sets_movement_scale).
narrative_ontology:cs_axiom_status(mediation_capacity_sets_movement_scale, holdable).
narrative_ontology:cs_axiom_grounding('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', mediation_capacity_sets_movement_scale, empirically_contingent).
narrative_ontology:cs_axiom('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', secondary, replication_economics_precede_confessional_crystallization).
narrative_ontology:cs_axiom_status(replication_economics_precede_confessional_crystallization, holdable).
narrative_ontology:cs_axiom_grounding('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', replication_economics_precede_confessional_crystallization, empirically_contingent).
narrative_ontology:cs_reference_frame('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', media_materialist_baseline).
narrative_ontology:cs_drift_state('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', post_print_fixity_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('38b81b5b-5e27-4c22-b9cf-ef70dee7a715', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, commercial_printers_publishers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, reformist_pamphleteers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_lay_readers).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, traditional_manuscript_trades).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, ecclesiastical_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the presses that decide, edition by edition, what text enters circulation; set prices, formats, and print runs within the limits local licensors allow. Collect the margin between production cost and sale price, and the windfalls of controversy-driven demand. Exit is real: presses, type, and stock are movable, and printers routinely relocated to friendlier jurisdictions when licenses tightened.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, commercial_printers_publishers, agenda_setter,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, commercial_printers_publishers, beneficiary).

% Compose the sermons, tracts, and translations that fill the presses; collect reach, reputation, and the beginnings of new institutions from a replication capacity no manuscript network ever offered. Once publicly committed, they cannot retract without spiritual and social ruin — their standing is fused with the movement the pamphlets build.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, reformist_pamphleteers, beneficiary,
    organized, generational, identity_locked, continental).

% Buy and read vernacular scripture, polemic, and news; gain direct access to texts previously mediated through clergy. Pay purchase prices and, in stricter territories, risk penalties for possession. Leaving the reading public is easy — stop buying — but the habit, once formed, reshapes what authority they accept.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_urban_lay_readers, beneficiary,
    moderate, biographical, mobile, continental).

% Copyists, scriptoria, and stationers of the old kind watch commissioned work migrate to the print shops. Their skills do not transfer: a trained hand cannot become a type-founder overnight, and their guild positions carry no claim on the new trade. Most decline into niche luxury work or leave the trade entirely.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, traditional_manuscript_trades, payer,
    powerless, biographical, trapped, regional).

% Holds doctrinal gatekeeping as a core asset: license to preach, license to publish, the pulpit as the channel. Cheap replication dissolves the chokepoint faster than countermeasures can rebuild it; prohibitions arrive after the editions do. Its options are reactive — indexes, rebuttal commissions, counter-printing — each conceding that the arena has moved to the press.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, ecclesiastical_hierarchy, payer,
    institutional, civilizational, constrained, continental).

% Administer licensing, pre-publication approval, and prohibition lists on behalf of princes and city councils. They tax and register the trade where they can, suppress editions where they must, and watch neighboring jurisdictions undercut every ban. Their reach stops at the border; the presses do not.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, territorial_censorship_offices, agenda_setter,
    institutional, generational, constrained, regional).

% Modern scholars reconstruct output figures, edition counts, and literacy curves from colophons, inventories, and library records. They take no position inside the sixteenth-century arrangement; their disputes concern which observable class carries the explanatory weight.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historians_of_the_book, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, commercial_printers_publishers).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the replication problem: producing many identical copies of a text at falling marginal cost, so that dispersed readers hold the same words on the same page. Identical editions create a shared reference space — citation, comparison, and cumulative argument become possible across distance — and decouple the spread of a message from the travel of any messenger.
% TRANSFER_FUNCTION: Moves standardized text from a small class of producers to an expanding reading public; moves money from buyers to printer-publishers; moves interpretive authority away from licensed clerical intermediaries toward whoever can read; and moves attention toward whatever controversy reproduces cheapest.
% ABSENT_VOICES: Non-literate rural majorities, whose religious life the print public sphere bypasses entirely; women, largely barred from the trades and Latin schooling that fed authorship and presswork; and the displaced manuscript workforce, which has no seat where the new trade sets terms. All stand outside the commercial networks where decisions about what circulates are made.
% DISAPPEARANCE_RATIONALE: Remove cheap replication overnight and dissent stays local: a Luther remains a provincial dispute argued in Latin at manuscript speed, reaching hundreds rather than hundreds of thousands; no shared edition space forms, so no continental alignment of grievances and no durable denominational identities crystallize. Princes lose the propaganda channel; Rome keeps its gatekeeping. The sixteenth century rearranges around slower media.
% FOUNDING_PROBLEM: Manuscript production could not supply standardized text at the scale demand required: copies diverged, prices stayed high, and distribution rode on personal networks. Commercial print was assembled to solve replication — profitably multiplying identical objects — and religious mobilization arrived as an unplanned tenant of that solution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historiography of the book (production-cost studies, edition censuses) and the documented succession of later replication media attest that the problem the press solved was solved permanently and is now carried by successor technologies; no source outside the print trade's own commemorative tradition holds press-based mediation to be the operative replication regime today.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.26 at interval end) because the regime's core operation — selling replicated text near competitive cost — is enabling; the rise across the series tracks controversy-driven premiums and early trade consolidation (privileges, emerging company structures), not a growing coercive take. Suppression is authored low (0.12) and carried as a static scalar with no temporal series: the medium itself coerces almost nothing, and the enforcement machinery visible in the record (indexes, prosecutions) targets content and sovereignty — machinery assigned to the sibling readings, with the boundary question routed to the suppression_boundary_with_siblings omega rather than smuggled into this scalar. Theater is near-floor (0.10 at end) because the function is entirely real during the interval; the gentle rise marks guild ritualization and privilege-page formalities, not proxy-goal drift. Accessibility_collapse is high (0.82) but honestly short of totality: once print economics are understood, manuscript replication cannot compete on cost for ordinary text, yet manuscript work survives in luxury, music, and presentation niches. Resistance is low (0.14): opposition targeted titles and authors, not the medium, which no actor could have removed at any acceptable cost. Both temporal series run on one shared eight-point grid (t=0,20,40,60,70,80,90,100 mapping c.1450 to c.1555, with t=70 marking the pamphlet-flood years after 1517) so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The mountain claim predicts index-free uniformity — every seat computes the same type, zero degrees of freedom. The structural data deliberately stresses that prediction: two payer seats (manuscript trades, hierarchy) experienced the regime as the dissolution of their position, while three beneficiary seats experienced it as liberation and opportunity. From the printer's bench the regime is an industry; from the scriptorium it is a death sentence; from Rome it is a breach in the wall. Whether that asymmetry demotes the regime from mountain to tangled_rope is exactly what the false-summit evaluation exists to measure — the beneficiary declarations are authored so the engine confronts the question rather than the author pre-answering it.
 *
 * DIRECTIONALITY LOGIC:
 *   Printer-publishers sit nearest the beneficiary pole: they collect the surplus and control the levers, with arbitrage-grade mobility (presses relocate across borders at will). Lay readers derive low directionality from the beneficiary declaration plus cheap exit. The ecclesiastical hierarchy derives high directionality as a trapped institutional payer — its gatekeeping rents dissolve and its countermeasures concede the new arena. Manuscript trades derive high directionality as powerless, skill-trapped payers. One override is declared: reformist_pamphleteers (power atom 'organized', d=0.15). The automatic derivation risks misreading their identity_locked exit as target-proximity — the lock-in modulation assumes locked agents sit nearer the full-target end — but structurally these agents collected reach, prestige, and institution-building capacity from the regime; their lock-in is commitment to a winning position, not captivity inside an extractive one. The override corrects the exit-modulation term without touching the beneficiary declaration that anchors it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards two opposite mislabels. Against snare: the hierarchy's catastrophic losses tempt a reading of the regime as predatory, but loss of a monopoly position to a superior technology is competitive displacement, not extraction — the regime transferred no rent from the hierarchy to the press; it deleted the hierarchy's rent stream. Against piton: the regime's function was fully live through the interval, so its tiny theater ratio is honest rather than symptomatic. The genuine obsolescence question — the regime is today a closed historical formation superseded by later replication media — is carried by the R5 interview (founding_problem_status: dead against disappearance_verdict: world_rearranges), which routes the zombie hypothesis to investigation instead of letting a present-day verdict retroactively stain the interval's classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_infrastructure,
    'Is the print-mediation regime a natural-law-like fixed constraint on early-modern communication, or a constructed commercial arrangement whose beneficiaries (printer-publishers, movement entrepreneurs) present it as inevitable?',
    'Counterfactual and comparative analysis: whether equivalent mediation regimes arose wherever the material preconditions held irrespective of intent, and whether contemporaries experienced the regime as alterable (petitioning for privileges, relocating presses across borders).',
    'If constructed, the mountain claim fails the false-summit check and the engine reclassifies toward tangled_rope with printer-publishers as coordinated beneficiaries and the manuscript trades as payers; if natural-law-like, the mountain certification stands and the beneficiary declarations are diagnostic residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_infrastructure, conceptual, 'Whether the press regime is a fixed feature of the communication environment or a constructed arrangement presented as one.').

omega_variable(
    kernel_reading_membership,
    'This constraint is the technological_mediation_reading of the reformation_composite kernel; what would change structurally if a sibling reading were instantiated instead?',
    'Compare the sibling stories'' beneficiary/victim sets and epsilon referents: the theological_fragmentation_reading authors the doctrinal-exclusion arrangement (confessional tests, heresy prosecution) with its own victim set; the political_realignment_reading authors the licensing-sovereignty arrangement with princes as beneficiaries.',
    'Relocates the fundamental-driver claim and reassigns which observable class (publication rates versus confessional propositions versus sovereignty instruments) carries classification weight; this file''s low epsilon is valid only for the mediation layer and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: kernel membership, reading identity, and sibling structural deltas.').

omega_variable(
    necessary_vs_sufficient_mediation,
    'Was print mediation necessary, sufficient, or merely enabling for a continental mass movement — does replication capacity bound the achievable scale of dissent, or only open a channel that doctrine and politics then filled?',
    'Comparative cases with matched print density and divergent outcomes: Italian print centers where the movement failed to take, Iberian zones where print operated under tight control, and correlation of pamphlet-output peaks with movement milestones.',
    'If print is necessary-but-not-sufficient, the ''fundamentally technological'' claim weakens to an enabling condition and the foundational axiom narrows from determination of movement scale to bounding of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessary_vs_sufficient_mediation, empirical, 'Causal weight of mediation capacity in the scale of religious mobilization.').

omega_variable(
    literacy_causality_direction,
    'Do publication rates and literacy measure the mediation regime''s operation, or do they partly measure its consequences — was rising lay literacy a precondition the press exploited or a product the press manufactured?',
    'Sequence analysis of literacy proxies (signature rates, school foundations) against press density at regional granularity, instrumented by paper-mill locations as exogenous capacity shocks.',
    'If literacy is endogenous to print expansion, the reading''s two primary observables are jointly determined and the mediation claim must be tested against capacity shocks rather than adoption curves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_causality_direction, empirical, 'Direction of causality between the reading''s primary observables.').

omega_variable(
    suppression_boundary_with_siblings,
    'The authored suppression (0.12) measures the medium itself; much documented coercion (execution of printers, index prosecutions) targets content or sovereignty — is the low figure an honest reading-layer value or an artifact of assigning enforcement to sibling stories?',
    'Boundary audit: classify each documented coercive episode by whether it targets the medium (licensing of presses, registration of workshops) or content (prosecution of titles, pursuit of authors); only medium-targeted coercion belongs to this file''s suppression.',
    'If medium-targeted coercion was substantial, this reading''s suppression is understated and the enabling-infrastructure profile softens; if content-targeted, the boundary assignment stands and the sibling files carry the coercive load.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_boundary_with_siblings, conceptual, 'Where the regime''s coercion lives relative to the reading boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(refo_tr_t0, observed).
narrative_ontology:measurement(refo_tr_t20, reformation_composite__technological_mediation_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement_basis(refo_tr_t20, observed).
narrative_ontology:measurement(refo_tr_t40, reformation_composite__technological_mediation_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement_basis(refo_tr_t40, observed).
narrative_ontology:measurement(refo_tr_t60, reformation_composite__technological_mediation_reading, theater_ratio, 60, 0.04).
narrative_ontology:measurement_basis(refo_tr_t60, observed).
narrative_ontology:measurement(refo_tr_t70, reformation_composite__technological_mediation_reading, theater_ratio, 70, 0.05).
narrative_ontology:measurement_basis(refo_tr_t70, observed).
narrative_ontology:measurement(refo_tr_t80, reformation_composite__technological_mediation_reading, theater_ratio, 80, 0.06).
narrative_ontology:measurement_basis(refo_tr_t80, observed).
narrative_ontology:measurement(refo_tr_t90, reformation_composite__technological_mediation_reading, theater_ratio, 90, 0.08).
narrative_ontology:measurement_basis(refo_tr_t90, observed).
narrative_ontology:measurement(refo_tr_t100, reformation_composite__technological_mediation_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement_basis(refo_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(refo_be_t0, observed).
narrative_ontology:measurement(refo_be_t20, reformation_composite__technological_mediation_reading, base_extractiveness, 20, 0.17).
narrative_ontology:measurement_basis(refo_be_t20, observed).
narrative_ontology:measurement(refo_be_t40, reformation_composite__technological_mediation_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(refo_be_t40, observed).
narrative_ontology:measurement(refo_be_t60, reformation_composite__technological_mediation_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement_basis(refo_be_t60, observed).
narrative_ontology:measurement(refo_be_t70, reformation_composite__technological_mediation_reading, base_extractiveness, 70, 0.19).
narrative_ontology:measurement_basis(refo_be_t70, observed).
narrative_ontology:measurement(refo_be_t80, reformation_composite__technological_mediation_reading, base_extractiveness, 80, 0.21).
narrative_ontology:measurement_basis(refo_be_t80, observed).
narrative_ontology:measurement(refo_be_t90, reformation_composite__technological_mediation_reading, base_extractiveness, 90, 0.24).
narrative_ontology:measurement_basis(refo_be_t90, observed).
narrative_ontology:measurement(refo_be_t100, reformation_composite__technological_mediation_reading, base_extractiveness, 100, 0.26).
narrative_ontology:measurement_basis(refo_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(reformation_composite__technological_mediation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% Kernel-family decomposition of the colloquial label 'the Reformation' per the epsilon-invariance principle: the mediation layer (this file — the print regime as enabling infrastructure, epsilon authored low against the replication arrangement, victims limited to displaced manuscript labor and the dispossessed gatekeeper), the doctrinal layer (reformation_composite__theological_fragmentation_reading — exclusion and prosecution structures with a distinct victim set), and the sovereignty layer (reformation_composite__political_realignment_reading — licensing and censorship machinery with princes and councils as beneficiaries). Each file authors its own epsilon, beneficiaries, and victims; edges propagate contamination analysis across the family. Influence runs outward from this file because its observables (edition counts, literacy series) are the evidentiary substrate both siblings cite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__technological_mediation_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
