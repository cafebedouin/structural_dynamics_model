% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print Economy as Transitional Infrastructure of the Reformation (Co-Constitution Reading)
 *   domain: historical/religious/media-economic
 *
 * SUMMARY:
 *   Between Luther's theses and the Peace of Augsburg, the early-modern print
 *   economy and the religious controversy it carried constituted each other
 *   through a feedback loop: cheap pamphlets lowered the cost of doctrinal
 *   attack, controversy created bestseller demand, demand financed new
 *   presses and smuggling networks, and expanded capacity carried more
 *   controversy. This story instantiates the co_constitution reading of the
 *   press_reformation_causality kernel: neither the technology nor the agents
 *   carry the explanation alone — the loop does. Epsilon's referent is the
 *   standing arrangement under contest, the print economy as it actually
 *   operated 1517-1555, assessed by this reading's own lights; the reading's
 *   endorsed alternative (whatever historiography prefers next) is not the
 *   referent. The colloquial label 'the printing press caused the
 *   Reformation' decomposes into three epsilon-invariant readings — this one,
 *   technological_determinism, and strategic_deployment — linked via
 *   network.affects_constraints; their epsilon values, beneficiary
 *   structures, and classifications are not averaged into this file. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as scaffold
 *   (transitional enabling infrastructure with a historical sunset) while the
 *   authored metrics describe moderately extractive, lightly suppressed,
 *   increasingly theatrical operation — the engine measures that divergence;
 *   do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - - printer_publishers: Agenda-setting beneficiaries (organized/mobile) — run the presses, finance editions, bear capital risk, collect controversy-driven profits
 *   - - reformation_publicists: Primary doctrinal beneficiaries (powerful/constrained) — gain reach no pulpit could deliver, cannot exit the medium without forfeiting it
 *   - - literate_urban_readers: Mass beneficiaries (moderate/mobile) — buy access to unmediated scripture and polemic at low commitment cost
 *   - - catholic_ecclesiastical_hierarchy: Primary target (institutional/trapped) — loses doctrinal gatekeeping, must fight inside the hostile medium
 *   - - anabaptist_radical_pamphleteers: Abandoned early beneficiaries turned targets (powerless/trapped) — the closing window falls on them specifically
 *   - - imperial_city_magistrates: Regulatory agenda-setters (institutional/constrained) — license, tax, and progressively close the open window
 *   - - historians_of_print_culture: Analytical observer (analytical/analytical) — reconstructs the loop from ledgers and censorship records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.45).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.38).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, scaffold).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print Economy as Transitional Infrastructure of the Reformation (Co-Constitution Reading)").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "historical/religious/media-economic").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).
narrative_ontology:has_sunset_clause(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '177330dd-0c2f-495a-9472-a8926f3aac46').
narrative_ontology:cs_kernel_codification('177330dd-0c2f-495a-9472-a8926f3aac46', distributed).
narrative_ontology:cs_authority_grounding('177330dd-0c2f-495a-9472-a8926f3aac46', expertise).
narrative_ontology:cs_interpretation_layer_present('177330dd-0c2f-495a-9472-a8926f3aac46').
narrative_ontology:cs_reading_relation('177330dd-0c2f-495a-9472-a8926f3aac46', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('177330dd-0c2f-495a-9472-a8926f3aac46', press_reformation_causality__strategic_deployment, forecloses).
narrative_ontology:cs_axiom('177330dd-0c2f-495a-9472-a8926f3aac46', foundational, outcomes_emerge_from_reciprocal_medium_agency_shaping).
narrative_ontology:cs_axiom_status(outcomes_emerge_from_reciprocal_medium_agency_shaping, holdable).
narrative_ontology:cs_axiom_grounding('177330dd-0c2f-495a-9472-a8926f3aac46', outcomes_emerge_from_reciprocal_medium_agency_shaping, empirically_contingent).
narrative_ontology:cs_axiom('177330dd-0c2f-495a-9472-a8926f3aac46', foundational, technological_effects_are_contingent_on_human_uptake).
narrative_ontology:cs_axiom_status(technological_effects_are_contingent_on_human_uptake, holdable).
narrative_ontology:cs_axiom_grounding('177330dd-0c2f-495a-9472-a8926f3aac46', technological_effects_are_contingent_on_human_uptake, empirically_contingent).
narrative_ontology:cs_reference_frame('177330dd-0c2f-495a-9472-a8926f3aac46', reciprocal_media_agency_feedback_loop).
narrative_ontology:cs_drift_state('177330dd-0c2f-495a-9472-a8926f3aac46', post_revisionist_book_history, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('177330dd-0c2f-495a-9472-a8926f3aac46', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printer_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformation_publicists).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, literate_urban_readers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_ecclesiastical_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, anabaptist_radical_pamphleteers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the shops in Wittenberg, Strasbourg, Basel, Augsburg and the imperial free cities: choose manuscripts, finance editions, hire compositors, and decide what circulates. Controversy-driven bestsellers concentrate profits in their hands, but they alone bear capital ruin when an edition fails and expulsion when a city council turns hostile; many relocated repeatedly, carrying punches and matrices to friendlier jurisdictions.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printer_publishers, agenda_setter,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printer_publishers, beneficiary).

% Luther, Melanchthon, and the pamphleteering clergy around them gain reach no pulpit or university disputation could deliver: vernacular tracts move faster than any rebuttal. They depend on printer schedules, city politics, and the smuggling networks that carry banned works; withdrawing from print would concede the medium entirely to opponents, and their public identities are fused with the movement the medium built.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformation_publicists, beneficiary,
    powerful, generational, constrained, continental).

% Townsmen, artisans, students, and lower clergy who buy pamphlets, broadsheets, and vernacular New Testaments. They gain direct access to scripture and polemic without clerical mediation, at prices a skilled wage can cover. Their commitment cost is low: they can stop buying, switch confessional allegiance, or ignore the market entirely, and their aggregate demand is what finances the loop.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, literate_urban_readers, beneficiary,
    moderate, immediate, mobile, regional).

% Rome, the bishops, and the university theology faculties lose the practical ability to control doctrinal dissemination: printed attack outruns written rebuttal, and bans are unenforceable across the Empire's fragmented jurisdictions. They are forced to fight inside the medium they condemn — commissioning Catholic polemic, funding rival presses, and eventually building the Index — because abandoning print means forfeiting the field altogether.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_ecclesiastical_hierarchy, payer,
    institutional, generational, trapped, continental).

% Radical evangelicals — Karlstadt's circle, the Swiss Brethren, apocalyptic pamphleteers — ride the open print window in its earliest years, then are abandoned when Lutheran and Reformed establishments consolidate: Protestant cities ban their presses, both confessions burn their books, and imperial and civic authorities execute their leaders. The transitional openness that carried them closes on them specifically, and they have nowhere to relocate that is safe.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, anabaptist_radical_pamphleteers, payer,
    powerless, biographical, trapped, regional).

% Councils of imperial free cities and territorial princes issue print ordinances, demand registered imprints and licenses, and balance fiscal interest in a thriving local print trade against confessional commitment and imperial pressure. Enforcement capacity varies wildly between jurisdictions; their successive ordinances are the visible mechanism by which the open window progressively closes.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, imperial_city_magistrates, agenda_setter,
    institutional, biographical, constrained, regional).

% Modern scholars of book history and the Reformation reconstruct the loop from printer ledgers, edition counts, privilege records, and censorship archives. They hold no stake in the arrangement and produce the evidentiary basis on which every reading of the causality dispute stands or falls.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, historians_of_print_culture, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Connects dispersed critics, readers, and markets into a self-reinforcing public: standardized cheap reproduction moves identical arguments across linguistic regions faster than any institutional countermeasure can travel, and controversy-driven demand finances the expansion of the very capacity that carries more controversy.
% TRANSFER_FUNCTION: Moves coin from pamphlet buyers to printer-publishers; doctrinal initiative from credentialed clerical gatekeepers to polemicists and lay readers; reputational and legal risk onto authors and printers; and, after consolidation, legal protection to licensed confessional presses at the expense of unlicensed ones.
% ABSENT_VOICES: Illiterate rural majorities — most of Europe — have no seat: the loop optimizes for literate urban purchasers. Women, the unlettered poor, and oral-culture practitioners appear only as objects of concern inside the pamphlets, never as participants. After consolidation, the radical left, previously vocal, is silenced out of the conversation by both confessions simultaneously.
% DISAPPEARANCE_RATIONALE: Remove the print economy in 1517 and the sequence rearranges: Luther's theses remain a local academic dispute; vernacular Bibles circulate at manuscript speed and price; no pamphlet war finances itself; and any break with Rome happens on a dynastic-political timetable rather than a media-economic one. Every stakeholder's position — the printers' trade, the reformers' reach, the readers' access, the hierarchy's loss, the radicals' brief opening — is constituted by the loop.
% FOUNDING_PROBLEM: Commercially scalable text reproduction: replacing scriptoria with presswork so texts could be copied profitably at volume — a problem the print economy solved decades before the Reformation and pursued continuously thereafter.
% FOUNDING_PROBLEM_CORROBORATION: Printer account books, guild registers, and imperial fair records corroborate the commercial founding problem from outside any confessional party, and the economic history of the book (the Febvre-Martin tradition onward) attests that profitability governed what printed regardless of doctrine. No party disputes that the trade predates and outlasts the Reformation's use of it.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).
:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed scaffold: the print economy's Reformation-era function was transitional — it built the confessional public and was then closed down by the very settlements it made possible (imperial print ordinances, Protestant city licensing, the Index). The sunset is historical fact, not aspiration: the open window of 1517-1555 ended. Metrics are authored independently of the claim. Extractiveness 0.45 reflects real but distributed costs — gatekeepers displaced, radicals abandoned — rather than concentrated rent. Suppression 0.38 reflects weak, late, fragmented enforcement rather than systematic coercion; suppression is authored as a raw structural property and is not scaled by scope or directionality — only extractiveness is scaled, in the engine's computation. Theater_ratio 0.30 tracks the widening gap between pious prefaces and commercial practice as the trade matured. Accessibility_collapse 0.40 because manuscript, oral, and visual channels persisted as workable alternatives throughout — print dominated but did not eliminate them. Resistance 0.60 because the hierarchy fought continuously (burnings, bans, counter-pamphlets) and printers lived under constant legal exposure. All three tracked series run on one shared eight-point grid (1517-1555) so no metric borrows another's timeline; the rising suppression_requirement series is authored because enforcement machinery (ordinances, licensing, visitation) visibly built up over the interval. Receipt surface: gains distribute across four seats — commercial surplus to printer_publishers, doctrinal reach to reformation_publicists, territorial-institutional gains to magistracies, access to readers — after checking each named seat, no single one captures the extraction imposed on the payers, hence the affirmative 'diffuse'. Fixing cost: prohibitive — no actor in the interval could suppress or redirect the loop at acceptable cost; imperial enforcement was fragmented, every city's fiscal interest favored its print trade, and unilateral suppression merely relocated production.
 *
 * PERSPECTIVAL GAP:
 *   From the printer's bench the arrangement reads as risky commerce occasionally blessed by providence; from the episcopal palace as an ungovernable flood that destroyed a millennium-old gatekeeping office; from the Anabaptist meetinghouse as a door that opened and then slammed shut. Same structure, three experiences: enabling market, catastrophe, betrayal. The engine computes these per-seat divergences from power, exit, and directionality data; the scaffold claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: printer_publishers (profits), reformation_publicists (reach), literate_urban_readers (access) — their d sits near the beneficiary end, damping effective extraction for those seats. Victims: catholic_ecclesiastical_hierarchy (institutional power but trapped exit — leaving the medium forfeits the field) and anabaptist_radical_pamphleteers (powerless and trapped — the window closed on them) — d near the target end, amplifying chi. One override: printer_publishers derive near-pure-beneficiary directionality from their beneficiary declaration and mobile exit, but they alone bore capital ruin and expulsion risk, so their override moves d to 0.22 — nearer symmetric than a pure collector. The override is keyed to the organized power atom, which in this story only the printers occupy, so it targets them without touching other seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding commercial problem (scalable, profitable reproduction) never died — print outlived the Reformation's use of it — so this is not mandate-atrophy in the classic sense; what sunset was the arrangement's transitional political function. Classifying as scaffold prevents two misreadings: reading the print economy as a pure rope (a neutral blessing that merely served pre-existing intentions — the determinist error) or as a snare (a predatory machine that manufactured controversy for profit — the cynic's error). The truth is transitional: a coordination structure whose justification was the change it enabled, withdrawn — unevenly, and most brutally from its earliest radical beneficiaries — once the change completed. The mismatch consumer should find no zombie flag here: founding_problem_status is live (the commercial substrate persists) and the disappearance verdict is world_rearranges (the Reformation-era arrangements genuinely depend on it), and these are consistent because the atrophied thing is the open-window function, not the trade itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the co_constitution reading of the press_reformation_causality kernel; would the technological_determinism or strategic_deployment readings classify the same arrangement differently, and where exactly is the disagreement located?',
    'Compare the three stories'' epsilon, beneficiary structures, and computed types: determinism predicts a fixed enabling condition with negligible extraction; deployment predicts concentrated capture by a reformer-printer coalition; co-constitution predicts a transitional scaffold with distributed extraction. The disagreement is located in causal primacy — medium, agents, or loop.',
    'Adopting determinism would reclassify the print economy toward mountain-side inevitability; adopting deployment would reclassify it toward snare/tangled_rope instrumented capture; this reading''s scaffold classification holds only under mutual-constitution premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Kernel-level reading choice: which causal-primacy premise governs classification.').

omega_variable(
    counterfactual_diffusion_share,
    'What share of the Reformation''s spread is attributable to print specifically, versus preaching networks, trade routes, and princely patronage?',
    'Comparative diffusion analysis across regions matched on politics and commerce but differing in press density; city-level adoption studies with press-presence controls.',
    'A small print-specific share would shrink the scaffold''s credited coordination function and raise effective extraction relative to function; a large share supports the transitional-infrastructure reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_diffusion_share, empirical, 'How much of the outcome the medium itself carries, net of confounders.').

omega_variable(
    sunset_terminal_or_mutated,
    'Did the open print window terminate at the confessional settlements (a true sunset), or mutate into licensed confessional print regimes that continued the same loop under new management?',
    'Trace enforcement records and edition economics past 1555: if cross-confessional controversy economics resumed under licensing, the sunset is nominal; if the open phase genuinely ended, the sunset is real.',
    'A nominal sunset pushes the later-period classification toward tangled_rope (confessional licensing coordinating believers while extracting from dissidents); a real sunset preserves the scaffold reading across the full interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_terminal_or_mutated, conceptual, 'Whether the declared sunset clause marks termination or transformation.').

omega_variable(
    displacement_vs_extraction,
    'Do the costs borne by the ecclesiastical hierarchy and the radical pamphleteers count as extraction imposed by the print economy, or as ordinary competitive displacement that any cheaper communication technology would have produced?',
    'A value judgment on whether displaced gatekeepers and abandoned allies are owed protection from infrastructural change; resolved by taking a stance on infrastructural liability, not by further data.',
    'Reading the costs as displacement lowers effective extraction toward the coordination-cost floor; reading them as extraction raises it and strengthens the tangled_rope residue within the scaffold period.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_vs_extraction, preference, 'Whether the constraint''s casualties are extraction victims or ordinary creative destruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__co_constitution, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(pres_tr_t1517, observed).
narrative_ontology:measurement(pres_tr_t1521, press_reformation_causality__co_constitution, theater_ratio, 1521, 0.14).
narrative_ontology:measurement_basis(pres_tr_t1521, observed).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__co_constitution, theater_ratio, 1525, 0.18).
narrative_ontology:measurement_basis(pres_tr_t1525, observed).
narrative_ontology:measurement(pres_tr_t1529, press_reformation_causality__co_constitution, theater_ratio, 1529, 0.21).
narrative_ontology:measurement_basis(pres_tr_t1529, observed).
narrative_ontology:measurement(pres_tr_t1534, press_reformation_causality__co_constitution, theater_ratio, 1534, 0.24).
narrative_ontology:measurement_basis(pres_tr_t1534, observed).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__co_constitution, theater_ratio, 1540, 0.27).
narrative_ontology:measurement_basis(pres_tr_t1540, observed).
narrative_ontology:measurement(pres_tr_t1548, press_reformation_causality__co_constitution, theater_ratio, 1548, 0.29).
narrative_ontology:measurement_basis(pres_tr_t1548, observed).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__co_constitution, theater_ratio, 1555, 0.3).
narrative_ontology:measurement_basis(pres_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__co_constitution, base_extractiveness, 1517, 0.26).
narrative_ontology:measurement_basis(pres_be_t1517, observed).
narrative_ontology:measurement(pres_be_t1521, press_reformation_causality__co_constitution, base_extractiveness, 1521, 0.3).
narrative_ontology:measurement_basis(pres_be_t1521, observed).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__co_constitution, base_extractiveness, 1525, 0.36).
narrative_ontology:measurement_basis(pres_be_t1525, observed).
narrative_ontology:measurement(pres_be_t1529, press_reformation_causality__co_constitution, base_extractiveness, 1529, 0.39).
narrative_ontology:measurement_basis(pres_be_t1529, observed).
narrative_ontology:measurement(pres_be_t1534, press_reformation_causality__co_constitution, base_extractiveness, 1534, 0.41).
narrative_ontology:measurement_basis(pres_be_t1534, observed).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__co_constitution, base_extractiveness, 1540, 0.43).
narrative_ontology:measurement_basis(pres_be_t1540, observed).
narrative_ontology:measurement(pres_be_t1548, press_reformation_causality__co_constitution, base_extractiveness, 1548, 0.44).
narrative_ontology:measurement_basis(pres_be_t1548, observed).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__co_constitution, base_extractiveness, 1555, 0.45).
narrative_ontology:measurement_basis(pres_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__co_constitution, suppression_requirement, 1517, 0.12).
narrative_ontology:measurement_basis(pres_su_t1517, observed).
narrative_ontology:measurement(pres_su_t1521, press_reformation_causality__co_constitution, suppression_requirement, 1521, 0.16).
narrative_ontology:measurement_basis(pres_su_t1521, observed).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__co_constitution, suppression_requirement, 1525, 0.22).
narrative_ontology:measurement_basis(pres_su_t1525, observed).
narrative_ontology:measurement(pres_su_t1529, press_reformation_causality__co_constitution, suppression_requirement, 1529, 0.27).
narrative_ontology:measurement_basis(pres_su_t1529, observed).
narrative_ontology:measurement(pres_su_t1534, press_reformation_causality__co_constitution, suppression_requirement, 1534, 0.31).
narrative_ontology:measurement_basis(pres_su_t1534, observed).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__co_constitution, suppression_requirement, 1540, 0.34).
narrative_ontology:measurement_basis(pres_su_t1540, observed).
narrative_ontology:measurement(pres_su_t1548, press_reformation_causality__co_constitution, suppression_requirement, 1548, 0.37).
narrative_ontology:measurement_basis(pres_su_t1548, observed).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__co_constitution, suppression_requirement, 1555, 0.38).
narrative_ontology:measurement_basis(pres_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'the printing press caused the Reformation' into three epsilon-invariant readings of one kernel: this file (co_constitution — transitional scaffold, distributed extraction), technological_determinism (fixed enabling condition, negligible extraction), strategic_deployment (instrumented capture by a reformer-printer coalition). Each carries its own epsilon, beneficiaries, and claimed type per the epsilon-invariance principle; the upstream determinism and deployment claims are routinely cited as evidence inside co-constitution debates, so edges run from this synthetic reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__co_constitution, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
