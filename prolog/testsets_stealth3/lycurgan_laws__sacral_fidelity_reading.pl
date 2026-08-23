% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Lycurgan Order as Divine Immutable Ordinance (Sacral Fidelity Reading)
 *   domain: political philosophy/constitutional theory/commitment systems
 *
 * SUMMARY:
 *   This story instantiates the sacral_fidelity_reading of the lycurgan_laws
 *   kernel: the Spartan ancestral order as delivered through the Delphic
 *   oracle, locked by the founder's return-oath, transmitted orally, and
 *   demanding absolute adherence as piety. Per the committer frame, the
 *   contest with the sibling readings (demographic_trap_reading,
 *   adaptive_fiction_reading) is routed to omega variables and cs_structure,
 *   not folded into this constraint: this file carries ONE epsilon over ONE
 *   referent — the standing Lycurgan arrangement as this reading's own lights
 *   assess it. The reading claims mountain (divine ordinance; naturality
 *   asserted via emerges_naturally), and the metrics are authored
 *   independently as descriptive facts about the arrangement's operation: an
 *   enforcement-saturated order with named victim classes and a late-interval
 *   drift toward performed piety. Where the computed classification diverges
 *   from the mountain claim, that divergence is the datum — the false-summit
 *   signature exists precisely for a purported natural law that names
 *   beneficiaries.
 *
 * KEY AGENTS:
 *   - spartiate_citizen_body: primary beneficiary (organized/identity_locked) — collects the surplus and the status; pays in total-life conscription
 *   - gerousia_elders: agenda setter and interpretive beneficiary (institutional/arbitrage) — administers the unwritten rules and absorbs drift into 'what the founder really said'
 *   - ephorate_magistrates: enforcing agenda setter with immediate horizon (institutional/constrained) — wields the machinery, answers for it annually
 *   - dual_royal_houses: hereditary beneficiary (powerful/constrained) — command legitimacy wholly derivative of the warrant; deviation prosecutable
 *   - helot_serf_population: primary target (powerless/trapped) — bears the productive and terror burdens; coalition-capable in revolt
 *   - deviant_spartiates: insider targets (moderate/identity_locked) — pay the absoluteness requirement with their standing
 *   - perioikoi_free_inhabitants: peripheral payers (moderate/constrained) — dues and blood without voice
 *   - delphic_oracle_priesthood: external warrant beneficiary (institutional/arbitrage) — collects patronage across hundreds of cities
 *   - foreign_visitors_expelled_under_xenelasia: excluded voices (moderate/mobile) — removed before speaking
 *   - greek_political_theorists: analytical observer (analytical/analytical) — scores the design from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.64).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.86).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Lycurgan Order as Divine Immutable Ordinance (Sacral Fidelity Reading)").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political philosophy/constitutional theory/commitment systems").

domain_priors:requires_active_enforcement(lycurgan_laws__sacral_fidelity_reading).
domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, 'ce718d8f-63e5-46e2-97be-aa626440d90d').
narrative_ontology:cs_kernel_codification('ce718d8f-63e5-46e2-97be-aa626440d90d', fixed_text).
narrative_ontology:cs_authority_grounding('ce718d8f-63e5-46e2-97be-aa626440d90d', lineage).
narrative_ontology:cs_interpretation_layer_present('ce718d8f-63e5-46e2-97be-aa626440d90d').
narrative_ontology:cs_reading_relation('ce718d8f-63e5-46e2-97be-aa626440d90d', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('ce718d8f-63e5-46e2-97be-aa626440d90d', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('ce718d8f-63e5-46e2-97be-aa626440d90d', foundational, rhetra_divinely_warranted).
narrative_ontology:cs_axiom_status(rhetra_divinely_warranted, holdable).
narrative_ontology:cs_axiom_grounding('ce718d8f-63e5-46e2-97be-aa626440d90d', rhetra_divinely_warranted, theological).
narrative_ontology:cs_axiom('ce718d8f-63e5-46e2-97be-aa626440d90d', foundational, immutability_secures_polity).
narrative_ontology:cs_axiom_status(immutability_secures_polity, holdable).
narrative_ontology:cs_axiom_grounding('ce718d8f-63e5-46e2-97be-aa626440d90d', immutability_secures_polity, empirically_contingent).
narrative_ontology:cs_reference_frame('ce718d8f-63e5-46e2-97be-aa626440d90d', divine_immutable_ordainment).
narrative_ontology:cs_drift_state('ce718d8f-63e5-46e2-97be-aa626440d90d', post_leuctra_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ce718d8f-63e5-46e2-97be-aa626440d90d', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_body).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, gerousia_elders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, dual_royal_houses).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_priesthood).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helot_serf_population).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, deviant_spartiates).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, perioikoi_free_inhabitants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, ephorate_magistrates).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, delphic_warrant_of_great_rhetra).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, constitutional_immutability_stability_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, founder_return_oath_binding_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Full citizens who eat at common messes funded by allotments worked by helot families; each contributes fixed quotas of barley, wine, and cheese to his mess and loses membership if he falls short. Trained from age seven in communal barracks, they hold equal status as 'peers' and monopolize citizenship and arms. Leaving the arrangement means losing mess, status, and self — the community is the entirety of their social world, and they know no other life than the one it prescribes.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_body, beneficiary,
    organized, generational, identity_locked, regional).

% Twenty-eight men over sixty, elected for life alongside the kings, who prepare business for the citizen assembly, judge capital cases, and keep custody of the unwritten law's meaning. When practice and the ancestral formula diverge, they decide what the founder 'really' said; changes adopted in assembly are recast as further words of the god at Delphi rather than acknowledged as revisions. They maneuver inside the rules they administer and collect the interpretive authority that comes with custodianship.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, gerousia_elders, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, gerousia_elders, beneficiary).

% Five annually elected overseers who watch the kings, convene the elders, police public conduct, and exchange a monthly oath with the kings binding both sides to the ancestral rules. Serving a single year, they face scrutiny and possible prosecution on leaving office, which pushes them toward strict, highly visible enforcement during their term; they wield the machinery but do not own it and answer for its use.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephorate_magistrates, agenda_setter,
    institutional, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, ephorate_magistrates, payer).

% Two hereditary royal lines claiming descent from Heracles, commanding the army in rotation and presiding over the major sacrifices. Bound by oaths renewed monthly with the overseers; kings who deviated — negotiating with enemies, proposing redistribution — faced prosecution, exile, or death, as the regent Pausanias did when he was starved out of the sanctuary he fled to. Their command legitimacy flows entirely from the ancestral warrant they are sworn to uphold.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, dual_royal_houses, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, dual_royal_houses, agenda_setter).

% Families of conquered Laconians and Messenians bound to specific plots, delivering most of each harvest to their citizen masters' messes. The state declares war on them annually so they may be killed without ritual pollution; a secret service raids at night and murders those judged strongest or best spoken. Some wear dog-skin caps, are forced to drink to excess for display, and are barred from assembly. Escape means outlawry; revolt is the only collective way out, attempted repeatedly from the Messenian wars through the great rising after the earthquake of 464.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_serf_population, payer,
    powerless, generational, trapped, regional).

% Citizens who failed the upbringing's thresholds, fell behind on mess contributions, or showed cowardice in battle. Struck from the peer roll, they become 'inferiors' or 'tremblers,' losing political rights while remaining physically inside the community that formed them — unable to leave and unable to belong. A conspiracy of such men around Kinadon in 397 showed how large this stratum had grown and how little channel existed for its grievances.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, deviant_spartiates, payer,
    moderate, biographical, identity_locked, regional).

% Free inhabitants of the towns around Laconia — traders, craftsmen, sailors — who fight in the army and pay dues but have no voice in the assembly, no share of citizenship, and no access to the citizen land monopoly. Their communities supply the commerce the citizens disdain while remaining permanently outside the sacred circle; their loyalty is purchased with autonomy everywhere except politics.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, perioikoi_free_inhabitants, payer,
    moderate, generational, constrained, regional).

% Keepers of the Delphic sanctuary whose pronouncement authorized the ancestral formula and whose shrine remains the court of appeal whenever the Spartans need the founder's authority reaffirmed. Sparta ranks among the sanctuary's most devoted patrons; the priesthood serves hundreds of cities at once and can withdraw its attention from any single one without loss, which keeps its position in this arrangement voluntary and cheap to leave.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, delphic_oracle_priesthood, beneficiary,
    institutional, civilizational, arbitrage, continental).

% Travelers, merchants, and teachers from other Greek cities whom the periodic expulsions remove before they can settle or speak. Athenian thinkers, Ionian sophists, and ordinary resident aliens find themselves escorted to the border; their objection — that a city sealed against strangers seals itself against correction — goes unheard by design, since their presence is what the expulsions exist to prevent.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, foreign_visitors_expelled_under_xenelasia, excluded,
    moderate, biographical, mobile, continental).

% Analysts from other poleis who study the Spartan arrangement from outside: Thucydides credits its fixity with four hundred years of stability; Aristotle catalogs its costs — the bound population's hatred, the concentration of land in women's hands, the shrinking citizen rolls — and judges the design superb for war and poor for peace. They bear none of its costs and collect none of its outputs; their seat is the scoring desk.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, greek_political_theorists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__sacral_fidelity_reading, spartiate_citizen_body).
narrative_ontology:fixing_cost_class(lycurgan_laws__sacral_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a small citizen class cohesive, uniformly trained, and continuously combat-ready atop a much larger bound farming population; standardizes warrior production through communal upbringing; suppresses wealth differentiation and the faction it breeds; routes all disputes about the ancestral rules to a fixed interpretive council backed by a distant oracle.
% TRANSFER_FUNCTION: Moves the bulk of helot agricultural output to citizen messes; moves the formative years of every citizen child into state institutions; moves political decision rights from the assembly floor into the elder council's agenda control; moves interpretive authority over the unwritten rules to the elders and, on demand, to Delphi.
% ABSENT_VOICES: Helots object only through revolt; expelled foreigners object from beyond the border they were carried past; degraded citizens object conspiratorially and anonymously. No institutional channel exists for any of them — the assembly hears only peers, and the agenda is set by the elders who also interpret the rules the assembly votes under.
% DISAPPEARANCE_RATIONALE: Overnight removal dissolves the citizen class's economic basis (unfunded messes), the army (no trained peers), and the labor regime (helot bindings were held by the same machinery); neighboring powers realign within a season and the bound population's emancipation reshapes the Peloponnese — as in fact happened when Theban arms broke the order in 371–369 and Messenia was refounded.
% FOUNDING_PROBLEM: Securing a small Dorian citizen minority's grip on conquered Messenian land and its own internal peace: producing unbeatable hoplites, preventing the wealth rivalry and faction that tore apart other Greek cities, and binding the whole order to an authority no local faction could claim for itself.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the liberated Messenian communities (who rebuilt Messene in 369 on their ancestral land), the Theban victors who broke the phalanx at Leuctra, and fourth-century analysts writing without Spartan allegiance — Aristotle's Politics and the closing chapters of Xenophon's Constitution of the Lacedaemonians — all treat the original problem (holding Messenia, hoplite supremacy) as dissolved while the forms persisted.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored by this reading's own lights over the fixed referent of the standing arrangement: early in the interval the costs borne (citizen austerity, helot delivery quotas) are classified as ordained duty rather than rent-taking, holding base extractiveness near 0.46; as the interval proceeds the arrangement increasingly takes without delivering the sacred order it promises — gold enters after the imperial victory, land concentrates, the citizen rolls shrink — and even pious accounting registers the shortfall as extraction, ending at 0.64. Suppression (0.86) is a raw structural property, unscaled by power or scope: the upbringing, the overseers' surveillance, the secret night raids, the annual war declaration, the periodic expulsion of foreigners, and the absence of any written text to argue from together constitute saturation-level coercive infrastructure. Theater_ratio (0.41 at end) tracks the late-interval shift from functional sacred order to performed sacred order — the founder's name invoked ever more frequently precisely as practice departs from his rules. Accessibility_collapse (0.62) sits deliberately below the natural-law range: alternatives did not collapse spontaneously upon understanding, they had to be fenced out by expulsion and information control, which is itself evidence against the mountain claim. Resistance (0.42) reflects recurrent helot risings — individually powerless, the bound population repeatedly achieved coalition scale, and the 464 revolt nearly ended the city — plus the emergence of internal conspiracy by 397. All three tracked series run on one shared nine-point grid; the trajectories are monotonic drift, not oscillation, so no cycle documentation is required.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine should register it. From the elder and citizen seats the arrangement is received perfection administered faithfully — an experience nearer the certified-natural end. From the helot seat the identical structure is an annual terror with a fixed harvest quota — an experience at the full-target end. From the Delphi seat it is one patronage relationship among hundreds, cheap to abandon. From the theorist seat it is a design to be scored. Same structure, four experienced types; the per-seat computation from power, exit, and role data is what turns this divergence into measurement rather than anecdote.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations cluster the derivation at low d: the citizen body collects the surplus, the elders collect interpretive authority, the royal houses collect command legitimacy, and Delphi collects patronage with multi-city arbitrage keeping its d nearest the beneficiary pole of any seat. Victim declarations push the complementary seats toward full target: helots (trapped, powerless) sit at the extreme; deviant citizens sit high despite insider status because identity lock converts membership itself into liability; perioikoi sit mid-high, paying dues and blood for no voice. One structural nuance the raw declarations understate: the citizen body's d is pulled up from pure-beneficiary territory by the same identity lock that secures its benefit — total-life conscription, wealth prohibition, and the forfeit-all exit tax its position. Its net position is therefore mid-low rather than near zero, and no override is authored because the beneficiary/victim plus exit data already carry this; the engine's derivation is trusted where the structural data is rich.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — hold Messenia, prevent faction, produce unbeatable hoplites — dies inside the modeled interval: Messenia is liberated in 369, the citizen rolls collapse, the phalanx is broken at Leuctra. The arrangement persists in sacral form anyway, which is why founding_problem_status=dead is paired with disappearance_verdict=world_rearranges: the zombie signature, cross-checked against the theater series climbing from 0.12 to 0.41. The reading's own apparatus guards against mislabeling in both directions: the genuine coordination achievements (roughly four centuries without the stasis that destroyed other poleis, uniform warrior production at scale) keep this story from collapsing into a pure-extraction caricature, while the named victims and the enforcement inventory keep it from passing as natural law. The mandate outlived its function before the interval closes; what remains is maintenance of the form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_warrant_vs_constructed_order,
    'Is the Lycurgan order a genuine transcendent ordinance — received, not made, and therefore mountain-like — or a constructed political arrangement whose sacral framing concentrates benefits on identifiable agents (the citizen class, the elder council, the Delphic sanctuary)?',
    'Comparative constitutional analysis across archaic Greece: whether poleis with openly revisable law suffered the factional collapse the fixity is credited with preventing, combined with historical-philological tracing of the rhetra''s formation and the documented folding of assembly amendments back into the oracle''s voice.',
    'If the constructed reading wins, the mountain claim collapses and the false-summit signature stands — the arrangement reclassifies toward an enforced hybrid with named beneficiaries and victims; if the warrant is genuinely received, low-extraction certification becomes defensible and the sibling deflationary readings lose their footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_warrant_vs_constructed_order, conceptual, 'Natural-law versus constructed-order ambiguity in the sacral framing of the Spartan constitution.').

omega_variable(
    decline_attribution_dispute,
    'Is Spartan decline attributable to the arrangement''s unrevisable design (the demographic-trap attribution) or to external shocks and citizen vice, as this reading holds?',
    'Counterfactual comparative analysis: did comparably rigid constitutions fail on similar timelines, did flexible ones survive analogous shocks, and does the timing of citizen-population collapse track the rigidity itself or the exogenous variables (earthquake, plague-era warfare, imperial gold influx) this reading blames?',
    'If design-attribution prevails, this reading''s core explanatory move fails, the sibling demographic_trap_reading gains, and the classification shifts toward a decay profile in which zero revision capacity is liability rather than virtue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_attribution_dispute, empirical, 'Causal attribution of Spartan decline: system design versus external pressure and citizen vice.').

omega_variable(
    amendment_absorption_mechanism,
    'Did the arrangement systematically undergo covert revision — assembly amendments recast as further words of the god — such that the immutability doctrine was always partly fictional?',
    'Textual comparison of rhetra variants (the Tyrtaios-era formula against the Plutarchic version shows the elder-council rider appended), plus dating of institutional additions (the overseership''s contested origin) against the doctrine''s no-change claim.',
    'If absorption was systematic, the immutability metrics are substantially theater, the epsilon-invariance of this reading breaks, and the sibling adaptive_fiction_reading becomes the better-fit instantiation of the same kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_absorption_mechanism, empirical, 'Whether covert amendment-under-denial was a routine operating mode of the unwritten constitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_sacral_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lycurgan_sacral_tr_t5, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(lycurgan_sacral_tr_t10, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(lycurgan_sacral_tr_t15, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement(lycurgan_sacral_tr_t20, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(lycurgan_sacral_tr_t25, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(lycurgan_sacral_tr_t30, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement(lycurgan_sacral_tr_t35, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 35, 0.33).
narrative_ontology:measurement(lycurgan_sacral_tr_t40, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(lycurgan_sacral_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(lycurgan_sacral_be_t5, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(lycurgan_sacral_be_t10, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(lycurgan_sacral_be_t15, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(lycurgan_sacral_be_t20, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(lycurgan_sacral_be_t25, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 25, 0.53).
narrative_ontology:measurement(lycurgan_sacral_be_t30, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(lycurgan_sacral_be_t35, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 35, 0.6).
narrative_ontology:measurement(lycurgan_sacral_be_t40, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 40, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_sacral_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(lycurgan_sacral_su_t5, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(lycurgan_sacral_su_t10, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement(lycurgan_sacral_su_t15, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(lycurgan_sacral_su_t20, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(lycurgan_sacral_su_t25, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(lycurgan_sacral_su_t30, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(lycurgan_sacral_su_t35, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 35, 0.84).
narrative_ontology:measurement(lycurgan_sacral_su_t40, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (lycurgan_laws), three readings emitted as separate stories per the epsilon-invariance principle — the colloquial label 'the Lycurgan constitution' covers structurally distinct claims that must not share one epsilon. This reading asserts the warrant is real (mountain claim, theological axioms, decline blamed on vice and shock); demographic_trap_reading treats unrevisability as fatal design flaw (decay-profile expectations); adaptive_fiction_reading treats immutability as cover for covert revision (theater-forward expectations). Each carries its own epsilon, beneficiary/victim structure, and claimed type. Edges run from this story to both siblings because its stability doctrine — the four-hundred-year fixity credit — is the upstream claim both siblings attack; contamination propagates downstream if this reading's purity degrades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
