% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Setting as Hybrid Legitimacy Practice
 *   domain: science communication/normative epistemology/risk governance
 *
 * SUMMARY:
 *   This story instantiates the hybrid_legitimacy_reading of the
 *   doomsday_clock_metric kernel: the annual setting of the Bulletin of the
 *   Atomic Scientists' Doomsday Clock, read as a practice whose authority
 *   rests on the deliberate fusion of empirical judgment and normative
 *   witness — and whose refusal to specify what the hands measure is the
 *   load-bearing feature, not a defect. Per the epsilon-invariance principle,
 *   the colloquial label 'the Doomsday Clock' decomposes into three
 *   structurally distinct constraints: this reading (epsilon 0.52 — genuine
 *   attention coordination with an accountability void riding on it), the
 *   objective_index_reading (an auditable indicator composite, epsilon near
 *   the coordination floor), and the performative_tool_reading (strategic
 *   impact maximization, epsilon high with publics as targets). The three are
 *   linked through network edges, not averaged. Claim and metrics are
 *   independent authored facts: the claim states tangled_rope because the
 *   same ambiguity that synchronizes global attention also shields the
 *   setters from audit while accruing authority to them; the metrics describe
 *   the arrangement's actual operation. KEY AGENTS (by structural
 *   relationship): - bulletin_atomic_scientists: Agenda setter
 *   (institutional/identity_locked) — administers the annual setting; the
 *   seat the ritual's yields accrue to - bulletin_board_of_sponsors:
 *   Beneficiary (organized/mobile) — lends eminence, receives a platform -
 *   disarmament_advocacy_networks: Beneficiary (organized/constrained) —
 *   mobilization focal point - news_media_organizations: Beneficiary
 *   (organized/mobile) — reliable annual news peg -
 *   quantitative_gcr_researchers: Payer (moderate/constrained) — producers of
 *   the crowded-out alternative signal - policy_planning_communities: Payer
 *   with secondary beneficiary position (powerful/constrained) — consume the
 *   shorthand, bear miscalibration risk - methodology_reform_advocates:
 *   Excluded (moderate/constrained) — demand published criteria from outside
 *   the room - risk_governance_scholars: Observer (analytical/analytical) —
 *   see the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.52).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.48).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Setting as Hybrid Legitimacy Practice").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science communication/normative epistemology/risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '3f38932a-841a-4235-a773-0108df08f476').
narrative_ontology:cs_kernel_codification('3f38932a-841a-4235-a773-0108df08f476', implicit).
narrative_ontology:cs_authority_grounding('3f38932a-841a-4235-a773-0108df08f476', practice).
narrative_ontology:cs_interpretation_layer_present('3f38932a-841a-4235-a773-0108df08f476').
narrative_ontology:cs_reading_relation('3f38932a-841a-4235-a773-0108df08f476', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('3f38932a-841a-4235-a773-0108df08f476', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_axiom('3f38932a-841a-4235-a773-0108df08f476', foundational, normative_entanglement_irreducible).
narrative_ontology:cs_axiom_status(normative_entanglement_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('3f38932a-841a-4235-a773-0108df08f476', normative_entanglement_irreducible, deontological).
narrative_ontology:cs_axiom('3f38932a-841a-4235-a773-0108df08f476', secondary, ambiguity_preserves_cross_audience_legitimacy).
narrative_ontology:cs_axiom_status(ambiguity_preserves_cross_audience_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3f38932a-841a-4235-a773-0108df08f476', ambiguity_preserves_cross_audience_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('3f38932a-841a-4235-a773-0108df08f476', hybrid_expert_normative_witness).
narrative_ontology:cs_drift_state('3f38932a-841a-4235-a773-0108df08f476', contemporary_multi_domain_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f38932a-841a-4235-a773-0108df08f476', '2026-06-11T12:00:00Z').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_board_of_sponsors).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, disarmament_advocacy_networks).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, news_media_organizations).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, quantitative_gcr_researchers).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_planning_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, policy_planning_communities).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, expert_normative_discretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A small nonprofit founded by Manhattan Project physicists publishes a journal and convenes a Science and Security Board that meets each year to decide where the Doomsday Clock's hands stand. The announcement is the organization's principal public event: it draws global press coverage, drives subscriptions and donations, and supplies the organization's reason for being cited. Stepping away from the ritual would mean surrendering a public identity that has been fused with the clock for three generations.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists, beneficiary).

% Nobel laureates and prominent scientists who lend their names to the clock's announcements. Association with the ritual keeps their voices in the public square on issues they care about, and their presence lends the setting its aura of eminence. Withdrawal would cost them a ready-made platform, but they are individually free to step back.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_board_of_sponsors, beneficiary,
    organized, biographical, mobile, global).

% NGOs and campaign groups that organize their annual messaging around the clock announcement, using the setting as a hook for op-eds, fundraisers, and lobbying pushes. Other mobilization tools exist, but none arrives with comparable earned-media reach; their calendars and pitches are built around the January event.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, disarmament_advocacy_networks, beneficiary,
    organized, generational, constrained, global).

% Outlets that receive a reliable, visually striking annual story with built-in stakes and quotable experts. The event fills a slow-news month and editors plan around it. They bear no cost from the setting itself and can ignore it in any given year, though the habit is entrenched.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, news_media_organizations, beneficiary,
    organized, immediate, mobile, global).

% Academics and institutes producing probabilistic estimates of catastrophic and existential risk. Their careful, hedged publications compete for the same finite public and policymaker attention that the clock's single dramatic number captures each year. Grant committees and journalists repeatedly ask why their work lacks the clock's reach, and their attempts to correct the record rarely travel. They cannot leave the discourse niche, because the niche, not their research, is what is occupied.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, quantitative_gcr_researchers, payer,
    moderate, biographical, constrained, continental).

% Government planners, foreign-ministry analysts, and legislative staff who use the clock as a shorthand in briefings and speeches. The number saves them the work of commissioning bespoke risk assessment, but it arrives without published criteria, so citing it imports judgments no one can audit; treating it as analysis substitutes a symbol for calibration.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_planning_communities, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, policy_planning_communities, beneficiary).

% Critics, including former board participants and science-studies scholars, who argue the setting should publish its evidentiary criteria and uncertainty bounds. They sit outside the room where the setting is decided; their proposals are acknowledged in passing and not adopted, and their leverage is limited to public argument.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, methodology_reform_advocates, excluded,
    moderate, biographical, constrained, continental).

% Researchers studying how societies govern catastrophic risk who observe the clock as a case study in expert authority. They take no side in the setting, publish analyses of how the ritual sustains itself, and occupy the seat from which the full structure is visible.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, risk_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__hybrid_legitimacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes dispersed attention on existential risk: governments, advocacy groups, researchers, and journalists get one recurring, mutually expected moment and one shared unit ('minutes to midnight') around which statements, coverage, and mobilization can align, sparing each actor the cost of independently timing and framing risk communication.
% TRANSFER_FUNCTION: Moves public attention and epistemic deference toward the Bulletin and its sponsor network each year, and moves a compressed risk signal outward to publics and policymakers; the attention and deference come from the wider risk-assessment community and the news-consuming public.
% ABSENT_VOICES: Methodology reformers who demand published criteria sit outside the setting room, as do quantitative catastrophic-risk researchers whose probabilistic estimates compete with the clock for the same attention niche; non-Western security perspectives are thinly represented on the board. All would contest the weighting choices and the refusal to operationalize.
% DISAPPEARANCE_RATIONALE: The annual news moment, the advocacy mobilization calendar, and the shared rhetorical unit would vanish; rival indices and dashboards would compete openly for the vacated attention niche, and editorial routines built around the January announcement would reorganize.
% FOUNDING_PROBLEM: In 1947, scientists who had built the bomb possessed knowledge of nuclear peril with no channel to mass publics; the clock was designed to translate expert dread into an image a newspaper reader could grasp instantly.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the atomic period corroborate the original channel-starvation problem from outside the benefiting parties. Whether it remains live is disputed: risk-communication scholars describe today's environment as attention-saturated rather than channel-starved, while the Bulletin attests the problem persists in new technical domains; no party outside the beneficiary set independently attests the original problem's persistence in its original form.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater sits at 0.42 rather than higher because, under this reading, performance and function are fused: the annual staging is precisely how attention is synchronized, so much of what looks theatrical is doing coordination work; the residual is ritual maintenance that would persist even absent new information. Suppression (0.48) is discursive rather than coercive — the enforcement burden is the annual production apparatus, the defense of ambiguity against transparency demands, and the crowding-out of rival signals — and it is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. Accessibility collapse is low (0.30): alternative indices and primary analyses remain available and are periodically attempted. Resistance is substantial (0.55): the clock draws steady methodological criticism from researchers, former participants, and editorial writers, and has survived it. Fixing the accountability void would require the Bulletin to publish criteria and uncertainty bounds, dissolving the hybrid authority model that constitutes the organization — prohibitive for the only actor positioned to fix it, while external displacement faces collective-action costs. The measurement series run on one shared ten-point grid spanning 1947–2025 so every tracked metric is authored at every examined time point. The series show a cycle rather than monotonic drift: crisis proximity ratchets attention and enforcement up (1953, 1984, 2018–2025), détente and post-Cold-War relief let them decay (1963, 1991), and the 1991 trough approaches atrophy before domain expansion from 2007 onward converts each subsequent crisis into permanent scope growth. The oscillation is partly the mechanism itself — intermittent reinforcement in which each relaxation makes the next ratchet more credible — which is why the cycle is documented here rather than smoothed away.
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin's seat the arrangement is a witness institution it has stewarded for three generations, and the refusal to operationalize is intellectual honesty about irreducible judgment. From the researcher and planner seats the same ritual operates as an unauditable signal that crowds out calibrated work and substitutes symbolism for analysis. The excluded reformer seat experiences a closed door: proposals for published criteria are received without adoption. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin sits nearest the beneficiary pole: it receives the attention and deference the ritual generates and bears essentially none of its costs, and its exit is blocked by identity fusion with the clock rather than by external barrier. Sponsors and advocacy networks receive prestige and a mobilization focal point at low cost. Media organizations benefit incidentally as customers of the news peg. Quantitative risk researchers sit far toward the target pole: they supply the crowded-out alternative and bear the reach deficit, with constrained exit because the attention niche, not their research, is what is occupied. Planning communities sit mid-range: they receive a usable shorthand and bear the miscalibration risk of an unauditable number. The signal's spatial scope is global, which amplifies verification difficulty for anyone attempting to audit the setting.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1991 setting (seventeen minutes) marks the moment the founding function nearly atrophied: the Cold War frame dissolved, coverage thinned, and the ritual persisted largely on inertia — a piton-shaped interval. Instead of dying, the arrangement re-expanded: climate in 2007, then biosecurity, cyber, and artificial intelligence, each addition renewing the attention yield the original nuclear frame no longer supplied. Reading the clock as pure rope would miss this ratchet and the accountability void riding on it; reading it as pure snare would miss the genuine synchronization value and the absence of coercion. The founding problem — channel starvation between expert nuclear knowledge and mass publics — is plausibly dead in its original form while the arrangement thrives on successor problems, which is why the genealogy status is authored contested rather than resolved, and no mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the doomsday_clock_metric kernel; which reading a consumer adopts determines the constraint''s very structure — what would adopting the objective_index_reading or the performative_tool_reading change?',
    'Adoption of a sibling reading generates a separate constraint file with its own epsilon, beneficiaries, and victims; cross-reading comparison runs through the network edges, not through re-measuring this story.',
    'Under the objective_index_reading, epsilon collapses toward the coordination-cost floor (an auditable index with published methodology); under the performative_tool_reading, epsilon rises sharply and publics become the target seat. This story''s tangled_rope profile holds only under the hybrid reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one of three readings of the clock kernel; classification is reading-relative.').

omega_variable(
    entanglement_reducibility_test,
    'Could a published indicator-weighting scheme reproduce the board''s settings within a small tolerance, demonstrating that the normative component is eliminable after all?',
    'Retrospective fitting of open-source indicator composites (warhead counts, treaty status, biosecurity indices, AI incident rates) against the historical setting record; a tight fit with stable weights would favor reducibility.',
    'If reducible, the hybrid reading''s foundational axiom fails and the objective_index_reading becomes the accurate constraint; if irreducible, the accountability void is constitutive rather than accidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entanglement_reducibility_test, empirical, 'Whether the clock''s judgment is decomposable into auditable empirical indicators.').

omega_variable(
    accountability_void_bearing,
    'Does the unaccountable-authority accrual impose real costs on identifiable parties, or is it a diffuse epistemic externality with no bearer?',
    'Trace citation and briefing practices in policy institutions: measure instances where clock citations substituted for commissioned risk assessment, and survey affected planning staff.',
    'Identifiable bearers strengthen the victim structure supporting the tangled_rope profile; a purely diffuse externality would push the profile toward rope with reputational spillover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_void_bearing, empirical, 'Whether the accountability void has identifiable cost-bearers.').

omega_variable(
    domain_expansion_ratchet,
    'Is the post-2007 pattern — each crisis expanding the clock''s domain (climate, biosecurity, cyber, AI) with relaxations never restoring prior scope — an intentional survival strategy of the arrangement, or a good-faith response to genuinely widening risk?',
    'Board minutes, internal communications, and comparison of stated rationales for each domain addition against measured risk trends in that domain at the time of addition.',
    'If strategic, the oscillation is an intermittent-reinforcement mechanism and effective extraction is higher than the scalar suggests; if good-faith, the ratchet reflects real coordination demand.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(domain_expansion_ratchet, empirical, 'Whether crisis-driven domain expansion is survival strategy or substantive response.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(doom_tr_t0, observed).
narrative_ontology:measurement(doom_tr_t6, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement_basis(doom_tr_t6, observed).
narrative_ontology:measurement(doom_tr_t16, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement_basis(doom_tr_t16, observed).
narrative_ontology:measurement(doom_tr_t37, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 37, 0.22).
narrative_ontology:measurement_basis(doom_tr_t37, observed).
narrative_ontology:measurement(doom_tr_t44, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 44, 0.35).
narrative_ontology:measurement_basis(doom_tr_t44, observed).
narrative_ontology:measurement(doom_tr_t51, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 51, 0.3).
narrative_ontology:measurement_basis(doom_tr_t51, observed).
narrative_ontology:measurement(doom_tr_t60, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(doom_tr_t60, observed).
narrative_ontology:measurement(doom_tr_t68, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 68, 0.36).
narrative_ontology:measurement_basis(doom_tr_t68, observed).
narrative_ontology:measurement(doom_tr_t73, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 73, 0.4).
narrative_ontology:measurement_basis(doom_tr_t73, observed).
narrative_ontology:measurement(doom_tr_t78, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 78, 0.42).
narrative_ontology:measurement_basis(doom_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(doom_be_t0, observed).
narrative_ontology:measurement(doom_be_t6, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement_basis(doom_be_t6, observed).
narrative_ontology:measurement(doom_be_t16, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(doom_be_t16, observed).
narrative_ontology:measurement(doom_be_t37, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 37, 0.33).
narrative_ontology:measurement_basis(doom_be_t37, observed).
narrative_ontology:measurement(doom_be_t44, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 44, 0.2).
narrative_ontology:measurement_basis(doom_be_t44, observed).
narrative_ontology:measurement(doom_be_t51, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 51, 0.28).
narrative_ontology:measurement_basis(doom_be_t51, observed).
narrative_ontology:measurement(doom_be_t60, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(doom_be_t60, observed).
narrative_ontology:measurement(doom_be_t68, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 68, 0.44).
narrative_ontology:measurement_basis(doom_be_t68, observed).
narrative_ontology:measurement(doom_be_t73, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 73, 0.5).
narrative_ontology:measurement_basis(doom_be_t73, observed).
narrative_ontology:measurement(doom_be_t78, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 78, 0.52).
narrative_ontology:measurement_basis(doom_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(doom_su_t0, observed).
narrative_ontology:measurement(doom_su_t6, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 6, 0.24).
narrative_ontology:measurement_basis(doom_su_t6, observed).
narrative_ontology:measurement(doom_su_t16, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement_basis(doom_su_t16, observed).
narrative_ontology:measurement(doom_su_t37, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 37, 0.28).
narrative_ontology:measurement_basis(doom_su_t37, observed).
narrative_ontology:measurement(doom_su_t44, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 44, 0.1).
narrative_ontology:measurement_basis(doom_su_t44, observed).
narrative_ontology:measurement(doom_su_t51, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 51, 0.18).
narrative_ontology:measurement_basis(doom_su_t51, observed).
narrative_ontology:measurement(doom_su_t60, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement_basis(doom_su_t60, observed).
narrative_ontology:measurement(doom_su_t68, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 68, 0.38).
narrative_ontology:measurement_basis(doom_su_t68, observed).
narrative_ontology:measurement(doom_su_t73, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 73, 0.44).
narrative_ontology:measurement_basis(doom_su_t73, observed).
narrative_ontology:measurement(doom_su_t78, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 78, 0.48).
narrative_ontology:measurement_basis(doom_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'Doomsday Clock' per the epsilon-invariance principle: hybrid_legitimacy_reading (this file), objective_index_reading, and performative_tool_reading are separate constraints with distinct epsilon values, beneficiary structures, and failure modes. The hybrid reading is upstream of the performative reading in discourse: by legitimating ambiguity it raises the evidentiary bar the strategic-manipulation critique must clear. It stands in direct contradiction to the objective reading's reducibility premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
