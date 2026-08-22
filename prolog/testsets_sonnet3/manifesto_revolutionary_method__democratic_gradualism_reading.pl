% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualist Reading of Revolutionary Method
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint instantiates the democratic-gradualism reading of the
 *   contested Manifesto-era question of revolutionary method: can
 *   working-class emancipation be achieved through winning electoral
 *   majorities and reforming existing state institutions from within, or does
 *   capitalist state power require rupture? Under this reading, socialism is
 *   treated as reachable via the ballot box, parliamentary
 *   coalition-building, and legally sanctioned union organizing — the Erfurt
 *   Program/Second International/postwar social-democratic trajectory. The
 *   reading names its own beneficiaries (parties and unions who built
 *   institutional power on this compact) and its own victims (militant
 *   currents disciplined out of the movement as 'adventurist' for pursuing
 *   extra-legal tactics). This is a distinct constraint from the
 *   vanguard_rupture_reading (party seizure of state power) and the
 *   council_communist_reading (workers' council federation) — those are
 *   separate files with their own ε, beneficiaries, and stakeholder sets;
 *   this reading does not average over them or hedge its ε against theirs.
 *
 * KEY AGENTS:
 *   - social_democratic_parties: agenda_setter/beneficiary (institutional/arbitrage) — sets strategy, collects institutional legitimacy
 *   - trade_union_bureaucracies: beneficiary/agenda_setter (organized/constrained) — depends on legal recognition within the existing order
 *   - rank_and_file_workers: beneficiary/payer (moderate/constrained) — gains incremental reform, bears cost of foreclosed transformation
 *   - revolutionary_militants: payer (powerless/trapped) — suppressed as adventurist, no institutional channel
 *   - wildcat_strike_organizers: payer (powerless/trapped) — disowned by official leadership for unauthorized action
 *   - capital_owning_class: excluded (powerful/arbitrage) — structurally shapes the terrain from outside the internal debate
 *   - historical_materialist_analysts: observer (analytical/analytical) — assesses whether gains survive capital's structural counter-pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.45).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualist Reading of Revolutionary Method").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '40f5a37e-2381-4485-a6ca-54637aea7c66').
narrative_ontology:cs_kernel_codification('40f5a37e-2381-4485-a6ca-54637aea7c66', distributed).
narrative_ontology:cs_authority_grounding('40f5a37e-2381-4485-a6ca-54637aea7c66', practice).
narrative_ontology:cs_interpretation_layer_present('40f5a37e-2381-4485-a6ca-54637aea7c66').
narrative_ontology:cs_reading_relation('40f5a37e-2381-4485-a6ca-54637aea7c66', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('40f5a37e-2381-4485-a6ca-54637aea7c66', manifesto_revolutionary_method__council_communist_reading, influences).
narrative_ontology:cs_axiom('40f5a37e-2381-4485-a6ca-54637aea7c66', foundational, state_apparatus_capturable_via_suffrage).
narrative_ontology:cs_axiom_status(state_apparatus_capturable_via_suffrage, holdable).
narrative_ontology:cs_axiom_grounding('40f5a37e-2381-4485-a6ca-54637aea7c66', state_apparatus_capturable_via_suffrage, empirically_contingent).
narrative_ontology:cs_axiom('40f5a37e-2381-4485-a6ca-54637aea7c66', secondary, extra_legal_rupture_invites_annihilating_repression).
narrative_ontology:cs_axiom_status(extra_legal_rupture_invites_annihilating_repression, holdable).
narrative_ontology:cs_axiom_grounding('40f5a37e-2381-4485-a6ca-54637aea7c66', extra_legal_rupture_invites_annihilating_repression, empirically_contingent).
narrative_ontology:cs_reference_frame('40f5a37e-2381-4485-a6ca-54637aea7c66', erfurt_program_parliamentary_road).
narrative_ontology:cs_drift_state('40f5a37e-2381-4485-a6ca-54637aea7c66', post_1980s_neoliberal_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40f5a37e-2381-4485-a6ca-54637aea7c66', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, parliamentary_left_officials).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, wildcat_strike_organizers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, extra_parliamentary_workers_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, rank_and_file_workers).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, rank_and_file_workers).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, peaceful_road_to_socialism_thesis).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, state_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets strategy for the labor movement around electoral competition, coalition government, and legislative reform. Controls party discipline, candidate selection, and the definition of acceptable tactics; disciplines or expels currents that pursue extra-parliamentary confrontation as 'adventurism.' Collects state resources, ministerial posts, and institutional legitimacy that flow from operating inside the parliamentary system.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, beneficiary).

% Negotiates collective bargaining agreements and legal recognition within the existing state framework. Depends on legal status, dues checkoff, and a seat at the negotiating table, all of which are conditioned on renouncing insurrectionary tactics. Benefits from stability and institutional continuity; would lose standing if the movement shifted toward direct confrontation with the state.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_bureaucracies, agenda_setter).

% Gains incremental wage gains, welfare provisions, and legal protections won through electoral and legislative channels. Also bears the cost of deferred or foreclosed transformation when reform stalls against capital's structural veto points (courts, central banks, capital flight); their exit from the gradualist strategy is constrained by lack of an organized alternative once militant formations have been marginalized.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, rank_and_file_workers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, rank_and_file_workers, payer).

% Argue that capital's structural power (over investment, courts, media, and the armed apparatus of the state) cannot be neutralized through electoral majorities alone and that gradualism systematically forecloses the moment of rupture. Face expulsion, marginalization, or public denunciation as 'adventurist' or 'sectarian' from party and union leaderships; have no institutional channel once excluded from party structures, and their tactics (wildcat strikes, workplace occupations) are treated as liabilities to be suppressed rather than resources to be coordinated.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, biographical, trapped, national).

% Initiate unauthorized workplace actions outside negotiated bargaining frameworks. Are frequently disowned by official union leadership seeking to preserve legal standing and bargaining relationships; face disciplinary action, legal liability, and loss of strike pay precisely because their action falls outside the sanctioned gradualist repertoire.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, wildcat_strike_organizers, payer,
    powerless, immediate, trapped, local).

% Not formally part of the labor movement's internal contest, but structurally shapes its terrain: capital flight, investment strikes, and control of courts and central banks constrain what an elected socialist majority can actually implement, which is precisely the vulnerability the vanguard and council-communist readings point to. Absent from the internal debate about tactics but decisive for whether the gradualist strategy can deliver its promise.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, capital_owning_class, excluded,
    powerful, generational, arbitrage, global).

% Assess whether electoral and legislative gains under this reading have historically survived capital's structural counter-pressure (coups, currency crises, capital strikes) or been rolled back, informing the broader kernel contest over revolutionary method.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, historical_materialist_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__democratic_gradualism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a mass working-class movement around a legal, electorally legible strategy that can win broad coalitions, avoid preemptive state repression, and accumulate institutional footholds (parties, unions, welfare provisions) that improve material conditions without triggering civil war or foreign intervention.
% TRANSFER_FUNCTION: Moves organizational discipline, tactical latitude, and legitimacy away from extra-parliamentary and insurrectionary currents and toward parliamentary parties and union leaderships; in return, moves incremental material concessions (wages, welfare, legal protections) from capital and the state toward the working class, mediated entirely through existing institutional channels.
% ABSENT_VOICES: Revolutionary militants and wildcat organizers who argue the strategy structurally cannot reach socialism because it never contests the state's coercive core are present in socialist discourse generally but are excluded from the leadership bodies (party congresses, union executive boards) that actually set gradualist strategy — they object from outside the room, not inside it.
% DISAPPEARANCE_RATIONALE: If the gradualist commitment (renunciation of extra-legal tactics, deference to electoral cycles, subordination of militant wings) disappeared overnight, social democratic parties and union bureaucracies would lose their basis for excluding and disciplining more militant currents; the labor movement's internal balance of power would shift toward whichever faction could organize direct action, and the parties/unions that built their institutional position on the gradualist compact would face a legitimacy crisis.
% FOUNDING_PROBLEM: Late 19th/early 20th century labor movements faced a strategic dilemma: revolutionary confrontation risked catastrophic repression (as seen in the Paris Commune's suppression and the Anti-Socialist Laws), while expanding suffrage offered a seemingly viable electoral path to state power without martyrdom. The gradualist reading was built to answer: how does the working class win power without being crushed before it can consolidate any gains?
% FOUNDING_PROBLEM_CORROBORATION: Social democratic parties and union leaderships attest the problem remains live and the strategy remains the only viable path absent revolutionary conditions. Independent historians of the German SPD's Erfurt-to-Weimar trajectory, and comparative analyses of Allende's Chile and Mitterrand's France, attest from outside both benefiting and victim camps that electorally-won socialist governments have repeatedly hit a structural ceiling (capital flight, judicial and military resistance, currency attacks) the gradualist framework does not have tools to overcome — suggesting the founding problem was addressed but a second, unaddressed problem (capital's non-electoral veto) was left unsolved.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.40 (moderate) per the expected structural delta: the reading genuinely delivers material coordination gains (wages, welfare, legal protection) but extracts organizational discipline and tactical latitude from militant currents, and the strategy's promise (achieving socialism, not merely ameliorating capitalism) has historically stalled against capital's non-electoral veto points, which the metric partially captures as rising extractiveness over the interval (0.22 to 0.40) as the gap between promised transformation and delivered reform widened. Theater ratio rises from 0.15 to 0.38 reflecting the growing gap between the stated revolutionary horizon ('socialism through the ballot box') and the actual steady-state function (managing capitalism, not superseding it) — a classic Goodhart-drift signature where the electoral-victory proxy substitutes for the substantive transformation goal. Suppression (0.45) reflects the disciplinary apparatus — expulsions, disavowals, denial of strike funds — used against militant currents, which is a raw structural property and is not scaled by scope in this authoring; the engine applies scope scaling only to extractiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the social democratic party and union bureaucracy seats, this reading is authentic coordination: a genuine, hard-won mechanism for improving material conditions without inviting the repression that crushed the Commune and the Spartacist uprising. From the revolutionary militant and wildcat organizer seats, the identical structure operates as an extraction and suppression mechanism — their tactical energy and organizational autonomy is confiscated and redirected into channels that structurally cannot reach the stated goal, while dissent is punished as sectarianism. The engine computes these divergent per-seat classifications from the declared power/exit/scope data; this commentary does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties and union bureaucracies are declared beneficiaries with institutional/organized power and arbitrage/constrained exit — they built durable institutional position on the gradualist compact and have low d (near the beneficiary end). Revolutionary militants and wildcat strike organizers are declared victims with powerless/trapped positioning — they bear the cost of exclusion and disciplinary suppression with no institutional exit, placing them near the full-target end of directionality. Rank-and-file workers occupy a genuinely mixed position (beneficiary + payer secondary role) reflecting that they receive real incremental gains while also bearing the opportunity cost of foreclosed transformation; this dual role is authored directly rather than resolved by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding the catastrophic repression that met earlier insurrectionary attempts — was genuinely live in 1889-1920 and the gradualist strategy solved it: electorally-organized labor parties survived and grew where insurrectionary movements were crushed (Paris Commune, Spartacist uprising). But the founding_problem_status is authored as contested rather than resolved-live, because the corroborating evidence (SPD/Weimar collapse, Allende's overthrow, Mitterrand's 1983 tournant de la rigueur under capital flight pressure) shows the strategy repeatedly hits a second, structurally distinct problem it was never built to solve: capital's non-electoral veto power. This classification prevents mislabeling the reading as pure extraction (it did solve its original problem, and continues to deliver real material coordination for rank-and-file workers) while also refusing to certify it as a clean Rope, because active enforcement against militant alternatives and a real victim class (revolutionary militants, wildcat organizers) are present — hence tangled_rope rather than rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gradualism_kernel_reading_scope,
    'Is the democratic-gradualism reading a genuinely distinct strategic claim from the vanguard-rupture and council-communist readings, or are all three simply different tactical emphases within one underlying commitment to working-class emancipation?',
    'This is routed as committer structure per Rule 2: the manifesto_revolutionary_method kernel is read three distinct ways (democratic_gradualism_reading, vanguard_rupture_reading, council_communist_reading), each instantiating a structurally different constraint with its own beneficiary/victim set and ε. The sibling readings are not folded into this file; each carries its own ε (this reading: 0.40 moderate, reflecting institutional continuity constraints on rapid transformation) and its own stakeholder surface. Resolution would require settling whether ''revolutionary method'' is a single contested empirical/strategic question or, as the framework treats it, three separate structurally distinct claims about how state power is transformed.',
    'If a future analysis determines the readings are not structurally separable (i.e., the same historical movements straddle gradualist and vanguardist tactics simultaneously such that they cannot be assigned distinct ε), this story and its siblings would need to be merged or the boundary redrawn — currently authored as three clean, ε-invariant constraints per the decomposition principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gradualism_kernel_reading_scope, conceptual, 'Whether the three kernel readings are genuinely distinct constraints or artificially separated facets of one strategic debate.').

omega_variable(
    capital_veto_as_second_founding_problem,
    'Was the gradualist strategy ever intended to solve the problem of capital''s non-electoral structural veto (investment strikes, capital flight, central bank and judicial resistance), or was that always outside its design scope?',
    'Close reading of foundational gradualist texts (Bernstein''s Evolutionary Socialism, Kautsky''s Erfurt commentary) against the historical record of electorally-won socialist governments confronting capital flight and constitutional/military resistance (Weimar, Chile 1973, France 1981-83).',
    'If the strategy was never designed to address capital''s structural veto, then its historical stalling is not evidence of failure on its own terms but evidence of an unaddressed second problem — supporting a ''contested'' rather than ''dead'' founding_problem_status. If it claimed to address this and failed, that supports reclassifying toward a higher extractiveness reading (closer to snare) where the electoral promise functions primarily as legitimation cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_veto_as_second_founding_problem, empirical, 'Whether capital''s structural veto power was within or outside the gradualist strategy''s original problem scope.').

omega_variable(
    suppression_mechanism_militants,
    'Is the suppression of revolutionary militants by party/union leadership primarily structural (formal expulsion, denial of resources, legal exposure) or partly internalized (militants accept the ''adventurist'' framing and self-censor tactics before any formal discipline occurs)?',
    'Comparative case study of party disciplinary proceedings against left factions (e.g., SPD vs. Spartacists, British Labour vs. Militant tendency) tracking whether expelled members continued organizing outside the party at rates consistent with pure structural exclusion, versus survey/interview evidence of internalized self-restraint prior to formal action.',
    'If suppression is substantially internalized, the effective suppression experienced by militant currents is higher than the structural measure (0.45) suggests, since ideological delegitimization operates even absent formal disciplinary action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_militants, empirical, 'Structural versus internalized suppression of militant currents within gradualist-led labor organizations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1889, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1889, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1889, 0.15).
narrative_ontology:measurement_basis(mani_tr_t1889, observed).
narrative_ontology:measurement(mani_tr_t1920, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement_basis(mani_tr_t1920, observed).
narrative_ontology:measurement(mani_tr_t1950, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1950, 0.26).
narrative_ontology:measurement_basis(mani_tr_t1950, observed).
narrative_ontology:measurement(mani_tr_t1975, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement_basis(mani_tr_t1975, observed).
narrative_ontology:measurement(mani_tr_t2000, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement_basis(mani_tr_t2000, observed).
narrative_ontology:measurement(mani_tr_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement_basis(mani_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(mani_be_t1889, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1889, 0.22).
narrative_ontology:measurement_basis(mani_be_t1889, observed).
narrative_ontology:measurement(mani_be_t1920, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement_basis(mani_be_t1920, observed).
narrative_ontology:measurement(mani_be_t1950, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement_basis(mani_be_t1950, observed).
narrative_ontology:measurement(mani_be_t1975, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement_basis(mani_be_t1975, observed).
narrative_ontology:measurement(mani_be_t2000, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement_basis(mani_be_t2000, observed).
narrative_ontology:measurement(mani_be_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement_basis(mani_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1889, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1889, 0.3).
narrative_ontology:measurement_basis(mani_su_t1889, observed).
narrative_ontology:measurement(mani_su_t1920, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement_basis(mani_su_t1920, observed).
narrative_ontology:measurement(mani_su_t1950, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement_basis(mani_su_t1950, observed).
narrative_ontology:measurement(mani_su_t1975, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement_basis(mani_su_t1975, observed).
narrative_ontology:measurement(mani_su_t2000, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement_basis(mani_su_t2000, observed).
narrative_ontology:measurement(mani_su_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement_basis(mani_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__democratic_gradualism_reading, 0.1).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, council_communist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the manifesto_revolutionary_method kernel. democratic_gradualism_reading claims moderate ε (0.40) reflecting genuine coordination gains constrained by structural limits on transformation; vanguard_rupture_reading and council_communist_reading are separate files with their own ε, beneficiary/victim sets, and classifications. The three are linked via affects_constraints because each reading's institutional success or failure changes the resource availability and legitimacy conditions for the others (e.g., gradualist failures historically fed vanguardist recruitment, and vice versa when vanguardist repression discredited insurrectionary tactics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__democratic_gradualism_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
