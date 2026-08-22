% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default Constraint — Hybrid Amnesia Reading (Lapsed Closure Enabling Beneficiary Capture)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   The constraint under classification is the treatment of market allocation
 *   as the natural, default, inevitable mode of economic organization — the
 *   frame within which non-market arrangements (planning, cooperation, public
 *   provision) appear as eccentric exceptions rather than live options. This
 *   file instantiates ONE reading of the contested kernel
 *   market_as_natural_default: the hybrid amnesia reading, which holds a
 *   two-stage genesis. Stage one (c. 1930s-1970s) is genuine lapsed closure:
 *   the interwar and wartime visibility of alternatives faded from
 *   institutional memory without being refuted or suppressed — curricula
 *   narrowed, political platforms converged, and the mixed-economy
 *   settlement's own contingency stopped being taught. Stage two (c.
 *   1980s-present) is beneficiary capture of the resulting vacuum: the
 *   arrangement's beneficiaries inherited the amnesia and weaponized it,
 *   funding a defensive-rationalization apparatus that converted passive
 *   forgetting into active foreclosure. The ε referent is the standing
 *   arrangement — the naturalized market default as it actually operates —
 *   assessed by this reading's own lights; the reading's endorsed alternative
 *   (a contingency-aware, pluralist default) is a different arrangement and
 *   is not measured here. Claim and metrics are authored independently:
 *   claimed_type is tangled_rope because the reading's core assertion is
 *   hybridity (a real coordination function — a settled default grammar —
 *   plus real, rising, beneficiary-collected extraction); the metrics
 *   describe actual operation without being tuned to any predicted engine
 *   verdict. Sibling readings are separate constraints (see
 *   network.dual_formulation_note); nothing here hedges across them.
 *
 * KEY AGENTS:
 *   - incumbent_capital_owners: primary beneficiary turned agenda-setter (powerful/arbitrage) — inherited the amnesia, funds its weaponization, collects the principal rents
 *   - financial_sector_institutions: secondary beneficiary (institutional/arbitrage) — operates the naturalized allocation machinery and finances the rationalization apparatus
 *   - business_policy_advocacy_networks: active maintenance machinery (powerful/mobile) — produces the defensive naturalness discourse; agenda-setter with derivative beneficiary position
 *   - organized_labor: primary payer (organized/constrained) — bears foreclosed-bargaining costs; resists through politics against a funded framing apparatus
 *   - cooperative_and_alternative_economy_practitioners: payer (moderate/constrained) — pays a standing friction tax to exist as exceptions; keeps the forgotten tradition partly alive
 *   - heterodox_economic_scholars: payer (moderate/identity_locked) — keeps the memory of alternatives in circulation; pays in career terms
 *   - democratic_citizens: dual-positioned beneficiary/payer (moderate/constrained) — consumes default stability, pays in shrunken policy space
 *   - economic_historians: analytical observer (analytical/analytical) — the seat from which the two-stage genealogy is checkable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.55).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default Constraint — Hybrid Amnesia Reading (Lapsed Closure Enabling Beneficiary Capture)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '8753c35e-afe7-4104-a515-81dd563d2cbb').
narrative_ontology:cs_kernel_codification('8753c35e-afe7-4104-a515-81dd563d2cbb', implicit).
narrative_ontology:cs_authority_grounding('8753c35e-afe7-4104-a515-81dd563d2cbb', extraction).
narrative_ontology:cs_interpretation_layer_present('8753c35e-afe7-4104-a515-81dd563d2cbb').
narrative_ontology:cs_reading_relation('8753c35e-afe7-4104-a515-81dd563d2cbb', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('8753c35e-afe7-4104-a515-81dd563d2cbb', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('8753c35e-afe7-4104-a515-81dd563d2cbb', foundational, amnesia_precedes_and_enables_capture).
narrative_ontology:cs_axiom_status(amnesia_precedes_and_enables_capture, holdable).
narrative_ontology:cs_axiom_grounding('8753c35e-afe7-4104-a515-81dd563d2cbb', amnesia_precedes_and_enables_capture, empirically_contingent).
narrative_ontology:cs_axiom('8753c35e-afe7-4104-a515-81dd563d2cbb', foundational, inherited_amnesia_is_a_capture_resource).
narrative_ontology:cs_axiom_status(inherited_amnesia_is_a_capture_resource, holdable).
narrative_ontology:cs_axiom_grounding('8753c35e-afe7-4104-a515-81dd563d2cbb', inherited_amnesia_is_a_capture_resource, empirically_contingent).
narrative_ontology:cs_reference_frame('8753c35e-afe7-4104-a515-81dd563d2cbb', contingent_mixed_economy_settlement).
narrative_ontology:cs_drift_state('8753c35e-afe7-4104-a515-81dd563d2cbb', contemporary_neoliberal_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8753c35e-afe7-4104-a515-81dd563d2cbb', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_owners).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financial_sector_institutions).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, organized_labor).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, cooperative_and_alternative_economy_practitioners).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, heterodox_economic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, business_policy_advocacy_networks).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, democratic_citizens).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, democratic_citizens).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_naturalness_doctrine).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, spontaneous_order_theory).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, tina_no_alternative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the asset positions whose returns the naturalized default shields from political contestation. They did not create the amnesia — they inherited a discourse in which market allocation already read as the natural order — and from the 1980s onward they funded the apparatus (think tanks, endowed chairs, business press, lobbying) that converted passive forgetting into active foreclosure. Exit is arbitrage-grade: capital moves across jurisdictions and the default holds everywhere they might land, so they bear almost none of the constraint's costs while collecting its principal rents.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_owners, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_owners, agenda_setter).

% Operate the allocation machinery the default naturalizes. The assumption that markets allocate capital best forecloses credit-policy, public-investment, and cooperative-finance alternatives that would redirect their intermediation rents. They finance and staff much of the rationalization apparatus and experience the constraint as the ordinary weather of their industry.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financial_sector_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% Think tanks, business associations, op-ed economists, and framing-heavy media that produce the defensive rationalization: the 'economic realism' vocabulary, the naturalness rhetoric, the treatment of alternatives as eccentric. They collect funding, relevance, and careers from the maintenance role itself; their exit is mobile because the framing skill transfers to any client with an arrangement to defend.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, business_policy_advocacy_networks, agenda_setter,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, business_policy_advocacy_networks, beneficiary).

% Bears the costs of foreclosed alternatives: when wage boards, sectoral bargaining, public employment, or cooperative ownership leave the feasible set, labor's bargaining position weakens without any single decision being taken against it. Unions retain organizational power but cannot exit the economy whose default they contest; their resistance runs through politics and framing, where they face the funded rationalization apparatus.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, organized_labor, payer,
    organized, biographical, constrained, national).

% Run worker cooperatives, mutuals, community land trusts, and complementary currencies inside an institutional environment built for the market default: credit access, legal form, procurement rules, and legitimacy all price their arrangements as exceptions. They pay a standing friction tax on existing at all; exit would mean abandoning the practice their communities are built on, so they persist at the margin and keep the forgotten tradition partly alive.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, cooperative_and_alternative_economy_practitioners, payer,
    moderate, generational, constrained, regional).

% Keep the historical and theoretical memory of alternatives in circulation — the archives of interwar planning debates, the cooperative tradition, the comparative-allocation literature — from departmental margins. They pay in career terms: journal access, citation networks, hiring, and grant flows concentrate in the mainstream that treats the default as settled. Leaving the heterodox project would mean abandoning the intellectual identity the foreclosure defines, so their exit is identity-locked.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economic_scholars, payer,
    moderate, biographical, identity_locked, national).

% Consume the coordination benefits of a settled economic default — stable expectations, legible prices, no permanent constitutional convention over allocation — while paying the diffuse cost of a shrunken policy space: options their grandparents voted on no longer appear on any ballot. They neither run the constraint nor profit from it; their position is genuinely dual.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, democratic_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, democratic_citizens, payer).

% Reconstruct the sequence this reading turns on: when alternatives were live, when they faded from curricula and political platforms, and when defensive naturalness discourse first appears in the record. They collect nothing and pay nothing under the constraint; theirs is the seat from which the two-stage genealogy is checkable at all.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_capital_owners).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared default economic grammar: treating market allocation as the settled baseline spares each generation the cost of re-deciding the economy's allocation regime from scratch, stabilizes long-horizon investment and employment expectations, and gives institutional design a fixed starting point.
% TRANSFER_FUNCTION: Moves policy imagination and legitimacy from non-market arrangements (planning, cooperation, public provision) to market incumbents: as alternatives leave the thinkable set, the rents those arrangements would have contested accrue unexamined to capital owners and financial institutions, and the costs of foreclosed options fall on labor, alternative-economy practitioners, and citizens' future policy space.
% ABSENT_VOICES: The forgotten tradition itself: interwar planners, guild socialists, cooperative organizers, and the communities that practiced non-market allocation before it was rendered eccentric. They lost the argument's framing before the current parties entered it; their heirs — heterodox economists and cooperative movements — sit at the table's edge with marginal standing, and the economic historians who could speak for the record hold no vote in policy venues.
% DISAPPEARANCE_RATIONALE: If the naturalization vanished overnight, the feasible set would reopen: planning instruments, cooperative structures, and public provision would re-enter policy debate as live options rather than eccentricities; incumbents would have to defend market arrangements on their merits each budget cycle; economics pedagogy and media framing would reorganize around explicit comparison of allocation regimes. The world rearranges because the constraint's principal product is the closure of that set.
% FOUNDING_PROBLEM: After the Depression discredited laissez-faire and the war discredited command planning, mid-century societies needed a workable default allocation regime that would not have to be re-fought every budget cycle; the mixed-economy settlement provided one, and the naturalization emerged as memory of the settlement's own contingency faded.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and the Polanyi-scholarship tradition corroborate the two-stage genealogy from outside the beneficiary set: the archival record of the 1930s-40s planning and cooperation debates shows live alternatives that later faded without refutation, and heterodox economists attest the foreclosure's current operation. Beneficiary-funded policy institutions attest instead that the founding problem is live (markets need constant defense from political interference); the status is genuinely disputed between seats, and no beneficiary-independent source attests the 'still live' reading.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.45 at interval end) is moderate-to-substantial but bounded: the constraint's cognitive form makes its rents diffuse and partly invisible — no invoice is rendered for a foreclosed option — and the two-stage history means the arrangement spent the interval's first half near 0.20, before capture. The series dips at t=15 (c. 1950) because the constraint was least extractive when alternatives were still institutionally present in the mixed economy; the inflection at t=45 (c. 1980) marks the weaponization stage. Suppression (0.55) is epistemic rather than coercive — funding asymmetries, career gates, curricular narrowing, framing control marginalize alternatives without banning them — and is partly internalized (see omega suppression_structural_vs_internalized); it is authored as a raw structural property and left unscaled, per the suppression rule. Theater (0.40) reflects that the defensive rationalization is substantially performative — ritual invocations of economic necessity, natural-law rhetoric — sitting atop a genuinely operating allocation system; the theater is the maintenance cost of stage two, not the constraint's whole body. Accessibility collapse (0.50) is honest mid-range: alternatives are partly collapsed (forgotten at the level of ordinary discourse) but recoverable — the archives exist, surviving cooperative institutions embody them, and understanding the constraint is precisely what partially reopens them. Resistance (0.45) is persistent and occasionally surges (the post-2008 point, t=75) but is contained by the same apparatus that maintains the default. All three series share one time grid (t = 0, 15, 30, 45, 60, 75, 90, approximating 1935-2025 in 15-year steps) so the engine samples aligned rows; suppression_requirement is authored because the story's narrative specifically tracks enforcement-capacity change (passive amnesia maturing into active defensive machinery).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From incumbent_capital_owners the naturalization is simply the settled order of the world: they inherited it, their defense of it is experienced as defending reality, and their arbitrage exit means they never sit anywhere its costs land. From organized_labor and heterodox_economic_scholars the same structure is a maintained foreclosure — something that must be actively kept in place and that they pay for daily in bargaining position and career terms. democratic_citizens sit between: they receive the coordination benefit and pay the foreclosure cost without either appearing as such on any ledger, which is why their seat is dual-positioned. business_policy_advocacy_networks experience the constraint as a job: the maintenance is their function and funding. The engine computes per-seat classifications from these structural positions; the authored claim does not adjudicate between them — a beneficiary seat computing this as near-rope and payer seats computing it as snare-ward is the divergence the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the beneficiary end: the naturalization subsidizes capital owners and financial institutions by removing contestation from market-allocated rents, and their arbitrage-grade exit further damps effective extraction — they can always be where the default holds. Payers sit at the target end: organized_labor, cooperative practitioners, and heterodox scholars bear the foreclosed alternatives' costs with constrained or identity-locked exit, and identity lock (the scholars) sits them nearer the full-target end than mobility would. democratic_citizens are near-symmetric — genuine coordination benefit from a stable default, diffuse cost from the shrunken feasible set — so their d sits near the middle rather than at either pole. business_policy_advocacy_networks are agenda-setters whose secondary beneficiary role places them on the beneficiary side: they are funded by the extraction and collect from the maintenance itself. The structural derivation from these beneficiary/victim declarations and exit options produces the per-seat d values; no overrides were needed because every agent's position is captured by its declared role and exit class.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — running a complex economy without re-fighting the allocation regime every budget cycle, after the interwar collapse of both laissez-faire and command planning — was live and was genuinely solved by the mid-century settlement; the hybrid reading's mandatrophy claim is that the settlement's memory-lapse converted a solved problem into a defensive posture, so the constraint now maintains itself against a problem that changed shape. Classifying the constraint as pure snare would flatten this history into a cover story and erase the diagnostic that extraction ACCUMULATED rather than being designed in — the rising ε series is the fingerprint of capture of a pre-existing amnesia, not of an original extraction scheme. Classifying it as pure rope would miss the second stage entirely: the enforcement machinery, the funded rationalization, and the foreclosed policy space are real and growing. Tangled rope holds both halves, and the R5 mismatch consumer does the temporal work: founding_problem_status is contested (beneficiaries attest the problem is live; economic historians and the archival record attest the original problem was solved and what persists is weaponized amnesia), and the status-x-verdict mismatch against world_rearranges flags the capture. Fixing is authored prohibitive not because the information is expensive — the archives are cheap and extant — but because the seats that could institutionalize the fix (universities, media, parties) are funded and legitimized inside the constraint's own settlement, so the cost to them of de-naturalization exceeds the diffuse benefit they would collect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story instantiates the hybrid_amnesia_reading of the market_as_natural_default kernel. Is the two-stage hybrid decomposition correct — genuine forgetting followed by capture — or does one sibling reading fit the record better: lapsed_alternative_reading (forgetting alone, no weaponization) or beneficiary_maintained_reading (active defense throughout, no genuine amnesia stage)?',
    'Date the first appearance of defensive (as opposed to merely assumptive) market-naturalness discourse in business press, economics pedagogy, and party platforms relative to the documented fading of interwar planning and cooperative memory. If funded naturalness discourse predates the memory lapse, the hybrid collapses into beneficiary_maintained_reading; if no capture stage is detectable after the lapse, it collapses into lapsed_alternative_reading.',
    'The ε trajectory and enforcement structure change: pure lapsed closure implies low flat ε with no agenda-setter weaponization; pure active defense implies high ε from early in the interval with enforcement throughout; only the hybrid yields the rising 0.20-to-0.45 series with enforcement appearing at the t=45 inflection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, empirical, 'Whether the two-stage hybrid account or a single-cause sibling reading fits the historical record.').

omega_variable(
    two_stage_boundary_date,
    'When exactly did genuine forgetting end and defensive rationalization begin — is the t=45 (c. 1980) inflection the right boundary, or did weaponization start earlier (late-1960s business mobilization) or later (post-1989 triumphalism)?',
    'Discourse analysis of economics textbooks, chamber-of-commerce publications, and party manifestos across 1960-1995, coding invocations of market naturalness as assumptive versus defensive (defensive = responding to a live alternative proposal).',
    'Shifts the ε series'' inflection and the extraction-accumulation reading: an earlier boundary means the amnesia stage was shorter and capture began on a less-forgotten public (weakening amnesia''s enabling role); a later boundary extends the low-ε period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_stage_boundary_date, empirical, 'Dating the boundary between the genuine-forgetting stage and the weaponization stage.').

omega_variable(
    amnesia_vs_merits_refutation,
    'Were the non-market alternatives forgotten, or were they refuted on the merits (e.g., 1970s stagflation discrediting planning)? The hybrid reading requires that at least some alternatives lapsed without refutation — is that requirement met?',
    'Assess the evidential record per alternative family: did the discrediting evidence actually bear on cooperatives, industrial policy, or public provision as such, or was a failure of one form generalized to all non-market forms without test? The archival and citation record of the 1970s-80s policy debates adjudicates.',
    'If most alternatives were genuinely refuted, the constraint''s closure is partly epistemically warranted, suppression drops, and ε falls; if they lapsed unrefuted, the foreclosure is real cognitive loss and the authored suppression and ε hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amnesia_vs_merits_refutation, empirical, 'Whether alternatives were forgotten or refuted — the load-bearing empirical premise of the amnesia stage.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the current closure of alternatives structural (funding asymmetries, career gates, media ownership, curricular narrowing) or internalized (policy elites sincerely cannot conceive of non-market arrangements as feasible)?',
    'Post-disclosure update behavior: when credible evidence for a specific alternative is presented to elite audiences, do they update the feasibility assessment (internalized closure yields updates once the frame is named) or reject on institutional grounds (structural closure persists regardless of frame)? Pluralism-economics program outcomes and deliberative-polling evidence are the test beds.',
    'Internalized closure is stickier — it persists after structural barriers fall and raises effective suppression beyond the authored 0.55; structural closure is cheaper to fix (change funding and gates), which would bear on the authored prohibitive fixing_cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether the suppression of alternatives is structural, internalized, or both.').

omega_variable(
    beneficiary_sincerity_ambiguity,
    'Do the weaponizing beneficiaries sincerely believe market naturalness, or do they strategically deploy it? The hybrid reading''s stage two is compatible with both, but they classify differently.',
    'Revealed preference under cost: track whether beneficiaries abandon naturalness claims when the claims conflict with their interests — e.g., demanding state bailouts, tariffs, or public risk-bearing after market failures while continuing to fund naturalness discourse for others.',
    'Strategic deployment pushes the constraint snare-ward (the naturalness story is cover) and raises effective extraction for payer seats; sincere belief keeps it tangled_rope (a genuinely held cognitive constraint that nonetheless enables extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_sincerity_ambiguity, empirical, 'Whether stage-two defense is cynical rationalization or sincere conviction — the boundary between cover story and lived assumption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mnd_hybrid_amnesia_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_tr_t0, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_tr_t15, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_tr_t15, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_tr_t30, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_tr_t45, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 45, 0.24).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_tr_t45, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_tr_t60, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_tr_t60, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_tr_t75, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_tr_t75, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_tr_t90, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 90, 0.4).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mnd_hybrid_amnesia_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_be_t0, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_be_t15, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_be_t15, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_be_t30, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_be_t45, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 45, 0.3).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_be_t45, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_be_t60, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_be_t60, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_be_t75, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_be_t75, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_be_t90, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 90, 0.45).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mnd_hybrid_amnesia_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_su_t0, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_su_t15, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_su_t15, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_su_t30, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_su_t45, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 45, 0.35).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_su_t45, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_su_t60, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_su_t60, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_su_t75, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 75, 0.53).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_su_t75, observed).
narrative_ontology:measurement(mnd_hybrid_amnesia_su_t90, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 90, 0.55).
narrative_ontology:measurement_basis(mnd_hybrid_amnesia_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the market is treated as natural' decomposes under the ε-invariance principle into three mechanism-distinct constraints sharing one kernel (market_as_natural_default): the lapsed_alternative_reading (persistence by forgetting alone — implies low, flat ε and no enforcement machinery), the beneficiary_maintained_reading (persistence by active post-hoc defense — implies high ε from the start with enforcement throughout), and this hybrid_amnesia_reading (two-stage: genuine 1930s-1970s forgetting creating the vacuum, then 1980s-present defensive rationalization by beneficiaries — implies ε rising 0.20 to 0.45 with enforcement appearing only in the second stage). The siblings are upstream parameter constraints on this story: the dating evidence separating genuine amnesia from defensive discourse adjudicates between them, and this story's network edges record the family. Each file carries its own ε, beneficiaries, and victims; no story hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
