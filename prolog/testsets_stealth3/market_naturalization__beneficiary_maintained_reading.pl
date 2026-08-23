% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Actively Defended Market Dominance (Beneficiary-Maintained Reading)
 *   domain: political economy / economic history / institutional analysis
 *
 * SUMMARY:
 *   A standing regime in which market-dominant positions are preserved not by
 *   natural equilibrium but by continuous expenditure: lobbying and campaign
 *   finance, litigation against challengers, acquisitions of nascent rivals,
 *   exclusive-dealing and standards control, and a fiscal channel of
 *   subsidies and asymmetric enforcement. The regime's public justification —
 *   that markets naturally reward the efficient, so dominant positions
 *   reflect merit — is the kernel under contest; this file instantiates the
 *   beneficiary_maintained_reading, which holds the justification is cover
 *   for rent defense. Time mapping for measurements: t=0 is 1980, t=46 is
 *   2026; the interval spans the neoliberal consolidation through the
 *   contemporary antitrust revival. CONSTRAINT FAMILY (per the ε-invariance
 *   principle): the colloquial label 'market dominance persists naturally'
 *   decomposes into three readings with different ε. This file authors the
 *   actively-defended pole (ε≈0.78: rents plus enforcement costs borne by
 *   identifiable payers). The lapsed_alternative_reading authors the inertial
 *   pole (dominance persisting unattended, low active extraction); the
 *   hybrid_reading authors the partitioned middle. Each is a separate story
 *   with its own beneficiaries and stakeholders; they are linked via
 *   network.affects_constraints, not merged. KEY AGENTS (by structural
 *   relationship): - incumbent_capital_holders: primary beneficiary and de
 *   facto agenda-setter ([institutional]/[arbitrage]) — finances and steers
 *   the maintenance machinery while collecting the rents -
 *   captured_enforcement_agencies: administering agenda-setter
 *   ([institutional]/[constrained]) — runs permissive review under
 *   donor-shaped mandates - dominance_defense_professionals: fee-collecting
 *   beneficiary ([powerful]/[mobile]) - would_be_entrants: primary target
 *   ([moderate]/[constrained]) — blocked or bought at the gate -
 *   monopsonized_workers: primary target ([powerless]/[constrained]) — wage
 *   suppression with thin outside options, coalition-capable only
 *   episodically - captive_consumers: target with incidental benefit
 *   ([organized]/[constrained]) - taxpayers: target via the fiscal channel
 *   ([organized]/[trapped]) - antitrust_reform_movements: excluded voice,
 *   partially admitted late-interval ([organized]/[constrained]) -
 *   institutional_economists: analytical observer — sees the full maintenance
 *   ledger
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.8).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, snare).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Actively Defended Market Dominance (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political economy / economic history / institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '68b2aab3-5082-4c57-83e6-076e37aab335').
narrative_ontology:cs_kernel_codification('68b2aab3-5082-4c57-83e6-076e37aab335', distributed).
narrative_ontology:cs_authority_grounding('68b2aab3-5082-4c57-83e6-076e37aab335', distributed).
narrative_ontology:cs_reading_relation('68b2aab3-5082-4c57-83e6-076e37aab335', market_naturalization__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_reading_relation('68b2aab3-5082-4c57-83e6-076e37aab335', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('68b2aab3-5082-4c57-83e6-076e37aab335', foundational, dominance_requires_continuous_expenditure).
narrative_ontology:cs_axiom_status(dominance_requires_continuous_expenditure, holdable).
narrative_ontology:cs_axiom_grounding('68b2aab3-5082-4c57-83e6-076e37aab335', dominance_requires_continuous_expenditure, empirically_contingent).
narrative_ontology:cs_axiom('68b2aab3-5082-4c57-83e6-076e37aab335', foundational, positional_advantage_is_barrier_not_merit).
narrative_ontology:cs_axiom_status(positional_advantage_is_barrier_not_merit, holdable).
narrative_ontology:cs_axiom_grounding('68b2aab3-5082-4c57-83e6-076e37aab335', positional_advantage_is_barrier_not_merit, empirically_contingent).
narrative_ontology:cs_axiom('68b2aab3-5082-4c57-83e6-076e37aab335', secondary, maintenance_cost_is_rent_financed).
narrative_ontology:cs_axiom_status(maintenance_cost_is_rent_financed, holdable).
narrative_ontology:cs_axiom_grounding('68b2aab3-5082-4c57-83e6-076e37aab335', maintenance_cost_is_rent_financed, empirically_contingent).
narrative_ontology:cs_reference_frame('68b2aab3-5082-4c57-83e6-076e37aab335', actively_defended_dominance_arrangement).
narrative_ontology:cs_drift_state('68b2aab3-5082-4c57-83e6-076e37aab335', contemporary_neobrandeisian_revival, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('68b2aab3-5082-4c57-83e6-076e37aab335', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, dominance_defense_professionals).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, would_be_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, monopsonized_workers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, captive_consumers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, captive_consumers).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, market_naturalization_doctrine).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, meritocratic_market_outcome_theory).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, consumer_welfare_antitrust_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and control the dominant firms in concentrated sectors: platforms, finance, pharmaceuticals, agribusiness, defense contracting. They finance the machinery that keeps challengers out — lobbying, campaign contributions, litigation against entrants, acquisitions of nascent rivals, and funding for the policy institutes that supply the intellectual case for laissez-faire. Their returns depend on their positions remaining uncontestable; they can move capital across jurisdictions and asset classes faster than any challenger can build scale, so abandoning a losing position costs them little even while the position itself is defended absolutely.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter).

% Staff and run the agencies formally charged with policing concentration — antitrust divisions, sector regulators, securities bodies. Day to day they negotiate consent decrees, review mergers using industry-supplied data, and rotate personnel through the industries they oversee. Budgets and statutory mandates are set by legislators responsive to the same donors. Their record across the interval shows long stretches of permissive review punctuated by episodic crackdowns.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, captured_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Antitrust bar partners, lobbying shops, economic consultancies, and academics-for-hire paid to produce the filings, studies, and testimony that justify incumbent positions. They collect fees regardless of outcome, and their livelihood depends on the contests continuing rather than on any side winning. Movement between defending incumbents and joining them runs through a well-worn revolving door.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, dominance_defense_professionals, beneficiary,
    powerful, biographical, mobile, national).

% Startups and small firms attempting to enter concentrated markets. They meet acquisition-or-crush offers, exclusive-dealing locks on suppliers and distributors, standards bodies controlled by incumbents, and financing gates where lenders prefer incumbency. The realistic paths are selling out early to a dominant buyer or confining themselves to niches too small for incumbents to notice.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, would_be_entrants, payer,
    moderate, biographical, constrained, national).

% Work in labor markets served by one or a few large employers — hospital systems, logistics giants, company towns, franchise networks. Wage growth trails productivity; non-compete clauses and licensure requirements limit movement; switching employers often means relocating. Collective organization has been episodically effective at history's turning points but faces persistent legal and logistical headwinds.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, monopsonized_workers, payer,
    powerless, biographical, constrained, regional).

% Buy from dominant providers because integration, switching costs, and ecosystem effects make alternatives impractical — app stores, broadband monopolies, airline hubs, retail banking. They pay above-competitive prices wrapped in bundled conveniences that make the premium hard to see, and they organize only sporadically through class actions and consumer campaigns.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, captive_consumers, payer,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, captive_consumers, beneficiary).

% Fund the subsidies, procurement premiums, bailout backstops, and the public half of the enforcement asymmetry — agencies resourced to pursue petty fraud while complex corporate structures go unaudited. Formal sovereignty through the vote translates weakly into policy: the arrangement's benefits are concentrated enough to defend politically while its costs stay diffuse enough to escape electoral punishment. Leaving means emigrating, which is prohibitive for nearly everyone.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, taxpayers, payer,
    organized, biographical, trapped, national).

% Neo-Brandeisian legal scholars, union federations, small-business coalitions, and open-source advocates pressing to revive structural antitrust and public options. Outside the formal conversation for most of the interval, they gained agency posts, hearings, and legislative sponsors only in the closing years. They operate on a small fraction of the defenders' budgets and face retaliatory litigation and primary challenges.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, antitrust_reform_movements, excluded,
    organized, generational, constrained, national).

% Academic economists and business historians measuring concentration, markups, entry rates, and mobility across the interval. They produce the evidentiary record that both camps cite, hold no enforcement power, and collect nothing from the arrangement's operation.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, institutional_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement does solve real problems of industrial scale: large-batch production, standardized products, deep distribution networks, liquid counterparties, and single-point accountability for quality and safety. Stated without evaluation: these coordination functions are performed, and they are the content invoked to justify the arrangement. This reading holds they ride atop, and are used to shield, a separate dominance-maintenance layer whose output is the suppression of alternatives.
% TRANSFER_FUNCTION: Moves surplus from four sources — entrants (blocked at the gate or bought on unfavorable terms), workers (wages suppressed below marginal product in thin labor markets), consumers (prices above competitive levels), and taxpayers (subsidies, bailouts, and asymmetric enforcement) — to incumbent capital holders as durable positional rents, less the operating cost of the defense machinery that keeps the flow intact.
% ABSENT_VOICES: Would-be competitors who cannot afford a seat in the policy process, communities dependent on single dominant employers, consumers who experience the arrangement as normality rather than choice, and future entrants not yet born. Reform movements sat outside the room for roughly thirty-five of the forty-six years; their late partial admission marks the exclusion weakening, not ending.
% DISAPPEARANCE_RATIONALE: Entry would surge within quarters: the acquisition-or-crush playbook loses its financier, exclusive-dealing and standards locks become litigable, and financing gates reopen. Prices and wages in concentrated markets converge toward competitive levels as margins compress; the defense professions lose their client base; the fiscal channel — subsidies, backstops, enforcement asymmetry — closes. Capital does not vanish; it redeploys toward competitive returns. But the positional-rent stream and the machinery that pumps it disappear together, which is the signature of an arrangement the world actively maintains rather than one reality imposes.
% FOUNDING_PROBLEM: Layered rather than singly founded: trust formation after the Gilded Age, the post-New Deal accommodation of regulated oligopoly, and the post-1980 deregulatory settlement each consolidated a version of the arrangement. Its stated problem: securing efficient industrial scale, stable investor returns, and systemic stability against destructive competition and expropriation. This reading holds the problem actually solved, continuously and deliberately, is securing positional rents against entry.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the benefiting parties but splits along the kernel fault line: business-history scholarship on the deliberate construction of entry barriers (pooling agreements ending railroad rate wars, the documented regulatory-capture record), Stigler-lineage capture studies, FTC and OECD retrospective merger reviews, and the markup-concentration literature attest that active defense is real and ongoing. Incumbent-adjacent policy institutes attest the founding problem is live — scale, they argue, still needs protecting. No adjudicator outside the contest settles which attestation governs; that unresolved contest is itself the kernel.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the transfer is decoupled from any service rendered at the margin: rents accrue from position, and the defense budget is itself paid out of those rents. Suppression (0.80) is the load-bearing number for this reading — the arrangement's persistence depends on actively excluding entry routes, not on participant preference — and it is authored as a RAW structural property: unlike extractiveness it is not scaled by directionality or scope anywhere downstream. Theater ratio (0.38) is moderate: the scale economies and safety functions are real, but a growing share of activity is performative maintenance — meritocracy narratives, 'competition is one click away' testimony, compliance programs that document rather than prevent. Accessibility collapse (0.62): once the maintenance structure is understood, alternatives do not fully collapse — disruptive breakouts happen, which is precisely why they are celebrated as miracles — but the standard routes (compete on price, scale organically, litigate access) close off for most entrants most of the time. Resistance (0.55): substantial and rising late-interval (Populist and New Deal precedents, then the neo-Brandeisian revival), historically intermittent. MEASUREMENTS run on one shared grid (t = 0,7,14,21,28,35,42,46) with all three tracked metrics authored at every point, per the alignment rule; points through t=42 carry basis 'observed', the t=46 endpoints are 'projected'. The base_extractiveness series rises monotonically as enforcement asymmetry matured and rents compounded; the suppression_requirement series traces the enforcement-capacity ratchet (Bork-era doctrinal consolidation, merger-wave tolerance, platform lock-in) flattening only at the very end as reform pressure begins straining the machinery — that trajectory is the story's enforcement-history signal and is authored deliberately, not defaulted from the scalar. Theater dips at t=28 (2008) because the crisis made the machinery briefly visible and functional — bailouts are operations, not rituals — before performative maintenance resumed its climb. Identity-lock note: the binding mechanism on the agency seat is career-path dependence (the revolving door fuses regulators' professional futures with the industry's self-description); breaking that frame — a hard door ban — is the single lever that would most quickly lower measured suppression requirement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the capital seat the same ledger reads as legitimate return on risk-bearing and the maintenance spend as ordinary business expense — a subsidized, arbitrage-mobile seat with generational horizon sees no extraction at all. From the entrant and worker seats the identical flows read as blocked opportunity and suppressed wages — constrained, biographically-horizoned seats bear the full directed cost. The agency seat experiences its own conduct as prudential administration under statutory constraint, not as enforcement of anyone's rents. The consumer seat is genuinely dual: payer of the premium, recipient of the bundle. These divergences are the measurement the per-seat computation exists to take; the authored claimed_type does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map onto the seats as follows. incumbent_capital_holders sit nearest the beneficiary pole: declared beneficiary, arbitrage-grade exit damps their effective burden toward zero, and their secondary agenda-setter role means they steer rather than endure the arrangement. dominance_defense_professionals are second-order beneficiaries — paid from the rent stream without owning the assets — so their derived directionality sits low but not as low as the principals'. captured_enforcement_agencies administer and directly absorb little of the cost; their capture binds discretion rather than transferring rents. The four payer seats sit near the full-target pole, ordered by exit: trapped taxpayers and regionally-constrained workers bear the highest effective burden; moderately-constrained entrants slightly less; ecosystem-constrained consumers least among the payers because of their incidental beneficiary position. antitrust_reform_movements are excluded rather than targeted — their exclusion is the enforcement surface itself. institutional_economists read near symmetric. No directionality_overrides are authored: the declaration set is rich enough that the derivation chain should locate every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy lens is what separates this file from its lapsed sibling. If the lapsed reading were correct — dominance as inertial residue — the arrangement would classify as degraded-but-cheap-to-fix, and its founding problem would be genuinely dead. This reading asserts the opposite: persistence tracks continuing expenditure, the founding problem functions as rhetorical cover that the rents themselves keep alive, and the arrangement therefore does not decay because decay is actively purchased against. Classifying it as a snare prevents the characteristic mandatrophy error of mistaking a maintained extraction regime for a vestigial one — the theater ratio is a symptom, but the cost-asymmetry test (fixing is prohibitive precisely because the capturer defends it) is the diagnosis. The analysis equally guards the mirror error: the coordination_function answer preserves the genuine scale-economy content from being read as nothing but extraction; whatever separable coordination residue exists is routed conceptually to the hybrid sibling via the omega variables rather than being silently averaged into this file's ε.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_status,
    'This story instantiates the beneficiary_maintained_reading of the market_naturalization kernel; is that the correct indexical reading for observed dominance persistence, or do the lapsed_alternative_reading or hybrid_reading capture more of the structure?',
    'Cross-sector process-tracing of maintenance expenditure (lobbying, litigation, acquisitions, standards control) against dominance duration, benchmarked against episodes where active defense demonstrably lapsed; adopt the reading whose causal share the record supports.',
    'A lapsed-majority finding demotes this constraint toward an inertial classification with thinning beneficiary structure and transfers epsilon to the lapsed sibling; a hybrid finding partitions epsilon between this file and the hybrid sibling and installs a genuine residual coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_status, empirical, 'Kernel-indexical uncertainty: which of the three readings of market_naturalization fits the causal record.').

omega_variable(
    active_maintenance_causal_share,
    'What fraction of observed market-dominance persistence is causally attributable to active defense versus unattended inertia (switching costs and network effects running on their own)?',
    'Jurisdictional natural experiments varying antitrust intensity and enforcement lapses; difference-in-differences on entry rates and price/wage margins around identifiable defense-spending shocks.',
    'Sets the epsilon partition between this reading and the lapsed sibling; a high active share confirms the foreclosing relation to the lapsed reading, a low share collapses it and moves this file toward the hybrid position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_maintenance_causal_share, empirical, 'Empirical partition of persistence into defended and inertial components.').

omega_variable(
    coordination_cover_separability,
    'Is the arrangement''s coordination content — scale economies, standard-setting, deep distribution — separable from the dominance-maintenance layer, or structurally fused with it?',
    'Examine sectors where maintenance mechanisms were stripped (mandated interoperability, structural divestitures) and test whether the coordination functions survived at lower concentration levels.',
    'Separable content keeps a genuine residual coordination function alive and pushes the classification toward a tangled-rope hybrid; fused content leaves this reading a clean pure-extraction classification and starves the hybrid sibling of material.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_cover_separability, conceptual, 'Whether the coordination story is cover or a detachable genuine function.').

omega_variable(
    suppression_mechanism_partition,
    'Is the measured suppression structural (legal, financial, and contractual barriers to entry and movement) or internalized (entrepreneurs, workers, and investors treating dominance as natural and self-limiting accordingly)?',
    'Post-liberalization trajectory: track entry-attempt rates, wage demands, and funding appetite after a barrier actually falls; persistence of self-limiting behavior after removal marks the internalized share.',
    'Internalized suppression travels with agents after exit and raises the constraint''s effective suppression above the structural measure; a purely structural reading predicts rapid behavioral response once barriers drop, which would revise the suppression attribution downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_partition, empirical, 'Partition of suppression into structural and internalized components.').

omega_variable(
    coalition_conversion_threshold,
    'Can the diffuse payer seats — workers, consumers, taxpayers — convert latent numbers into effective resistance, as in the Populist and New Deal episodes?',
    'Comparative analysis of mobilization episodes: organizational technology, media structure, and legal environment at moments when diffuse opposition did and did not coalesce into countervailing power.',
    'High conversion capacity raises the resistance trajectory and destabilizes the arrangement''s long-run persistence; low capacity locks in the current profile and reinforces the enforcement-asymmetry reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_conversion_threshold, empirical, 'Whether diffuse opposition can aggregate into countervailing power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 46).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t7, market_naturalization__beneficiary_maintained_reading, theater_ratio, 7, 0.26).
narrative_ontology:measurement_basis(mark_tr_t7, observed).
narrative_ontology:measurement(mark_tr_t14, market_naturalization__beneficiary_maintained_reading, theater_ratio, 14, 0.3).
narrative_ontology:measurement_basis(mark_tr_t14, observed).
narrative_ontology:measurement(mark_tr_t21, market_naturalization__beneficiary_maintained_reading, theater_ratio, 21, 0.33).
narrative_ontology:measurement_basis(mark_tr_t21, observed).
narrative_ontology:measurement(mark_tr_t28, market_naturalization__beneficiary_maintained_reading, theater_ratio, 28, 0.29).
narrative_ontology:measurement_basis(mark_tr_t28, observed).
narrative_ontology:measurement(mark_tr_t35, market_naturalization__beneficiary_maintained_reading, theater_ratio, 35, 0.34).
narrative_ontology:measurement_basis(mark_tr_t35, observed).
narrative_ontology:measurement(mark_tr_t42, market_naturalization__beneficiary_maintained_reading, theater_ratio, 42, 0.38).
narrative_ontology:measurement_basis(mark_tr_t42, observed).
narrative_ontology:measurement(mark_tr_t46, market_naturalization__beneficiary_maintained_reading, theater_ratio, 46, 0.38).
narrative_ontology:measurement_basis(mark_tr_t46, projected).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t7, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 7, 0.63).
narrative_ontology:measurement_basis(mark_be_t7, observed).
narrative_ontology:measurement(mark_be_t14, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 14, 0.67).
narrative_ontology:measurement_basis(mark_be_t14, observed).
narrative_ontology:measurement(mark_be_t21, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 21, 0.7).
narrative_ontology:measurement_basis(mark_be_t21, observed).
narrative_ontology:measurement(mark_be_t28, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 28, 0.73).
narrative_ontology:measurement_basis(mark_be_t28, observed).
narrative_ontology:measurement(mark_be_t35, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 35, 0.76).
narrative_ontology:measurement_basis(mark_be_t35, observed).
narrative_ontology:measurement(mark_be_t42, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 42, 0.78).
narrative_ontology:measurement_basis(mark_be_t42, observed).
narrative_ontology:measurement(mark_be_t46, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 46, 0.78).
narrative_ontology:measurement_basis(mark_be_t46, projected).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t7, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement_basis(mark_su_t7, observed).
narrative_ontology:measurement(mark_su_t14, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement_basis(mark_su_t14, observed).
narrative_ontology:measurement(mark_su_t21, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 21, 0.72).
narrative_ontology:measurement_basis(mark_su_t21, observed).
narrative_ontology:measurement(mark_su_t28, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 28, 0.74).
narrative_ontology:measurement_basis(mark_su_t28, observed).
narrative_ontology:measurement(mark_su_t35, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 35, 0.77).
narrative_ontology:measurement_basis(mark_su_t35, observed).
narrative_ontology:measurement(mark_su_t42, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 42, 0.79).
narrative_ontology:measurement_basis(mark_su_t42, observed).
narrative_ontology:measurement(mark_su_t46, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 46, 0.8).
narrative_ontology:measurement_basis(mark_su_t46, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel 'market naturalization'. The colloquial label 'market dominance persists naturally' conflates three structurally distinct claims: (1) this file, the beneficiary_maintained_reading — dominance as actively defended, high epsilon (~0.78), identifiable beneficiaries, active suppression; (2) the lapsed_alternative_reading — dominance as inertial residue requiring no maintenance, low active epsilon; (3) the hybrid_reading — a partitioned mixture. The upstream edge runs from the empirical record toward whichever reading it supports; this reading links to both siblings, forecloses the lapsed pole's causal claim while merely coexisting with the hybrid pole. Each member carries its own epsilon, beneficiaries, victims, and stakeholders per DP-001; the files are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
