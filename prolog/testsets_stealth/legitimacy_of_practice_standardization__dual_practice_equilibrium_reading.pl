% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual-Practice Equilibrium: Domain-Partitioned Practice Legitimacy
 *   domain: political_history/modernization_studies
 *
 * SUMMARY:
 *   A modernizing state (historical template: Meiji Japan, with close
 *   analogues in the Ottoman Tanzimat and late-Qing self-strengthening)
 *   adopts Western administrative forms — Gregorian calendar for taxation and
 *   bureaucracy, Western dress and office hours for officials and workers,
 *   standardized schooling and conscription — while leaving festivals,
 *   agrarian time, domestic dress, and household rite under traditional
 *   authority. The settlement is never codified as doctrine: it lives as
 *   practice, each authority sovereign in its own domain, compliance in the
 *   state's domain strategic rather than internalized. This file instantiates
 *   the dual_practice_equilibrium_reading of the
 *   legitimacy_of_practice_standardization kernel and authors epsilon for the
 *   standing partition arrangement as that reading sees it. Per the
 *   epsilon-invariance rule the kernel decomposes into three linked stories:
 *   the exogenous_override_reading authors the same arrangement as state
 *   imposition (higher epsilon), the endogenous_displacement_reading authors
 *   it as voluntary uptake (lower epsilon); this reading locates legitimacy
 *   in the partition itself and authors a middle, rising epsilon. The claimed
 *   type and the metrics below are authored independently: the claim states
 *   the structure judged true; the metrics state the operation judged
 *   descriptively accurate.
 *
 * KEY AGENTS:
 *   - modernizing_state_bureaucracy: Agenda setter and principal collector (institutional/arbitrage) — draws the public/private boundary, collects fiscal capacity, conscripts, and legibility, and can redraw the boundary at will.
 *   - traditional_ritual_authorities: Protected beneficiary (organized/constrained) — keeps the ritual domain, wholly dependent on the partition's persistence.
 *   - foreign_treaty_powers: External beneficiary (institutional/mobile) — collects credible-modernization performance without bearing its costs.
 *   - rural_households: Primary cost-bearer (powerless/trapped) — lives on two calendars, pays taxes and sons on state time.
 *   - urban_dual_domain_workers: Cost-bearer with incidental gain (moderate/constrained) — pays the double-life bill, profits from standardized commerce.
 *   - household_women: Concentrated cost-bearer (powerless/identity_locked) — the private domain runs on their unbought labor; exit costs identity.
 *   - pro_convergence_reformers: Excluded flank critics (moderate/constrained) — object from both directions, never seated at the bargain.
 *   - modernization_historians: Analytical observer (analytical/analytical) — sees the full structure from the archive.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.66).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.69).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.69).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual-Practice Equilibrium: Domain-Partitioned Practice Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/modernization_studies").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '1a1d737a-fce4-4112-bf81-c82ebb41655b').
narrative_ontology:cs_kernel_codification('1a1d737a-fce4-4112-bf81-c82ebb41655b', implicit).
narrative_ontology:cs_authority_grounding('1a1d737a-fce4-4112-bf81-c82ebb41655b', distributed).
narrative_ontology:cs_reading_relation('1a1d737a-fce4-4112-bf81-c82ebb41655b', legitimacy_of_practice_standardization__endogenous_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('1a1d737a-fce4-4112-bf81-c82ebb41655b', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_axiom('1a1d737a-fce4-4112-bf81-c82ebb41655b', foundational, public_domain_state_authority_supreme).
narrative_ontology:cs_axiom_status(public_domain_state_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('1a1d737a-fce4-4112-bf81-c82ebb41655b', public_domain_state_authority_supreme, conventional).
narrative_ontology:cs_axiom('1a1d737a-fce4-4112-bf81-c82ebb41655b', foundational, private_ritual_domain_traditional_authority_supreme).
narrative_ontology:cs_axiom_status(private_ritual_domain_traditional_authority_supreme, holdable).
narrative_ontology:cs_axiom_grounding('1a1d737a-fce4-4112-bf81-c82ebb41655b', private_ritual_domain_traditional_authority_supreme, conventional).
narrative_ontology:cs_axiom('1a1d737a-fce4-4112-bf81-c82ebb41655b', secondary, strategic_outward_compliance_validates_administrative_practice).
narrative_ontology:cs_axiom_status(strategic_outward_compliance_validates_administrative_practice, holdable).
narrative_ontology:cs_axiom_grounding('1a1d737a-fce4-4112-bf81-c82ebb41655b', strategic_outward_compliance_validates_administrative_practice, conventional).
narrative_ontology:cs_reference_frame('1a1d737a-fce4-4112-bf81-c82ebb41655b', bifurcated_authority_settlement).
narrative_ontology:cs_drift_state('1a1d737a-fce4-4112-bf81-c82ebb41655b', contemporary_mass_society, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a1d737a-fce4-4112-bf81-c82ebb41655b', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, foreign_treaty_powers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_households).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_dual_domain_workers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, household_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_dual_domain_workers).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, domain_partitioned_legitimacy).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, strategic_compliance_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the civil and administrative codes, fixes the fiscal year on the Gregorian calendar, prescribes Western dress and office hours for officials, schools, and the army, and collects the taxes, conscripts, and school attendance that the standard calendar makes schedulable. Funds and staffs the enforcement machinery and periodically renegotiates the boundary with ritual authorities when enforcement provokes unrest. Because it drew the boundary, it can move it — as the wartime state did when it reached into festivals and homes for mobilization.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Shrine and temple establishments, festival committees, and household ritual specialists who keep lunar reckoning for festivals and agrarian rites, traditional dress for ceremony and home, and the domestic rite cycle. The settlement leaves their sphere socially and legally intact, and their standing rests on that protection; if the state ever claimed the whole lifeworld they would face open competition they are organized to lose.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_ritual_authorities, beneficiary,
    organized, generational, constrained, national).

% Diplomatic and commercial partners who made visible administrative westernization the price of treaty revision and great-power recognition. They observe the public-domain performance — ministries, uniforms, fiscal punctuality — and extend recognition and trade accordingly, while paying none of the domestic costs of the arrangement and remaining free to redirect commerce elsewhere.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, foreign_treaty_powers, beneficiary,
    institutional, biographical, mobile, global).

% Farm by the lunar and agrarian calendar — planting, festivals, village rites — while owing taxes, school attendance, and military service on dates the state calendar fixes. Keep two reckonings running at once, petition when tax deadlines fall between harvests, and absorb the periodic taking of sons. Cannot leave the state's jurisdiction, and cannot drop the festival round without losing the village social order.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_households, payer,
    powerless, generational, trapped, national).

% Clerks, merchants, teachers, and factory hands who keep Western office hours and dress by day and return to festival, domestic, and neighborhood practice off the clock. Carry the wardrobe and scheduling costs of the double life, but also earn from what the state standard provides: uniform commercial time, enforceable contracts, portable credentials.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_dual_domain_workers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_dual_domain_workers, beneficiary).

% Dress the household in traditional clothing, prepare the festivals, maintain the domestic rites, and teach the old calendar to children — the protected private domain runs on their unbought daily labor. Their standing as wives, mothers, and daughters-in-law is built through this maintenance, so setting it down would cost them their place in the family and community, not merely convenience.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, household_women, payer,
    powerless, biographical, identity_locked, national).

% Journalists, scholars, and politicians who argue either that the split is hypocrisy preserving feudal remnants and the state should finish the job, or that tradition belongs in public life and its quarantine inside the home is a national humiliation. They publish, petition, and stand for office, but were never parties to the settlement, which was struck between state planners and the ritual establishments.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, pro_convergence_reformers, excluded,
    moderate, biographical, constrained, national).

% Reconstruct the settlement from ministry archives, prefectural petitions, and household records; measure where compliance was strategic and where it hardened into habit; compare the pattern across modernizing states. Hold no stake in the arrangement's persistence and can name its costs and benefits without belonging to either camp.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collision between the modernizing state's needs (fiscal legibility, conscription, schooling, treaty credibility) and the population's attachment to inherited practice, by allocating each sphere of life to the authority that governs it best: state standards bind in offices, courts, schools, factories, and tax schedules; traditional authority retains festivals, agrarian time, domestic dress, and household rite. Each side gets a domain; neither fights a total war over the whole lifeworld.
% TRANSFER_FUNCTION: Moves time-discipline, labor, and outward conformity from the population to the state in public domains (taxes reckoned on the state calendar, sons conscripted on the state schedule, bodies dressed to state code during work), and moves deference and maintenance labor to traditional authorities in private domains; the population itself absorbs the cost of living on both clocks at once.
% ABSENT_VOICES: Full-convergence advocates on both flanks: radical westernizers who read the partition as hypocrisy sustaining feudal remnants, and nativists who wanted traditional practice restored to public standing rather than quarantined in the home. Also absent: adherents of new religious and hybrid practices fitting neither domain, and colonial subjects governed by the same state under far less generous terms.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, the state would face the choice it originally avoided — compel full convergence, provoking the resistance that made the compromise necessary, or abandon standardization, losing fiscal and military legibility. Administrative time, taxation, and official conduct would fragment; traditional authorities would lose their protected sphere or be forced into open competition with the state.
% FOUNDING_PROBLEM: Treaty-port-era states needed fiscal-military capacity and great-power recognition quickly, but wholesale replacement of inherited practice exceeded administrative reach and risked revolt; the partition was built to obtain the state's minimum requirements without a frontal assault on the culture.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: treaty-power diplomatic correspondence attests both the original recognition problem and its solution by visible administrative westernization; prefectural petition records and riot-commission reports attest the costs borne by rural households; later historiography (Beasley, Gluck, Harootunian) analyzes the partition as a deliberate governing strategy rather than a natural division. None of these sources depends on the state's or the ritual establishments' self-account.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66, the interval-end value: the state's take grew with each addition to the public domain (taxation, conscription, schooling, factory discipline) and peaked when the wartime state suspended the partition and reached into festivals and homes. Suppression (0.69) is authored as a raw structural property, unscaled by power or scope: enforcement was always selective — heavy inside offices, courts, and barracks, light by design inside the home — and the endpoint value reflects the wartime breach of that selectivity. Theater ratio (0.38) carries two humps the series records: the Rokumeikan-era ceremonial westernization staged for foreign eyes, and wartime ritual mobilization; between them the arrangement ran as plain administration. Accessibility collapse is 0.45: systemic alternatives (a single unified legitimacy regime in either direction) were politically foreclosed, but domain-internal choice stayed wide open — that residual openness is the settlement's selling point. Resistance 0.50 reflects the documented record: calendar-confusion petitions, dress-edict resentment, conscription and tax riots in the 1870s-80s, then quiet noncompliance rather than open revolt; rural households acted in petition coalitions, which is what kept the state's demands negotiable. All three series share one seven-point grid (1873-1945, twelve-year steps) so no metric is sampled against another's gaps. The claimed type is authored from structure — a real coordination achievement joined to asymmetric costs and active enforcement — independently of these metric values.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is its own prudence: the state asked only for what administration required and left everything else alone, and it can point to the revolts it avoided. From the rural-household and household-women seats the same structure is a double burden — two calendars, two wardrobes, a private sphere maintained by unbought female labor — experienced as taking dressed up as restraint. The ritual-authority seat experiences protection that is also captivity: its world survives only inside the space the state left it, and it knows the boundary moves when the state's needs change. Treaty powers see adequate performance and nothing else. The engine computes these per-seat classifications from the roles, power atoms, and exit options authored here; nothing in the claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The state is declared beneficiary and sits near the beneficiary end despite funding enforcement, because what flows to it (fiscal capacity, conscripts, legibility, recognition) dwarfs what it spends. Ritual authorities are beneficiaries with constrained exit: their gain is real but hostage to the partition's persistence, which tempers their effective position below a free beneficiary's. Treaty powers approach the pure-beneficiary pole with arbitrage-grade exit. Rural households and household women sit near the full-target pole — trapped and identity-locked respectively — with the women's identity lock amplifying their effective burden beyond what the raw transfer alone would predict. Urban dual-domain workers land mid-range: declared payers carrying a declared secondary benefit from standardized commerce, so their net position is materially lighter than the rural seats'. No directionality overrides were needed: the role declarations plus exit options already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the partition as pure coordination misses the asymmetry: the code-switching costs and the maintenance labor fall on seats that never bargained, while the collectors are concentrated. Reading it as pure imposition misses what it actually dissolved: a looming total conflict between state-building and the lifeworld, plus a real guarantee that the state would not come for the festivals. The tangled-rope claim keeps both halves visible. On obsolescence: the acute founding problem — obtaining fiscal-military capacity and recognition without civil war — was substantially solved by the early 1900s, yet the partition persisted because each collecting class had sunk stakes in it; the R5 mismatch consumer should find status=contested paired with verdict=world_rearranges, flagging persistence-by-interest rather than persistence-by-necessity. The drift state records where that leads: with the private domain hollowed to residual ceremony in mass society, the arrangement decays toward a maintained shell rather than resolving — the permanence omega tracks whether anything functional remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the dual-practice partition the correct account of practice legitimacy under this kernel, or do the endogenous-displacement or exogenous-override readings describe the same history better?',
    'Comparative institutional analysis of episodes where the partition held, strained, and broke (1873 calendar rollout, Rokumeikan era, Taisho relaxation, wartime mobilization, occupation reform): whichever reading predicts the observed compliance patterns without ad hoc repair wins.',
    'If a sibling reading wins, this constraint''s epsilon and type are reauthored: override implies concentrated imposition (higher epsilon, capture in the state); endogenous implies voluntary uptake (lower epsilon, coordination dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the practice-legitimacy kernel the historical record supports.').

omega_variable(
    sibling_structural_delta,
    'What exactly would each sibling reading change in this story''s structure if adopted?',
    'Reauthor the story under each sibling frame and diff the beneficiary/victim sets, enforcement requirements, and epsilon; the deltas localize the disagreement.',
    'Override adoption removes the traditional-authority beneficiary seat (nothing is legitimately protected) and raises epsilon; endogenous adoption dissolves the enforcement requirement (compliance becomes uptake) and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Structural consequences of switching readings within the kernel.').

omega_variable(
    equilibrium_permanence_question,
    'Is the bifurcation a stable permanent equilibrium, as this reading asserts, or a slow-motion transition toward convergence?',
    'Long-run tracking of private-domain vitality: festival participation, ceremonial dress usage, lunar observance, and household rite transmission rates across generations.',
    'If transitional, the claimed type drifts toward scaffold (support on the way to convergence) or piton (a maintained shell after function lapses); the reading''s permanence axiom fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_permanence_question, empirical, 'Whether the dual-practice equilibrium is terminal or transitional.').

omega_variable(
    codeswitch_cost_attribution,
    'Is the double-life cost borne by households extraction imposed by the arrangement, or an inherent cost of cultural pluralism that any non-assimilationist settlement would carry?',
    'Counterfactual comparison with societies that forced convergence (did the cost vanish or reappear as conflict cost?) and with pluralist societies that never standardized administration.',
    'If inherent, epsilon drops toward the coordination-cost floor; if imposed, the full measured burden counts as extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(codeswitch_cost_attribution, empirical, 'Attribution of the bifurcation''s household costs between design and pluralism overhead.').

omega_variable(
    gendered_maintenance_burden,
    'Is the concentration of the private domain''s maintenance labor on women intrinsic to the partition, or contingent on the particular household regime it happened to protect?',
    'Cross-case comparison of domain partitions under different family-law settlements; measure whether the private-domain labor burden tracks the partition itself or the accompanying household code.',
    'If intrinsic, the victim set concentrates further on women and effective extraction on that seat rises; if contingent, the burden belongs to the family-law constraint, not this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_maintenance_burden, empirical, 'Whether the partition structurally targets women''s unbought labor.').

omega_variable(
    wartime_breach_diagnostic,
    'Does the 1931-1945 penetration of the private domain refute the partition''s stability (showing it lasted only while the state restrained itself), or count as an exogenous shock the reading can absorb?',
    'Test whether postwar practice re-bifurcated spontaneously where coercion lapsed (festival revival, dress reversion) or stayed converged; spontaneous rebifurcation supports the equilibrium reading.',
    'If refuted, the reading downgrades to a fair-weather description and the arrangement''s true character sits closer to the override reading''s; if absorbed, the equilibrium claim survives with a scope condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wartime_breach_diagnostic, conceptual, 'Whether the wartime breach falsifies or merely stresses the equilibrium claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 1873, 1945).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1873, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1873, 0.18).
narrative_ontology:measurement_basis(legi_tr_t1873, observed).
narrative_ontology:measurement(legi_tr_t1885, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1885, 0.3).
narrative_ontology:measurement_basis(legi_tr_t1885, observed).
narrative_ontology:measurement(legi_tr_t1897, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1897, 0.26).
narrative_ontology:measurement_basis(legi_tr_t1897, observed).
narrative_ontology:measurement(legi_tr_t1909, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1909, 0.24).
narrative_ontology:measurement_basis(legi_tr_t1909, observed).
narrative_ontology:measurement(legi_tr_t1921, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1921, 0.25).
narrative_ontology:measurement_basis(legi_tr_t1921, observed).
narrative_ontology:measurement(legi_tr_t1933, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1933, 0.33).
narrative_ontology:measurement_basis(legi_tr_t1933, observed).
narrative_ontology:measurement(legi_tr_t1945, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1945, 0.38).
narrative_ontology:measurement_basis(legi_tr_t1945, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t1873, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1873, 0.36).
narrative_ontology:measurement_basis(legi_be_t1873, observed).
narrative_ontology:measurement(legi_be_t1885, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1885, 0.41).
narrative_ontology:measurement_basis(legi_be_t1885, observed).
narrative_ontology:measurement(legi_be_t1897, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1897, 0.45).
narrative_ontology:measurement_basis(legi_be_t1897, observed).
narrative_ontology:measurement(legi_be_t1909, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1909, 0.48).
narrative_ontology:measurement_basis(legi_be_t1909, observed).
narrative_ontology:measurement(legi_be_t1921, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1921, 0.5).
narrative_ontology:measurement_basis(legi_be_t1921, observed).
narrative_ontology:measurement(legi_be_t1933, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1933, 0.57).
narrative_ontology:measurement_basis(legi_be_t1933, observed).
narrative_ontology:measurement(legi_be_t1945, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1945, 0.66).
narrative_ontology:measurement_basis(legi_be_t1945, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1873, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1873, 0.58).
narrative_ontology:measurement_basis(legi_su_t1873, observed).
narrative_ontology:measurement(legi_su_t1885, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1885, 0.54).
narrative_ontology:measurement_basis(legi_su_t1885, observed).
narrative_ontology:measurement(legi_su_t1897, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1897, 0.46).
narrative_ontology:measurement_basis(legi_su_t1897, observed).
narrative_ontology:measurement(legi_su_t1909, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1909, 0.42).
narrative_ontology:measurement_basis(legi_su_t1909, observed).
narrative_ontology:measurement(legi_su_t1921, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1921, 0.39).
narrative_ontology:measurement_basis(legi_su_t1921, observed).
narrative_ontology:measurement(legi_su_t1933, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1933, 0.56).
narrative_ontology:measurement_basis(legi_su_t1933, observed).
narrative_ontology:measurement(legi_su_t1945, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1945, 0.69).
narrative_ontology:measurement_basis(legi_su_t1945, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'how did practice change become legitimate in modernizing states' decomposes into three structurally distinct claims with different epsilon values over the same standing arrangement. This story (dual_practice_equilibrium_reading) authors epsilon ~0.66 for the partition arrangement seen as a domain-partitioned settlement; the exogenous_override sibling authors the same arrangement as decree-imposed modernization (higher epsilon, single capturer); the endogenous_displacement sibling authors it as accumulated voluntary uptake (lower epsilon, coordination dominant). Upstream/downstream: the override reading is the justification the state itself cited, and the endogenous reading is the account voluntary-adoption histories cite; this reading sits between, describing the settlement both siblings try to explain away. All three files link each other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
