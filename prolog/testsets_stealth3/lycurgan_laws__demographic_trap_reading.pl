% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__demographic_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__demographic_trap_reading, []).

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
 *   constraint_id: lycurgan_laws__demographic_trap_reading
 *   human_readable: Lycurgan Constitutional Freeze (Demographic Trap Reading)
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   The Great Rhetra and the customs gathered around the lawgiver Lycurgus
 *   fixed Sparta's constitution against revision: equal hereditary land
 *   allotments (kleroi) worked by helot labor, compulsory common messes
 *   funded from each allotment, lifelong communal training, and an oath-bound
 *   prohibition on amendment sealed by the founder's reported self-exile and
 *   death. For roughly two centuries the arrangement delivered exceptional
 *   internal cohesion and military primacy; then the citizen roll entered
 *   sustained collapse — from on the order of eight thousand full citizens at
 *   the Persian-war muster to perhaps a thousand in Aristotle's day and seven
 *   hundred by the reform king's census — as inheritance practice, women's
 *   property, and estate consolidation shrank the number of households able
 *   to fund citizenship, while every redistributive remedy was prosecutable
 *   as impiety toward the founder. This file instantiates ONE reading of the
 *   lycurgan_laws kernel — the demographic_trap_reading — under which ε's
 *   referent is the standing Lycurgan arrangement as this reading assesses it
 *   (a locked distributional order consuming its own citizenry), never the
 *   reformers' alternative. The sibling readings (sacral fidelity; noble-lie
 *   covert adaptation) are separate constraints in separate files; nothing of
 *   theirs enters this file's metrics. KEY AGENTS (by structural
 *   relationship): - gerousia_guardian_elders: primary agenda-setter
 *   (institutional/identity_locked) — administers the unamendable corpus;
 *   their authority is constituted by its immutability -
 *   spartiate_dual_kingship: dual-positioned beneficiary/payer
 *   (powerful/identity_locked) — thrones guaranteed by the freeze, reformist
 *   kings destroyed by it - consolidated_landholding_oikoi: primary material
 *   beneficiary (powerful/constrained) — accumulate land inside the closed
 *   allotment regime - remaining_spartiate_citizens: principal target
 *   (moderate/trapped) — bear escalating burdens as the roll shrinks -
 *   hypomeione_disfranchised_citizens: secondary target (moderate/trapped) —
 *   expelled from the roll for poverty, articulately aggrieved -
 *   mothones_and_nothoi: excluded seat (powerless/trapped) — trained to
 *   fight, barred from citizenship - perioikic_levy_communities: excluded
 *   seat (organized/constrained) — taxed and levied without voice -
 *   helot_laborers: extraction substrate (powerless/trapped) — produce
 *   everything, hold nothing - ephorate_board: secondary enforcer
 *   (institutional/constrained) — annual magistrates who police the freeze
 *   but cannot touch its terms - classical_constitutional_analysts:
 *   analytical observer (analytical/analytical) — see the full ledger from
 *   outside its obligations
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, 0.82).
domain_priors:suppression_score(lycurgan_laws__demographic_trap_reading, 0.7).
domain_priors:theater_ratio(lycurgan_laws__demographic_trap_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(lycurgan_laws__demographic_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__demographic_trap_reading, snare).
narrative_ontology:human_readable(lycurgan_laws__demographic_trap_reading, "Lycurgan Constitutional Freeze (Demographic Trap Reading)").
narrative_ontology:topic_domain(lycurgan_laws__demographic_trap_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:requires_active_enforcement(lycurgan_laws__demographic_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__demographic_trap_reading, '640f72c0-5ae9-4a88-aeaa-06441e6f5230').
narrative_ontology:cs_kernel_codification('640f72c0-5ae9-4a88-aeaa-06441e6f5230', fixed_text).
narrative_ontology:cs_authority_grounding('640f72c0-5ae9-4a88-aeaa-06441e6f5230', extraction).
narrative_ontology:cs_interpretation_layer_present('640f72c0-5ae9-4a88-aeaa-06441e6f5230').
narrative_ontology:cs_reading_relation('640f72c0-5ae9-4a88-aeaa-06441e6f5230', lycurgan_laws__sacral_fidelity_reading, influences).
narrative_ontology:cs_reading_relation('640f72c0-5ae9-4a88-aeaa-06441e6f5230', lycurgan_laws__adaptive_fiction_reading, coexists_with).
narrative_ontology:cs_axiom('640f72c0-5ae9-4a88-aeaa-06441e6f5230', foundational, unrevisable_distributive_kernel_is_lethal).
narrative_ontology:cs_axiom_status(unrevisable_distributive_kernel_is_lethal, holdable).
narrative_ontology:cs_axiom_grounding('640f72c0-5ae9-4a88-aeaa-06441e6f5230', unrevisable_distributive_kernel_is_lethal, empirically_contingent).
narrative_ontology:cs_axiom('640f72c0-5ae9-4a88-aeaa-06441e6f5230', secondary, guardian_interest_explains_freeze_persistence).
narrative_ontology:cs_axiom_status(guardian_interest_explains_freeze_persistence, holdable).
narrative_ontology:cs_axiom_grounding('640f72c0-5ae9-4a88-aeaa-06441e6f5230', guardian_interest_explains_freeze_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('640f72c0-5ae9-4a88-aeaa-06441e6f5230', founder_fixed_allotment_order).
narrative_ontology:cs_drift_state('640f72c0-5ae9-4a88-aeaa-06441e6f5230', oliganthropia_terminal_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('640f72c0-5ae9-4a88-aeaa-06441e6f5230', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__demographic_trap_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, gerousia_guardian_elders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, spartiate_dual_kingship).
narrative_ontology:constraint_beneficiary(lycurgan_laws__demographic_trap_reading, consolidated_landholding_oikoi).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, remaining_spartiate_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, hypomeione_disfranchised_citizens).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, mothones_and_nothoi).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, perioikic_levy_communities).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, helot_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__demographic_trap_reading, spartiate_dual_kingship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Twenty-eight men over sixty, elected for life, plus the two kings in session: they set the assembly's business, can dismiss it at will, try capital cases, and adjudicate what counts as faithful observance of the founder's ordinances. Their standing exists only insofar as the ancestral order stays untouched — every ruling presumes the corpus they guard cannot be amended. Leaving the council is not a live option for any of them; it is the summit of a lifetime inside the training system, and stepping outside its guardianship would dissolve the honor they spent sixty years earning.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, gerousia_guardian_elders, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, gerousia_guardian_elders, beneficiary).

% Two hereditary houses hold military command, priesthoods, and precedence for life. The frozen order guarantees their thrones and shields them from the harshest disciplines; the same order destroys any king who treats the corpus as revisable — one king who campaigned for debt relief and land redistribution was prosecuted, fled, and condemned in absentia; another was broken by the magistrates before he could marshal support, and a third was eventually strangled with his mother and grandmother at his arrest. Raised inside the training system from boyhood, they cannot picture themselves outside it; their room to maneuver lies in war and diplomacy, rarely in the domestic settlement.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, spartiate_dual_kingship, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__demographic_trap_reading, spartiate_dual_kingship, payer).

% Dynastic households that, across generations of partible inheritance, heiress marriages, and widows' property rights inside an unamendable allotment regime, have accumulated estates worked by dependent laborers far exceeding the original equal plot. Every proposal to cancel debts or redistribute acreage threatens their holdings directly; their interest aligns fully with treating the founder's division as permanently closed. Departure would mean flight with movable wealth, abandoning the estate, kin network, and public standing that constitute them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, consolidated_landholding_oikoi, beneficiary,
    powerful, generational, constrained, regional).

% Full citizens — the peers — who train from childhood, dine at common messes funded from their assigned estates, and serve in the army. As plots fragment and consolidate around them, ever more cannot meet their mess contribution and quietly drop off the roll; those who remain shoulder longer service on thinner estates. Their assembly vote is real but summoned and dismissed at the council's pleasure, and any speech for redistribution is received as an attack on the founder himself. Exit means exile from the only society they are equipped to inhabit.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, remaining_spartiate_citizens, payer,
    moderate, biographical, trapped, regional).

% Men of full citizen descent who lost their place — usually by failing the mess payment after their estate shrank — and now live inside the city they defended as diminished survivors: barred from the mess, the assembly, and the training-ground honors, yet still bound to its wars and its loyalties. They know the drill yard and the battlefield intimately; their grievance is precise and articulate, and at least one of them built a secret coalition reaching into every dependent class before the authorities strangled it in its crib.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, hypomeione_disfranchised_citizens, payer,
    moderate, biographical, trapped, regional).

% Boys of mixed or unfree parentage raised alongside citizens in the same training, fighting in the same battles, frequently outperforming the sons of the full roll — and permanently barred from citizenship by the frozen rules of descent. They would ask why valor earns nothing and why the city trains soldiers it refuses to count; no venue exists in which that question can be pressed, and the men who might press it are scattered through the barracks and the ranks.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, mothones_and_nothoi, excluded,
    powerless, biographical, trapped, regional).

% Free inhabitants of the subject towns across Laconia and Messenia: traders, craftsmen, and farmers who govern their own villages, pay dues, furnish troops, and hold no vote over anything that governs them. Several of their communities grew wealthy while the citizen body thinned; their leading men would gladly trade revenue for rights, or at least for admission to a roll their blood and taxes already qualify them for. Their objection is old, articulate, and structurally unheard.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, perioikic_levy_communities, excluded,
    organized, generational, constrained, regional).

% The unfree population bound to the land, outnumbering their masters many times over, producing everything the citizen order consumes. They are tied to their masters' plots, subjected to yearly ritual degradation and episodic killing dressed up as security precaution, and periodically in open revolt — most consequentially in the long insurrection that followed the great earthquake. Nothing in the settlement offers them a way out; their labor is the substrate on which every other arrangement stands.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, helot_laborers, payer,
    powerless, immediate, trapped, regional).

% Five annually elected overseers who watch the kings, may indict any official, and administer the counting of citizens. Annual rotation keeps the office responsive to panic — in bad years they take censuses and ask aloud why the rolls shrink — yet their instruments stop short of the settlement itself: they can punish a man, not revise a law. One board arrested a young reforming king and put him and his female kin to death; another was cut down wholesale by a later reformer. Enforcement is their whole function; redesign is beyond their reach and their term.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, ephorate_board, agenda_setter,
    institutional, immediate, constrained, regional).

% Travelers, philosophers, and later historians — an admiring Athenian general marveling at a constitution unchanged for four hundred years, a philosopher cataloguing the landholding and census arithmetic, the reformers' biographers recording who blocked what and why — examining the arrangement from outside its obligations. They see the complete ledger: the stability, the discipline, the shrinking rolls, and the prosecutions of everyone who proposes repair.
narrative_ontology:constraint_stakeholder(lycurgan_laws__demographic_trap_reading, classical_constitutional_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__demographic_trap_reading, consolidated_landholding_oikoi).
narrative_ontology:fixing_cost_class(lycurgan_laws__demographic_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinated a closed warrior caste: standardized lifelong training, common meals funded from assigned estates, synchronized command under hereditary kings checked by an elder council, and a sealed succession of generations — solving the problem of keeping a numerically thin master class cohesive, combat-ready, and unreachable by commerce, indefinitely, atop a hostile labor force.
% TRANSFER_FUNCTION: It moved agricultural surplus from unfree laborers to citizen households as the condition of holding land; it moved each citizen's lifetime — childhood to old age — into communal training and service; and, as estates consolidated inside the unamendable frame, it moved land and citizen standing away from poorer lineages toward richer ones, while concentrating interpretive authority over the settlement in the council of elders.
% ABSENT_VOICES: The disfranchised peers, the unenrolled fighters, the subject-town levies, and the unfree laborers all held standing objections — for land, for enrollment, for rights, for freedom — and none possessed a forum: the assembly met only when convened and could be dismissed mid-session, and any redistributive motion arrived pre-framed as an offense against the founder rather than as a policy question. Their objections survive only in the record of prosecutions, conspiracies, and revolts.
% DISAPPEARANCE_RATIONALE: Overnight repeal of the lock — mere permission to amend — rearranges everything downstream: debt relief and land redistribution rebuild the mess-paying base; enrollment of the trained but uncounted and of qualified subject-townsmen refills the roll; the council loses its monopoly on interpretation and the assembly regains initiative. Nothing else in the Spartan world depends on the lock itself — the army fights the same day, the estates keep producing, the festivals continue. What vanishes is only the guardian interest and the frozen distribution it protects.
% FOUNDING_PROBLEM: After the conquest of Messenia, a small Dorian citizen body found itself ruling a servile population many times its number across two fertile regions. The settlement was built to solve that: bind every citizen to an equal allotment and a common mess so no one could opt out of defense, concentrate command in hereditary kings checked by a council of elders, and seal the whole bargain against renegotiation so no faction could buy support by promising to unwind it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Aristotle's constitutional survey analyzes the shrinking citizen roll and the failed allotment system from an external analytic seat and traces the decline to the settlement's own rules; the reform movement's recorded speeches and programs diagnose the founding problem as obsolete and the lock as lethal; later constitutional historians treat the citizen shortage as the proximate cause of the collapse. No corroboration comes from the council or the great estates, who attributed every shortfall to impiety and individual failing — their denial is itself part of the record.
narrative_ontology:disappearance_verdict(lycurgan_laws__demographic_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__demographic_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__demographic_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__demographic_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__demographic_trap_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__demographic_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__demographic_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__demographic_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high and rising (0.44 to 0.82 across the interval) because this reading's referent — the locked arrangement itself — converts every exogenous shock (war deaths, the 464 earthquake, estate fragmentation) into permanent, uncompensated loss for insiders: the freeze forbids the compensating adjustments any amendable order would make. Suppression is the constraint's core mechanism rather than a side effect: persistence required treating redistributive politics as sacrilege, with prosecutions of kings, the strangling of a reforming monarch and his female kin, the preemptive destruction of a cross-class conspiracy, and routine expulsion of foreigners who might carry new ideas inward — hence suppression_requirement climbing from 0.38 to a peak of 0.87 around the reform crisis before external powers absorbed enforcement and the requirement eased. Theater_ratio climbs from 0.12 to 0.70 as the equality machinery (common messes, peer ranks, the training parade) keeps performing after the equal-allotment reality beneath it has visibly dissolved — the mess survives as liturgy after the plot that funded it has been subdivided and consolidated out of existence. Accessibility_collapse 0.75: inside the framework, alternatives collapse almost completely (reform reads as treason against the founder, exit as civic annihilation, interpretation is monopolized by the council), though physical exits — other poleis, mercenary service, neighboring courts — keep it below natural-law levels. Resistance 0.60: helot insurrection including the decade-long revolt after the great earthquake, a conspiracy of the disfranchised reaching into every dependent class in 397, and finally two royal reform movements with mass support among the poor — each met and crushed. Claim and metrics are independent authored facts: the snare claim states what this reading takes the structure to be; the metrics describe its operation as the record attests.
 *   
 *   Interval convention: t=0 is approximately 700 BCE (the consolidated settlement after the First Messenian War) and t=500 approximately 200 BCE (the externally supervised remnant); grid points fall every fifty years. Anchors: t=225 approximates the Persian-war muster (~8,000 citizens), t=300 the acknowledged shortage and the 397 conspiracy, t=325 the Leuctra aftermath, t=350 Aristotle's demographic catalogue, t=450 the Agis IV crisis, t=475 Cleomenes III and the battle that ended independence. All three tracked series share this single grid.
 *   
 *   Coordination typing: resource_allocation — the load-bearing circuit is the kleros-helot-syssition exchange of land, labor, and mess obligation; its seizure-up is precisely what empties the citizen roll, so the allocation layer is where the constraint's coordination function lives. Suppression composition: roughly 60% structural (religious-legal penalties, council control of the agenda, expulsions of strangers) and 40% internalized (formation in piety from childhood, terror of impiety, ancestor-veneration), with the residual ambiguity routed to the suppression_mechanism_composition omega. Coalition note: the 397 conspiracy was exactly the feared coalition of victims — a disfranchised former citizen building a cell that reached helots, subject townsmen, and even household servants; detected early and destroyed, and the scare hardened enforcement afterward. Coalition potential existed on paper and was extinguished in practice, which is itself diagnostic.
 *
 * PERSPECTIVAL GAP:
 *   From the council chamber the freeze is sacred stewardship: the elders experience guardianship as the source of their honor and cannot occupy a frame in which amending the corpus is ordinary governance. From the mess-line the identical arrangement is a machine for grinding citizens out of the roll. From the consolidated estate it is benign continuity. From the helot quarters it is simply servitude. The kings straddle: beneficiaries of the throne's guarantee and prospective victims of its enforcement — which is why the reform impulse came from inside the royal houses rather than against them. The binding mechanism is institutional-professional identity fusion: the council's guardianship is not a role its members play but what six decades of formation made them; a king is a king only inside the order he would have to break to save it; a citizen's entire competence is Spartan competence. When the identity frame broke — and it did, once, when a young king concluded the founder's settlement had become the city's killer — the constraint's hold evaporated with startling speed: within a couple of years that king had abolished the overseers' board, redistributed the land at a stroke, and begun enrolling the excluded, demonstrating that the lock was identity and enforcement, not physics. Per-seat classifications diverge accordingly; the engine computes that divergence from the structural data, and the divergence — not the label — is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: the council, the royal houses, and the consolidating estates sit in beneficiaries[] (low d, toward the subsidized end), with the council additionally the agenda-setter. The victim rows — shrinking citizens, disfranchised peers, the uncounted fighters, the subject towns, the helots — sit near the full-target end, and trapped exit keeps them there: no arbitrage-grade exit exists for any insider, because leaving means forfeiting the only identity the training system permits. No directionality overrides are authored: the beneficiary/victim declarations plus exit differentiation already separate the seats correctly — notably the kings, whose identity-locked exit pushes their derived directionality above the comfortable estates' despite both nominally benefiting; the structural derivation handles that asymmetry without intervention. The ephorate carries no declaration and falls back near symmetric: it enforces the freeze, rotates out of it annually on a deliberately short leash, and is periodically destroyed by it (one board was massacred by a reforming king), which nets to roughly zero structural gain. Spatial scopes are regional throughout — the polis plus its conquered territory is the constraint's whole world — so no large-scope amplification applies beyond the base computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — holding a small citizen body safely dominant over a vastly larger servile population newly acquired by conquest — was real, urgent, and solved brilliantly for roughly two centuries. It died with the loss of Messenia and the transformation of Greek warfare, yet the arrangement persisted past its problem, and that persistence is precisely the demographic catastrophe this reading names. Mandatrophy resolution here prevents a double mislabel. The admirer's reading (four hundred unchanging years, the envy of Greece) mistakes inertia for natural permanence and would classify a death spiral as a mountain — the false-summit risk this corpus exists to catch. The prosecutor's reading flattens the genuine early coordination function — which was real: the messes, the training, and the allotments did solve a hard collective-action problem, which is why intelligent Spartiates defended the arrangement for generations — into pure predation and cannot explain the arrangement's origin. The snare classification preserves both truths at once: real coordination, asymmetric extraction, enforcement-dependent persistence, identifiable victims, and a founding problem that died while the machinery lived on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    citizen_decline_causation,
    'How much of the Spartiate population collapse was produced by the unrevisable distributive kernel itself, as opposed to war mortality, the 464 earthquake, plague, and elite reproductive patterns?',
    'Comparative demography against peer poleis with amendable citizenship regimes, plus quantitative reconstruction of citizen counts from the attested census anchors (muster figures of the Persian-war era, Aristotle''s totals, the reform king''s enumeration).',
    'If the lock accounts for most of the variance, the snare classification strengthens and the temporal series is causal; if exogenous shocks dominate, this reading overstates the freeze''s agency and the arrangement looks closer to a coordination device overtaken by misfortune.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_decline_causation, empirical, 'Attribution weight of constitutional rigidity in the citizen decline.').

omega_variable(
    lock_boundary_partition,
    'Exactly which variables were locked and which covertly adjustable — did the celebrated hidden flexibility (extraconstitutional offices, ad hoc waivers, foreign-policy improvisation) ever reach the load-bearing distributive core of land and citizenship?',
    'Systematic audit of every attested deviation from the founder''s settlement, sorted by whether it touched allotments, mess eligibility, or the descent rules of citizenship.',
    'If covert adaptation routinely reached the distributive core, the adaptive_fiction_reading instantiates the better model and this file''s rigidity and suppression estimates are inflated; if the core was genuinely untouchable, this reading''s structural claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_boundary_partition, conceptual, 'Partition of locked versus covertly adaptive variables; arbitrates against the noble-lie sibling reading.').

omega_variable(
    suppression_mechanism_composition,
    'Was the enforcement of immutability primarily structural (penalties, adjournment, prosecution, expulsion) or internalized (formation in piety, terror of impiety, ancestor-veneration) — and in what proportion?',
    'Trace who invokes sacrilege and when: if pious framing concentrates among holders of threatened privilege while the poor speak the language of hunger, the internalized share shrinks; compare crisis-decade rhetoric against calm-decade rhetoric.',
    'A predominantly structural reading supports the classification as authored; a predominantly internalized reading raises effective suppression above the structural measure and shifts what remediation would even mean.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, conceptual, 'Structural versus internalized share of the immutability enforcement.').

omega_variable(
    reform_counterfactual_viability,
    'Could redistribution-plus-enrollment of the kind attempted in the 240s–220s have restored demographic viability if attempted earlier or at full scale, or was the spiral already irreversible when the first serious attempt was mounted?',
    'Demographic modeling over alternative policy paths from the attested population anchors, stress-tested against continuing war mortality.',
    'If earlier reform was viable, the harm was contingent on the lock and this reading''s indictment strengthens; if the spiral was already irreversible at first attempt, part of the measured damage belongs to delay dynamics rather than the freeze alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_counterfactual_viability, empirical, 'Viability of the counterfactual reform path.').

omega_variable(
    epsilon_referent_decomposition,
    'Within the measured ε, how much is the universal helot-rent substrate that every reading of this kernel shares, and how much is trap-specific — the denial of compensatory adjustment that only the immutability lock produces?',
    'Decompose extraction streams by beneficiary: rent flows that would exist under any Spartan regime versus losses traceable specifically to forbidden adjustment (foregone enrollments, unrelieved debt, unrepaired allotments).',
    'Guards ε-invariance across the reading family: the shared substrate belongs equally to the sibling files'' ε, while only the trap-specific share differentiates this reading''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_decomposition, conceptual, 'Splitting shared helot-substrate extraction from lock-specific extraction.').

omega_variable(
    reading_position_in_kernel_family,
    'This file instantiates the demographic_trap_reading of the lycurgan_laws kernel; which structural verdicts would flip under the sibling readings, and where exactly does the disagreement bite?',
    'Compile sacral_fidelity_reading and adaptive_fiction_reading as separate constraints and compare per-seat classifications and ε over the identical referent.',
    'Under the sacral reading, ε falls (suffering re-reads as the price of ordained fidelity) and the target rows shift toward impious reformers; under the adaptive-fiction reading, suppression falls (hidden valves soften measured rigidity) and the classification drifts toward hybrid coordination. The disagreement is located in a single variable: whether the freeze was divinely warranted, fictively elastic, or substantively rigid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_position_in_kernel_family, conceptual, 'Committer-frame routing: this file is one reading of the lycurgan_laws kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__demographic_trap_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__demographic_trap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lycu_tr_t50, lycurgan_laws__demographic_trap_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(lycu_tr_t100, lycurgan_laws__demographic_trap_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement(lycu_tr_t150, lycurgan_laws__demographic_trap_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement(lycu_tr_t200, lycurgan_laws__demographic_trap_reading, theater_ratio, 200, 0.26).
narrative_ontology:measurement(lycu_tr_t250, lycurgan_laws__demographic_trap_reading, theater_ratio, 250, 0.34).
narrative_ontology:measurement(lycu_tr_t300, lycurgan_laws__demographic_trap_reading, theater_ratio, 300, 0.43).
narrative_ontology:measurement(lycu_tr_t350, lycurgan_laws__demographic_trap_reading, theater_ratio, 350, 0.53).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__demographic_trap_reading, theater_ratio, 400, 0.61).
narrative_ontology:measurement(lycu_tr_t450, lycurgan_laws__demographic_trap_reading, theater_ratio, 450, 0.62).
narrative_ontology:measurement(lycu_tr_t500, lycurgan_laws__demographic_trap_reading, theater_ratio, 500, 0.7).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__demographic_trap_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(lycu_be_t50, lycurgan_laws__demographic_trap_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement(lycu_be_t100, lycurgan_laws__demographic_trap_reading, base_extractiveness, 100, 0.54).
narrative_ontology:measurement(lycu_be_t150, lycurgan_laws__demographic_trap_reading, base_extractiveness, 150, 0.57).
narrative_ontology:measurement(lycu_be_t200, lycurgan_laws__demographic_trap_reading, base_extractiveness, 200, 0.6).
narrative_ontology:measurement(lycu_be_t250, lycurgan_laws__demographic_trap_reading, base_extractiveness, 250, 0.65).
narrative_ontology:measurement(lycu_be_t300, lycurgan_laws__demographic_trap_reading, base_extractiveness, 300, 0.69).
narrative_ontology:measurement(lycu_be_t350, lycurgan_laws__demographic_trap_reading, base_extractiveness, 350, 0.73).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__demographic_trap_reading, base_extractiveness, 400, 0.77).
narrative_ontology:measurement(lycu_be_t450, lycurgan_laws__demographic_trap_reading, base_extractiveness, 450, 0.8).
narrative_ontology:measurement(lycu_be_t500, lycurgan_laws__demographic_trap_reading, base_extractiveness, 500, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__demographic_trap_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(lycu_su_t50, lycurgan_laws__demographic_trap_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement(lycu_su_t100, lycurgan_laws__demographic_trap_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(lycu_su_t150, lycurgan_laws__demographic_trap_reading, suppression_requirement, 150, 0.47).
narrative_ontology:measurement(lycu_su_t200, lycurgan_laws__demographic_trap_reading, suppression_requirement, 200, 0.51).
narrative_ontology:measurement(lycu_su_t250, lycurgan_laws__demographic_trap_reading, suppression_requirement, 250, 0.57).
narrative_ontology:measurement(lycu_su_t300, lycurgan_laws__demographic_trap_reading, suppression_requirement, 300, 0.63).
narrative_ontology:measurement(lycu_su_t350, lycurgan_laws__demographic_trap_reading, suppression_requirement, 350, 0.71).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__demographic_trap_reading, suppression_requirement, 400, 0.75).
narrative_ontology:measurement(lycu_su_t450, lycurgan_laws__demographic_trap_reading, suppression_requirement, 450, 0.87).
narrative_ontology:measurement(lycu_su_t500, lycurgan_laws__demographic_trap_reading, suppression_requirement, 500, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__demographic_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__demographic_trap_reading, adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Lycurgan laws' conflates at least three structurally distinct commitments: (1) that the corpus is sacred and absolutely binding (sacral_fidelity_reading), (2) that its immutability was a deliberate fiction laid over flexible practice (adaptive_fiction_reading), (3) that it was substantively rigid and that the rigidity killed the citizen body (this file). Each yields a different ε, victim set, and classification, so each gets its own file; family edges run through network.affects_constraints. Edge structure: the demographic record presses on the sacral reading's legitimacy conditions (influences) without making it logically untenable, while coexisting with the adaptive-fiction reading, whose core premise of hidden elasticity partitions — rather than contradicts — this reading's core premise of substantive lock: covert adaptation may have operated at the margins of offices and procedure while the distributive kernel stayed shut, and the lock_boundary_partition omega arbitrates exactly that boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
