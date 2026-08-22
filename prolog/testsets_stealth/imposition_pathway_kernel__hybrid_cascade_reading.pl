% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade Reading: State-Manufactured Fringe Climb (Meiji Dress Imposition Pathway)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Meiji state's dress decrees did not displace existing commitment by
 *   decree alone, nor did the new attire spread purely organically: the
 *   decree manufactured a mandatory fringe — officials, policemen, teachers,
 *   conscripts — whose adopted dress then became the visible summit of a
 *   status gradient the rest of society climbed. Override initiated; climb
 *   completed. This file authors ONE reading of the
 *   imposition_pathway_kernel, the hybrid_cascade_reading, as a clean
 *   epsilon-invariant constraint: the standing arrangement under contest is
 *   the state-manufactured-fringe cascade as this reading sees it, and
 *   epsilon is authored for THAT arrangement (compelled adoption costs plus
 *   status-coercion during the cascade, net of the genuine coordination
 *   delivered), never for the endogenous or exogenous alternatives. Per the
 *   epsilon-invariance principle, the sibling readings are separate
 *   constraint stories with their own epsilon, beneficiaries, and victims,
 *   linked through network.affects_constraints; the contest between readings
 *   lives in the omega variables and cs_structure, not inside this
 *   constraint's classification.
 *
 * KEY AGENTS:
 *   - meiji_state_apparatus: Agenda setter (institutional/arbitrage) — issues and enforces the decrees, collects diplomatic legitimacy, can unwind the mechanism at will
 *   - bureaucratic_military_elite: Primary beneficiary (powerful/constrained) — collects status marking and career advantage; bore first-wave purchase costs as secondary payer
 *   - salaried_officials_conscripts: Primary target (moderate/trapped) — bears the compelled purchase burden; exit means leaving the modern sector
 *   - traditional_textile_artisans: Secondary target (organized/constrained) — displaced demand, organized but structurally losing
 *   - western_cloth_importers: Receipt seat (organized/mobile) — collects the redirected spending without running anything
 *   - urban_status_climbers: Voluntary climber (moderate/constrained) — pays retail voluntarily, collects access and standing; near-symmetric position
 *   - rural_taxpaying_households: Indirect payer (powerless/trapped) — finances the apparatus through land tax, no voice
 *   - meiji_women_reform_debaters: Excluded seat (powerless/trapped) — discussed but never consulted; documents the consensus-provenance gap
 *   - western_diplomatic_observers: Observer (institutional/analytical) — supplies the validation feedback the display sought
 *   - bunmei_kaika_publicists: Secondary beneficiary (moderate/mobile) — supplies the legitimating vocabulary, collects circulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.22).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Hybrid Cascade Reading: State-Manufactured Fringe Climb (Meiji Dress Imposition Pathway)").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, 'b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c').
narrative_ontology:cs_kernel_codification('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', formalized).
narrative_ontology:cs_authority_grounding('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', expertise).
narrative_ontology:cs_interpretation_layer_present('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c').
narrative_ontology:cs_reading_relation('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_axiom('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', foundational, override_initiates_climb_completes).
narrative_ontology:cs_axiom_status(override_initiates_climb_completes, holdable).
narrative_ontology:cs_axiom_grounding('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', override_initiates_climb_completes, empirically_contingent).
narrative_ontology:cs_axiom('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', foundational, state_manufactured_fringe_is_causally_load_bearing).
narrative_ontology:cs_axiom_status(state_manufactured_fringe_is_causally_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', state_manufactured_fringe_is_causally_load_bearing, empirically_contingent).
narrative_ontology:cs_reference_frame('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', override_initiates_climb_completes_pathway).
narrative_ontology:cs_drift_state('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', contemporary_mset_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b85c53e1-d67f-4227-b3a4-24ff2f9b0f5c', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, bureaucratic_military_elite).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, western_cloth_importers).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, bunmei_kaika_publicists).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, salaried_officials_conscripts).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditional_textile_artisans).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, rural_taxpaying_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, urban_status_climbers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, bureaucratic_military_elite).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, urban_status_climbers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1872 court and official dress decrees, procures uniforms for the new conscript army, and directs ministries and police to secure compliance among its own personnel. Gains a visibly modern face for treaty negotiations and a uniformly equipped military. Can amend, relax, or abandon the decrees at will; its exposures are diplomatic embarrassment and conservative backlash rather than personal cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Senior officials and officers whose advancement runs through the new dress order; the mandated wardrobe marks rank and separates them from domain-era rivals. They also absorbed the first wave of purchase costs on fixed stipends and cannot step out of the costume without leaving office.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, bureaucratic_military_elite, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, bureaucratic_military_elite, payer).

% Clerks, teachers, policemen, and conscripts ordered into Western dress or issued uniforms deducted from pay. Refusal ends careers or brings discipline; compliance consumes a large share of entry-level salaries. Leaving means resigning from the modern sector entirely.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, salaried_officials_conscripts, payer,
    moderate, biographical, trapped, national).

% Kimono weavers, dyers, and tailors serving official, school, and ceremonial demand. As ministries and the army shift procurement to wool and Western tailoring, their premium market erodes. Guild networks cushion but cannot reverse the shift; pivoting to export or civilian fashion is possible but slow and capital-hungry.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_textile_artisans, payer,
    organized, generational, constrained, regional).

% Trading houses linking official demand to Manchester and Milan mills. Every newly mandated uniform category expands their order book. They hold no enforcement duties and can redirect capital to other trades if policy reverses.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, western_cloth_importers, beneficiary,
    organized, biographical, mobile, global).

% Merchants, professionals, and students who adopt Western dress ahead of any requirement in order to signal modernity, win appointments, and enter fashionable society. They pay retail prices no decree imposed on them and collect access and standing in return; abandoning the costume after adoption reads as decline.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, urban_status_climbers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, urban_status_climbers, beneficiary).

% Land-tax payers financing the ministries, schools, and army whose procurement redirects demand away from local artisan networks. Distant from the decree's drafting, they experience the arrangement as prices and taxes; no channel exists through which to register objection.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rural_taxpaying_households, payer,
    powerless, generational, trapped, regional).

% Women whose clothing was repeatedly debated by male officials and publicists — proposed for Western reform, then deferred into modesty campaigns and apron-dress schemes. They sat in no deliberative body; every arrangement made about their dress was argued entirely without them.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, meiji_women_reform_debaters, excluded,
    powerless, biographical, trapped, national).

% Ministers and envoys of the treaty powers whose reception of Japanese delegations and courts supplied the feedback the modernization display sought. They judge, sign, and report; they bear none of the costs and enforce nothing.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, western_diplomatic_observers, observer,
    institutional, generational, analytical, global).

% Writers and educators of the civilization-and-enlightenment movement whose lectures and journals supplied the vocabulary for the dress reforms. Their circulation and consulting income grew with official enthusiasm, and they can change subjects if the fashion turns.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, bunmei_kaika_publicists, beneficiary,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, western_cloth_importers).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized official and military dress solved real problems at once: instant visual identification of state personnel, equipment and appearance coherence across a newly conscripted army drawn from unrelated domains, a legible diplomatic presence for treaty negotiations, and a single visible signal of sovereign modernization replacing fragmented domain-specific dress.
% TRANSFER_FUNCTION: Moves purchasing power from salaried officials, conscripts, and (through land tax) rural households toward the imported wool and cotton garment trade and the new tailoring sector; moves status from hereditary markers toward displayed modernity; moves diplomatic legitimacy toward the Meiji state from both domestic conservatives and the treaty powers.
% ABSENT_VOICES: Rural taxpayers who financed the apparatus, women whose dress was debated but who were seated in no deliberative body, and artisan guilds facing demand erosion without consultation would all object if present. Their absence made the decree consensus real only inside the room where it was drafted.
% DISAPPEARANCE_RATIONALE: Had the mandate-and-cascade vanished overnight in 1873, the conscript army would have lacked unified equipment and appearance standards, diplomatic receptions would have read as pre-modern to the treaty powers, treaty revision would have lost its chosen proof of fitness, and the urban status economy would have organized around different markers. Named parties exist on every side of the arrangement; the world it orders would rearrange.
% FOUNDING_PROBLEM: Post-Restoration Japan faced existential diplomatic pressure: unequal treaties, gunboat diplomacy, and the recently demonstrated fate of states that could not display modernization. Rapid, visible adoption of Western forms by the state's own personnel was the chosen proof of sovereign fitness for equal treatment.
% FOUNDING_PROBLEM_CORROBORATION: Treaty-power diplomatic archives and subsequent economic histories corroborate that the recognition objectives were substantially achieved by the mid-1890s, by which time the mandates had already receded — attested from outside the benefiting parties. The state itself continued to attest live security and modernity rationales; the two attestations disagree, hence contested.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).
:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on structural grounds independent of the metrics: the arrangement delivers genuine coordination (army coherence, diplomatic legibility, administrative identification) AND asymmetric extraction through the same structure (compelled purchases on trapped salaries, artisan demand displacement, tax-financed procurement shifts), and it required active enforcement to hold. Metrics are authored independently as descriptive facts. End-state extractiveness 0.42 reflects the arrangement after the climb completed: residual compelled categories (military, school, ceremonial procurement) plus locked-in replacement cycles, net of coordination value; the series peaks at 0.66 mid-cascade when enforcement and purchase burdens coincided. Suppression_requirement is authored because enforcement capacity is precisely the dynamic this story traces: a rise to 0.74 as police enforcement matured against resistance, then decay to 0.22 as organic adoption made the mandate redundant — the signature of an override whose coercive shell dissolves into custom. Theater_ratio humps at 0.36 around the Rokumeikan display era, when performative westernization for foreign audiences outweighed functional standardization, then declines as durable institutions outlast the performance. All three series run on one shared eight-point grid (1872-1900, four-year steps) so every metric is authored at every examined time point; trajectories are monotonic arcs, not cycles, so no intermittent-reinforcement reading applies. Coalition note: the victim seats never coalesced — officials were atomized and individually trapped, artisans were organized but on the losing side of a procurement shift, rural households had no channel — so repeal came from the agenda setter once the founding function expired, not from below.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from identical structural data. From the state's position the arrangement is an instrument it built, controls, and can unwind — arbitration-grade exit places it near the beneficiary end regardless of costs borne. From the trapped salaried seats the same structure operates as compelled transfer with career-priced exit. The dual-positioned climbers sit near symmetric: voluntary payment, collected status. The excluded seats (women, rural taxpayers) register no computed type but document that the decree's unanimity was manufactured by room composition. The engine computes these divergences from power, exit, and role data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. meiji_state_apparatus is listed beneficiary and agenda setter with arbitrage exit — derived d sits near the beneficiary end (effective extraction damped toward subsidy). bureaucratic_military_elite, western_cloth_importers, and bunmei_kaika_publicists are beneficiaries with constrained-to-mobile exit — low d, low effective extraction. salaried_officials_conscripts and rural_taxpaying_households are victims with trapped exit — high d, amplified effective extraction; trapped or immobile targets sit nearer the full-target end than mobile ones. traditional_textile_artisans are victims with constrained (not trapped) exit — elevated but not maximal d. urban_status_climbers carry dual payer/beneficiary roles with voluntary adoption — the derivation lands them near symmetric, which is why no directionality override is needed: the structural data already encodes the dual position through secondary_role. Scope is national for the mandate's operation, which moderately amplifies verification difficulty and hence effective extraction on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question — has the mandate outlived its function — resolves YES for the coercive layer by interval end: the diplomatic-recognition problem the decrees were built to solve was substantially achieved by the mid-1890s, after which the mandate persisted only as inertia and was informally wound down as custom took over. This is why the story declares mandatrophy_resolved and why end-state suppression falls to 0.22 rather than ratcheting. The classification prevents two mislabels: calling the arrangement a pure snare ignores the real coordination delivered (a conscript army that actually cohered, treaties that actually progressed); calling it a pure rope ignores who paid (trapped salaried men, displaced artisans, voiceless rural taxpayers) versus who decided. The residual post-1900 status competition is treated as a successor arrangement — a separate constraint story under the epsilon-invariance rule — rather than inflating this constraint's end-state epsilon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment_meiji,
    'Does the Meiji dress displacement instantiate the hybrid cascade (override initiates, climb completes), the endogenous climb with invisible fringe stages, or a genuine exogenous override requiring no climb completion?',
    'Stratified adoption-curve process tracing: if state-personnel saturation measurably precedes broad urban uptake with a lag tracking visibility rather than price, the hybrid cascade is confirmed; if broad uptake tracks independent commercial channels contemporaneous with official adoption, the endogenous reading gains; if compliance never became voluntary after enforcement relaxed, the exogenous reading gains.',
    'Determines whether the M-set requires a distinct hybrid cell or whether the existing endogenous and exogenous cells exhaust the mechanism space; reclassifies this story''s network position as terminal, intermediate, or redundant within the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment_meiji, empirical, 'Which reading of the imposition-pathway kernel the Meiji case actually instantiates.').

omega_variable(
    fringe_artificiality_degree,
    'How artificial was the manufactured fringe — how much early official adoption of Western dress was decree-caused versus already underway through treaty-port trade and prior contact?',
    'Pre-decree adoption baselines comparing port-city officials (with commercial exposure) to interior postings (without), controlling for rank and salary.',
    'If the decree''s marginal contribution was small, the cascade compresses toward the endogenous reading and this story''s epsilon drops; if decisive, the state-manufactured-fringe claim is load-bearing and epsilon holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_artificiality_degree, empirical, 'Degree to which the mandatory fringe was state-created rather than accelerated pre-existing adoption.').

omega_variable(
    rent_vs_standardization_cost,
    'How much of the measured extraction is the inherent cost of any rapid standardization (fast modernization compels purchases under any mechanism) versus rent captured by the import trade through official preference?',
    'Price series for equivalent garments in mandated procurement channels versus open retail channels over the interval; a persistent mandated-channel premium indicates rent, convergence indicates coordination cost.',
    'If most extraction is inherent transition cost, the arrangement sits closer to rope and the tangled_rope claim weakens toward rope; if a durable premium accrued to importers, the extraction component is structural and the tangled_rope claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_vs_standardization_cost, conceptual, 'Boundary between unavoidable transition cost and captured rent within measured extraction.').

omega_variable(
    status_economy_successor_boundary,
    'Does the post-mandate status-competition extraction (voluntary climbing, replacement-cycle spending after 1900) belong to THIS constraint''s operation or to a successor arrangement that deserves its own story?',
    'Test whether the post-1900 extraction survives hypothetical abolition of the original decree structure: if the status economy operates identically once the mandate layer is gone, it is a successor constraint with its own epsilon; decompose and link via network.affects_constraints.',
    'If successor, this story''s end-state epsilon falls toward the mandate-only residue (~0.35) and a new story carries the status economy; if continuous, the end-state 0.42 stands as one arrangement''s late phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_economy_successor_boundary, conceptual, 'Whether late-period status extraction is this constraint''s tail or a distinct successor constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 1872, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_hybrid_tr_t1872, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1872, 0.2).
narrative_ontology:measurement(imposition_hybrid_tr_t1876, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1876, 0.26).
narrative_ontology:measurement(imposition_hybrid_tr_t1880, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1880, 0.34).
narrative_ontology:measurement(imposition_hybrid_tr_t1884, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1884, 0.36).
narrative_ontology:measurement(imposition_hybrid_tr_t1888, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1888, 0.33).
narrative_ontology:measurement(imposition_hybrid_tr_t1892, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1892, 0.3).
narrative_ontology:measurement(imposition_hybrid_tr_t1896, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1896, 0.27).
narrative_ontology:measurement(imposition_hybrid_tr_t1900, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 1900, 0.25).

% Extraction over time
narrative_ontology:measurement(imposition_hybrid_be_t1872, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1872, 0.58).
narrative_ontology:measurement(imposition_hybrid_be_t1876, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1876, 0.63).
narrative_ontology:measurement(imposition_hybrid_be_t1880, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1880, 0.66).
narrative_ontology:measurement(imposition_hybrid_be_t1884, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1884, 0.6).
narrative_ontology:measurement(imposition_hybrid_be_t1888, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1888, 0.54).
narrative_ontology:measurement(imposition_hybrid_be_t1892, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1892, 0.49).
narrative_ontology:measurement(imposition_hybrid_be_t1896, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1896, 0.44).
narrative_ontology:measurement(imposition_hybrid_be_t1900, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 1900, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(imposition_hybrid_su_t1872, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1872, 0.7).
narrative_ontology:measurement(imposition_hybrid_su_t1876, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1876, 0.74).
narrative_ontology:measurement(imposition_hybrid_su_t1880, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1880, 0.68).
narrative_ontology:measurement(imposition_hybrid_su_t1884, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1884, 0.58).
narrative_ontology:measurement(imposition_hybrid_su_t1888, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1888, 0.46).
narrative_ontology:measurement(imposition_hybrid_su_t1892, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1892, 0.36).
narrative_ontology:measurement(imposition_hybrid_su_t1896, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1896, 0.28).
narrative_ontology:measurement(imposition_hybrid_su_t1900, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 1900, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'top-down imposition of commitment' decomposes into three structurally distinct claims per the epsilon-invariance principle: endogenous_climb_reading (epsilon near zero for the mechanism itself — displacement is climb all the way down), exogenous_override_reading (imposition as a distinct no-climb mechanism), and this hybrid_cascade_reading (override-initiated, climb-completed, with a causally load-bearing manufactured fringe). This story authors epsilon only for the hybrid arrangement as seen by its own reading; the upstream empirical record (Meiji process-tracing) feeds all three, and each sibling cites the same cases as evidence for incompatible mechanisms. Family members are mutually linked via affects_constraints; contamination propagates along these edges when new case evidence shifts one reading's empirical footing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
