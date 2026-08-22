% ============================================================================
% CONSTRAINT STORY: cbdr_principle__voluntary_commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__voluntary_commitment_reading, []).

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
 *   constraint_id: cbdr_principle__voluntary_commitment_reading
 *   human_readable: CBDR Voluntary Commitment Reading: Nationally Determined Contributions with Technology Transfer as Primary Developed Obligation
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the CBDR kernel: the claim that
 *   common-but-differentiated responsibilities are discharged through
 *   voluntary, nationally determined contributions, with technology transfer
 *   as the primary developed-nation obligation. The standing arrangement
 *   under contest is the pledge-and-review architecture that crystallized
 *   after Copenhagen and was codified in the Paris framework: every state
 *   files a self-determined contribution, a facilitative transparency
 *   mechanism reviews it, and periodic stocktakes aggregate the results —
 *   with no quantified binding reduction schedule for any party and no
 *   compensation guarantee for climate damages. The sibling reading
 *   (historical responsibility: binding reductions proportional to cumulative
 *   emissions plus loss-and-damage financing) is a SEPARATE constraint
 *   authored in its own file, linked through network.affects_constraints; per
 *   the epsilon-invariance principle the two readings get different epsilon
 *   values, different victim sets, and different classifications, and this
 *   file hedges nothing across them. Claim/metric independence is deliberate:
 *   proponents claim the arrangement as pure coordination (voluntariness
 *   bought universality), while the authored metrics describe a structure
 *   with a real coordination core AND substantial asymmetric extraction — the
 *   engine measures that divergence.
 *
 * KEY AGENTS:
 *   - advanced_economy_governments: primary beneficiary and co-agenda-setter (institutional/arbitrage) — obtains exemption from binding schedules; can exit cheaply
 *   - emerging_economy_polluters: dual-positioned beneficiary-payer (institutional/constrained) — keeps growth policy space, absorbs impacts and diplomatic pressure
 *   - climate_vulnerable_developing_nations: primary target (organized/trapped) — bears adaptation and disaster costs without compensation guarantees
 *   - unfccc_secretariat: administrator (institutional/constrained) — runs the pledging, review, and stocktake machinery; compels no one
 *   - future_generations: diffuse target (powerless/trapped) — inherits the locked-in trajectory, represented only by proxies
 *   - displaced_climate_communities: excluded voice (powerless/trapped) — no direct standing in the funds or negotiations
 *   - binding_compensation_advocates: excluded voice (organized/trapped) — demands admitted to agenda, deflected from operative core
 *   - ipcc_assessment_body: analytical observer (institutional/analytical) — documents the pledge-implementation gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.62).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.43).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.43).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR Voluntary Commitment Reading: Nationally Determined Contributions with Technology Transfer as Primary Developed Obligation").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, '59b6c20a-7384-46ce-96fc-3199bdd9c8b3').
narrative_ontology:cs_kernel_codification('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', fixed_text).
narrative_ontology:cs_authority_grounding('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', lineage).
narrative_ontology:cs_interpretation_layer_present('59b6c20a-7384-46ce-96fc-3199bdd9c8b3').
narrative_ontology:cs_reading_relation('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', cbdr_principle__historical_responsibility_reading, influences).
narrative_ontology:cs_axiom('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', foundational, differentiation_without_binding_quantification).
narrative_ontology:cs_axiom_status(differentiation_without_binding_quantification, holdable).
narrative_ontology:cs_axiom_grounding('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', differentiation_without_binding_quantification, conventional).
narrative_ontology:cs_axiom('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', secondary, technology_transfer_as_primary_developed_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_as_primary_developed_obligation, holdable).
narrative_ontology:cs_axiom_grounding('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', technology_transfer_as_primary_developed_obligation, instrumental).
narrative_ontology:cs_reference_frame('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', sovereign_pledge_differentiation_framework).
narrative_ontology:cs_drift_state('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', contemporary_global_stocktake_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('59b6c20a-7384-46ce-96fc-3199bdd9c8b3', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, advanced_economy_governments).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, emerging_economy_polluters).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_developing_nations).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, emerging_economy_polluters).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, pledge_and_review_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, national_sovereignty_over_mitigation_allocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiating coalitions of wealthy industrialized states that shaped the pledge-based architecture and govern the finance funds' capitalization. Under this arrangement they take on no quantified, binding reduction schedule; their stated obligation is facilitating technology transfer, much of which moves through market-rate commercial channels. They can leave or downgrade participation at comparatively low cost, as demonstrated by past treaty-withdrawal episodes, and domestic electoral cycles reward avoiding economy-wide mandates.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, advanced_economy_governments, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, advanced_economy_governments, agenda_setter).

% Large, rapidly industrializing emitters that preserve policy space for growth because no binding cap applies to them under this reading. They simultaneously absorb severe domestic climate impacts, face intensifying diplomatic pressure to peak and decline, and would forfeit export-market access and diplomatic standing if they abandoned the framework altogether.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, emerging_economy_polluters, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, emerging_economy_polluters, payer).

% Coalition members such as island states and least-developed countries that finance adaptation, disaster recovery, and relocation largely from domestic budgets and concessional loans. Compensation arrives through voluntary funds without entitlement or guarantee, and contribution levels elsewhere determine how much harm they absorb. They cannot exit the physical impacts, and leaving the framework would cost them their seat in finance and technology channels.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_developing_nations, payer,
    organized, generational, trapped, regional).

% Treaty bureaucracy that maintains the contribution registry, runs transparency reviews and periodic global stocktakes, and convenes the annual conferences. Its mandate expands only by party consensus, it compels no one, and its operational continuity depends on the very pledging cycle it administers.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% People who will inherit the warming trajectory that current pledge levels lock in. They hold no seat in negotiations and act only through advocate proxies; nothing in the arrangement gives their interests a binding weight against present-day economic preferences.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, future_generations, payer,
    powerless, civilizational, trapped, global).

% Communities relocating after floods, droughts, and sea-level encroachment. Loss-and-damage claims are routed through state-mediated voluntary funds they cannot petition directly, and they appear in negotiations only through member-state patrons or accredited observers.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, displaced_climate_communities, excluded,
    powerless, immediate, trapped, local).

% Jurists, campaign networks, and vulnerable-state negotiators pressing for quantified liability tied to cumulative emissions. Their demand is admitted onto conference agendas but is structurally deflected into voluntary pledging channels; it has no place in the operative legal core of the arrangement, and no procedural route exists by which it could acquire one without unanimous consent.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, binding_compensation_advocates, excluded,
    organized, generational, trapped, global).

% Scientific assessment body whose reporting cycles document the gap between aggregate pledges and pathways consistent with agreed temperature goals. Its findings feed the stocktake process; it holds no enforcement power and its participation is epistemic rather than material.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, ipcc_assessment_body, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__voluntary_commitment_reading, advanced_economy_governments).
narrative_ontology:fixing_cost_class(cbdr_principle__voluntary_commitment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the participation problem that defeated binding allocation: by making each state's contribution self-determined, the arrangement achieves near-universal membership in a common accounting, reporting, and five-yearly stocktake framework, keeping every major emitter inside one system.
% TRANSFER_FUNCTION: Moves allocation discretion inward to each state; moves promised technology and finance from developed to developing nations through channels that are largely voluntary and partially delivered; and leaves adaptation and disaster costs with the nations least able to carry them.
% ABSENT_VOICES: Parties demanding binding compensation for loss and damage are present in the room but excluded from the operative core: their claims are heard, then redirected into voluntary funds with no entitlement attached. Displaced communities have no direct standing at all. Unanimity around the pledge format therefore reflects a procedural filter, not agreement among all affected seats.
% DISAPPEARANCE_RATIONALE: If the contribution-and-stocktake framework vanished overnight, climate diplomacy would fragment into rival blocs and ad hoc minilateral deals, existing finance and technology channels would close for lack of a hosting framework, national reporting would stop, and vulnerable-nation coalitions would lose their primary venue — the entire multilateral climate apparatus would have to be rebuilt around some other architecture.
% FOUNDING_PROBLEM: After binding-target approaches collapsed at the 2009 Copenhagen conference, the practical problem was designing an architecture that the United States, China, and other major emitters would actually join, given sovereignty objections and development-equity disputes — participation first, stringency later.
% FOUNDING_PROBLEM_CORROBORATION: Advanced-economy governments attest the participation problem was real and is solved, citing universal membership. Vulnerable-nation coalitions and independent sources corroborate the narrower genealogy while disputing sufficiency: IPCC assessment reports and UNEP Emissions Gap analyses — produced outside the benefiting parties — document that aggregate pledges diverge sharply from agreed temperature limits, and Climate Action Tracker independently grades pledge adequacy. No source outside the beneficiary set attests that the deeper problem the arrangement nominally serves (halting dangerous warming) is on track.
narrative_ontology:disappearance_verdict(cbdr_principle__voluntary_commitment_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__voluntary_commitment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__voluntary_commitment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__voluntary_commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__voluntary_commitment_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__voluntary_commitment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__voluntary_commitment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 at interval end because the arrangement's costs concentrate where its benefits do not: vulnerable nations finance adaptation from domestic budgets while the compensation channel stays voluntary, and developed nations' headline obligation (technology transfer) is substantially discharged through market-rate sales rather than concessional transfer. Suppression is 0.43 — the regime is famously sanction-free, so its coercive force is low in absolute terms, but the payer seat's alternatives are genuinely closed: exiting costs finance access and voice, and no rival forum exists. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope. Theater ratio is 0.55: the transparency and stocktake machinery does real work, but a growing share of activity is pledge announcement, net-zero declaration, and communiqué production decoupled from implementation — the pledge-implementation gap documented by independent trackers widens as headline ambition rises. Accessibility collapse is 0.50: inside the consensus framework the binding-allocation alternative is politically foreclosed, but the historical-responsibility program remains live outside it, so alternatives are half-collapsed, not eliminated. Resistance is 0.60: every conference cycle features organized contestation from both directions — vulnerable-bloc pressure for binding compensation and developed-state defense of voluntariness. The temporal series run on ONE shared grid (points 0,2,4,...,16) with every tracked metric authored at every point; the underlying dynamic is a rising trend with COP-cycle pulses (pledge announcements cluster before stocktakes), and the base_properties scalars reflect the interval-end state. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change: the transparency framework was built up from near-nothing at Copenhagen to an operational enhanced-transparency apparatus, a maturing-enforcement trajectory, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different types from identical structural facts. From the advanced-economy seat the arrangement is a coordination achievement it built and staffed: universality was purchased with voluntariness, and the deal is performing as designed. From the vulnerable-nation seat the same architecture operates as a structure that coordinates everyone's reporting while concentrating uncompensated costs on those least responsible and least able to adapt — participation without protection. The emerging-economy seat straddles the divide, which is why it carries dual roles. The engine computes this divergence from power, exit, and directional data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: advanced_economy_governments sit near the full-beneficiary end (d low) — the arrangement subsidizes them with avoided binding costs, and their arbitrage-grade exit pushes them further toward the subsidy end. emerging_economy_polluters derive a low-to-moderate d from their beneficiary role, tempered by constrained exit and their declared payer side; their dual position is captured structurally rather than by override, because the derivation reads the primary role and the secondary role together. climate_vulnerable_developing_nations sit near the full-target end (d high): they bear the transfer, their exit is trapped, and their coalition organization raises their power atom without improving their position inside the structure. future_generations are maximal-d by construction — powerless, trapped, and wholly outside the bargaining table. The secretariat sits near-symmetric: it administers the structure and collects no rents from its asymmetry. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct relationships, and the schema's override surface (keyed by power atom) cannot distinguish same-power seats better than the structural data already does.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Reading the arrangement as pure rope (its proponents' claim) would erase the identifiable victims — the uncompensated adaptation burden and the deflected compensation claims — and launder extraction as the price of universality. Reading it as pure snare would erase the genuine coordination function: universal participation in one accounting and stocktake framework is a real collective-action achievement that a coercive binding regime failed to secure, and the transparency machinery produces information no alternative currently produces. Tangled rope holds both: coordination function and asymmetric extraction operate through the same pledging structure, sustained by continuous administrative and diplomatic maintenance. On the R5 mismatch test: founding_problem_status is 'contested' (the narrow participation problem is arguably solved; the broader stabilization problem it serves is demonstrably unmet) paired with disappearance verdict 'world_rearranges', so no zombie flag fires — the arrangement's persistence tracks a live function, not a dead mandate wearing its costume.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of kernel cbdr_principle (reading: voluntary_commitment_reading). What structurally changes if the sibling reading (historical_responsibility_reading) is adopted instead?',
    'Comparative classification against the sibling story file: the sibling places developed nations in the victim set for binding emissions constraints and removes the uncompensated-adaptation victim position of developing nations (replacing it with compensated loss-and-damage claims); this reading does the reverse. The disagreement is located entirely in whether ''differentiated responsibilities'' entails quantified binding obligations.',
    'Adopting the sibling reading swaps the victim sets and would move the developed-economy seat from beneficiary to target, changing per-seat classifications wholesale; this file''s epsilon describes only the voluntary-commitment arrangement and must not be averaged with the sibling''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which kernel, which reading, what the sibling changes, where the disagreement sits.').

omega_variable(
    technology_transfer_delivery_gap,
    'Is the technology-transfer obligation — this reading''s primary developed-nation duty — actually being discharged, or is it moving predominantly through market-rate commercial channels that shift costs back to recipients?',
    'UNFCCC Technology Mechanism and CTCN delivery data, concessional-versus-commercial terms in transferred-technology flows, and recipient-country reporting on access costs.',
    'If transfer is substantively concessional, measured extraction falls and the coordination reading strengthens; if it is predominantly commercial, the obligation is nominal and effective extraction rises toward the sibling reading''s assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_delivery_gap, empirical, 'Whether the reading''s own promised consideration is delivered.').

omega_variable(
    voluntariness_ambition_counterfactual,
    'Does the voluntary form actually produce more aggregate mitigation than a feasible binding allocation would have — the core defense of this reading — or does voluntariness function as cover for under-allocation?',
    'Integrated-assessment counterfactual modeling of participation-weighted binding scenarios against observed NDC trajectories, plus participation-elasticity estimates from the Copenhagen failure record.',
    'If the counterfactual binding regime would have joined enough major emitters to beat observed pledge outcomes, the coordination function does not require the voluntary form and the extraction component stands closer to pure rent; if voluntariness is participation-necessary, part of the measured extraction is the genuine price of the coordination achieved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntariness_ambition_counterfactual, empirical, 'Whether the voluntary form is functionally necessary or strategically chosen.').

omega_variable(
    loss_damage_fund_convergence,
    'Will the evolving loss-and-damage fund architecture acquire de facto compensation-guarantee properties, pulling this reading''s victim structure toward the sibling reading''s?',
    'Track fund capitalization floors, disbursement entitlements, and whether contributor assessments become predictable or remain discretionary across replenishment cycles.',
    'If guarantees emerge, the uncompensated-adaptation victim position weakens and this reading converges structurally toward the historical-responsibility reading; if contributions stay discretionary, the victim set holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loss_damage_fund_convergence, empirical, 'Whether compensation is becoming guaranteed, changing the victim structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_vol_read_tr_t0, cbdr_principle__voluntary_commitment_reading, theater_ratio, 0, 0.34).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t0, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t2, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2, 0.37).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t2, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t4, cbdr_principle__voluntary_commitment_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t4, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t6, cbdr_principle__voluntary_commitment_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t6, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t8, cbdr_principle__voluntary_commitment_reading, theater_ratio, 8, 0.47).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t8, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t10, cbdr_principle__voluntary_commitment_reading, theater_ratio, 10, 0.49).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t10, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t12, cbdr_principle__voluntary_commitment_reading, theater_ratio, 12, 0.51).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t12, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t14, cbdr_principle__voluntary_commitment_reading, theater_ratio, 14, 0.53).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t14, observed).
narrative_ontology:measurement(cbdr_vol_read_tr_t16, cbdr_principle__voluntary_commitment_reading, theater_ratio, 16, 0.55).
narrative_ontology:measurement_basis(cbdr_vol_read_tr_t16, observed).

% Extraction over time
narrative_ontology:measurement(cbdr_vol_read_be_t0, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t0, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t2, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2, 0.49).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t2, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t4, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 4, 0.53).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t4, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t6, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t6, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t8, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t8, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t10, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t10, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t12, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t12, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t14, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 14, 0.61).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t14, observed).
narrative_ontology:measurement(cbdr_vol_read_be_t16, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(cbdr_vol_read_be_t16, observed).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_vol_read_su_t0, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t0, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t2, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2, 0.25).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t2, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t4, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t4, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t6, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t6, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t8, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t8, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t10, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t10, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t12, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t12, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t14, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 14, 0.42).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t14, observed).
narrative_ontology:measurement(cbdr_vol_read_su_t16, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 16, 0.43).
narrative_ontology:measurement_basis(cbdr_vol_read_su_t16, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, resource_allocation).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, cbdr_principle__historical_responsibility_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'CBDR' conflates two structurally distinct claims. The voluntary_commitment_reading (this file) instantiates self-determined contributions with technology transfer — epsilon 0.62, victims concentrated in uncompensated adaptation burden. The historical_responsibility_reading (sibling file) instantiates binding reductions proportional to cumulative emissions plus loss-and-damage financing — different epsilon, different victim sets, different research and negotiating communities. The upstream/downstream structure runs from this reading TO the sibling: the voluntary architecture's codification created the structural conditions (consensus procedure, pledge-format lock-in) that changed the sibling's operating environment, which is why the reading_relations edge is 'influences'. Each file links the other through network.affects_constraints; neither averages across the pair.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
