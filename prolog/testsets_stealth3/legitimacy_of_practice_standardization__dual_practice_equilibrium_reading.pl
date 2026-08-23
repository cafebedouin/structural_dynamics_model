% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual-Practice Equilibrium: Domain-Partitioned Practice Legitimacy
 *   domain: political history/institutional change
 *
 * SUMMARY:
 *   A modernizing state standardizes the public surface of life — a single
 *   official calendar for taxation, courts, schooling, and treaties;
 *   prescribed dress and procedure for officials and state occasions; legible
 *   administrative time — while tacitly ceding festivals, funerals,
 *   agricultural rites, and household observance to traditional authority on
 *   the old reckoning. The settlement consolidates into a dual-practice
 *   equilibrium: the bifurcation is expected to be permanent, no party
 *   anticipates convergence, and compliance in each domain is performed
 *   because the domain demands it, not because anyone accepts the other
 *   domain's practices as truer. This story instantiates the
 *   dual_practice_equilibrium_reading of the kernel
 *   legitimacy_of_practice_standardization; the sibling readings are separate
 *   constraints with their own epsilon, victims, and classifications, linked
 *   through network.affects_constraints. The epsilon referent is the standing
 *   partition arrangement itself, assessed by this reading's own lights — not
 *   the convergence either sibling would produce. The claimed type and the
 *   authored metrics are independent facts: the claim states the structure I
 *   believe true of the arrangement, the metrics state what I believe
 *   descriptively accurate of its operation, and the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - modernizing_state_bureaucracy: agenda-setter and principal beneficiary (institutional/arbitrage) — fixes official time, dress, and procedure for the public domain; collects fiscal and diplomatic standardization without policing private observance; can extend or retract its restraint at will
 *   - traditional_authority_holders: secondary beneficiary and co-administrator (organized/constrained) — run the ritual and agricultural calendar; their authority is legally untouched but bounded at the office door, and their congregations thin as official time structures daily life
 *   - ordinary_practitioners: principal payer (moderate/constrained) — households living on both sides of the line, carrying two calendars and two wardrobes, complying in public strategically and without a seat at the bargain
 *   - rural_farming_communities: payer with retained benefits (moderate/constrained) — keep the most from the private-domain settlement and pay the largest calendar-friction bill as festival dates slide against official seasons
 *   - urban_commercial_elites: beneficiary (powerful/mobile) — schedule commerce on the official calendar and keep observance at home at near-zero cost
 *   - foreign_treaty_partners: external beneficiary (institutional/arbitrage) — demanded the standardized surface as a condition of treaty revision and receive it while bearing none of the settlement's costs
 *   - full_convergence_reformers: excluded (both flanks) — total modernizers and ritual-primacy traditionalists alike, stripped of a seat by a settlement that treats their question as permanently closed
 *   - modernization_historians: analytical observer — comparative record of calendar reform, dress regulation, and ritual toleration across modernizing states; no stake in the settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.33).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.33).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual-Practice Equilibrium: Domain-Partitioned Practice Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political history/institutional change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'ffe1b55a-be96-41f1-aa91-afa2d48d4046').
narrative_ontology:cs_kernel_codification('ffe1b55a-be96-41f1-aa91-afa2d48d4046', implicit).
narrative_ontology:cs_authority_grounding('ffe1b55a-be96-41f1-aa91-afa2d48d4046', practice).
narrative_ontology:cs_interpretation_layer_present('ffe1b55a-be96-41f1-aa91-afa2d48d4046').
narrative_ontology:cs_reading_relation('ffe1b55a-be96-41f1-aa91-afa2d48d4046', legitimacy_of_practice_standardization__endogenous_displacement_reading, influences).
narrative_ontology:cs_reading_relation('ffe1b55a-be96-41f1-aa91-afa2d48d4046', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_axiom('ffe1b55a-be96-41f1-aa91-afa2d48d4046', foundational, domain_partitioned_practice_legitimacy).
narrative_ontology:cs_axiom_status(domain_partitioned_practice_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ffe1b55a-be96-41f1-aa91-afa2d48d4046', domain_partitioned_practice_legitimacy, conventional).
narrative_ontology:cs_axiom('ffe1b55a-be96-41f1-aa91-afa2d48d4046', foundational, strategic_compliance_sufficiency).
narrative_ontology:cs_axiom_status(strategic_compliance_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('ffe1b55a-be96-41f1-aa91-afa2d48d4046', strategic_compliance_sufficiency, conventional).
narrative_ontology:cs_reference_frame('ffe1b55a-be96-41f1-aa91-afa2d48d4046', settled_domain_partition_equilibrium).
narrative_ontology:cs_drift_state('ffe1b55a-be96-41f1-aa91-afa2d48d4046', post_consolidation_generational_turnover, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffe1b55a-be96-41f1-aa91-afa2d48d4046', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_commercial_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, foreign_treaty_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, ordinary_practitioners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_farming_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_farming_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the public side of the settlement: fixes the official calendar for taxation, courts, schools, and treaties; prescribes Western dress and procedure for officials and state occasions; and decides case by case which ceremonies count as public business. It collects predictable fiscal time and diplomatic credibility without having to police festivals, funerals, or household observance. Its restraint in the private domain is a choice it can revisit — it can extend decree into ritual life whenever it accepts the enforcement bill that would follow.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Elders, ritual specialists, temple and shrine custodians, and household heads who set festival dates, marriage and funeral observance, and agricultural rites on the old reckoning. The settlement leaves their calendar and ceremonies legally untouched so long as they stay out of official business. Their authority is real but bounded — it stops at the office door — and their congregations thin as work, schooling, and migration follow the official calendar.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authority_holders, agenda_setter).

% Households and workers who live on both sides of the line: official-calendar dates for pay, taxes, school, and contracts; the old reckoning for festivals, memorial days, and rites; a work wardrobe and a ceremonial wardrobe. They were never party to the bargain that drew the line — they comply in public because the domain demands it, not because they accept the official practices as better — and they carry the standing overhead of keeping two systems running. Defying the code in either direction costs them: open traditionalism at work invites sanction, and abandoning the festival calendar severs family and village obligations.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, ordinary_practitioners, payer,
    moderate, biographical, constrained, national).

% Villages whose agricultural and festival life runs on the old calendar while taxes, school terms, and market days run on the new one. The settlement preserves their ritual year, but the two reckonings drift apart — festival dates slide against the seasons when computed officially, children miss school for observances the state does not recognize, and elders must translate between the calendars for every official transaction. They keep the most from the private-domain settlement and pay the largest calendar-friction bill.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_farming_communities, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, rural_farming_communities, beneficiary).

% Merchants, bankers, and industrialists who schedule contracts, shipping, and credit on the official calendar and appear in Western dress in business and court settings, while keeping traditional observance at home. They get the standardized surface commerce needs at almost no cost to their private life, and they can relocate their households and observance across jurisdictions if any domain's rules harden against them.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, urban_commercial_elites, beneficiary,
    powerful, biographical, mobile, national).

% Treaty powers whose diplomats and merchants demanded a legible counterpart: a single official calendar for deadlines and agreements, recognizable procedure, and presentational codes at official functions. The settlement gives them exactly the standardized surface they asked for and requires nothing of them in return; their own archives document having made standardization a condition of treaty revision. They can redirect trade and recognition elsewhere if the surface they rely on degrades.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, foreign_treaty_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Would-be standardizers of the whole field, from both flanks: modernizers who want the official calendar, dress, and procedure to reach festivals and homes, and traditionalists who want the old reckoning restored in public life. Some are socially prominent — senior officials sympathetic to total reform, ritual elites who never accepted the settlement — but the arrangement gives them no seat: the line is treated as settled, and their projects are dismissed on both sides as provocation or nostalgia. Within this settlement's legitimacy structure they hold no lever; changing it would require reopening the founding bargain itself.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, full_convergence_reformers, excluded,
    powerless, biographical, constrained, national).

% Scholars comparing calendar reform, dress regulation, and ritual toleration across modernizing states — Meiji Japan, the Ottoman Tanzimat, Siam, late Qing reforms. They reconstruct who drew the line, who paid for keeping two systems running, and whether the bifurcation held, eroded, or was eventually overridden; they hold no stake in the settlement and can read both sides' ledgers.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernizing_state_bureaucracy).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates standardization authority by domain so that two rival legitimacy systems do not fight over every practice: the state gets a single official calendar, procedure, and presentational code for taxation, courts, schools, and diplomacy, while traditional authority keeps the festival, agricultural, and ceremonial calendar legally untouched. Each side enforces its own domain and forgoes the other, converting a total legitimacy contest into a bounded jurisdictional split.
% TRANSFER_FUNCTION: Moves compliance and maintenance labor from ordinary households to the settlement's two authority systems: households supply official-calendar compliance (taxes, schooling, work dress, contracts) and keep the old reckoning alive at their own expense, while the state collects fiscal and diplomatic standardization it would otherwise have to enforce against resistance, and traditional authorities collect a legally untouched ritual domain.
% ABSENT_VOICES: The households who pay the dual-maintenance overhead were never party to the bargain — the line was drawn between state officials and traditional elites, and those who live on both sides of it had no seat at the drawing. Full-convergence advocates from both flanks are likewise outside: the settlement treats the very question they exist to argue as permanently closed. Neither group's consent was sought, and the settlement's stability does not depend on it — which is precisely the strategic-compliance condition this reading describes.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, the legitimacy contest it suspended would reopen across every practice at once: the state would either push decree into festivals, funerals, and household observance — at enforcement costs its own founding generation judged prohibitive — or traditional reckoning would reflow into official business, breaking fiscal time, court scheduling, and treaty deadlines. Commercial scheduling and diplomatic procedure lose their standardized surface, and the distribution fight over whose calendar governs which day begins anew.
% FOUNDING_PROBLEM: A modernizing state needed one official calendar and one presentational code for taxation, courts, schooling, and treaty diplomacy, but could not afford to suppress the ritual and agricultural life of its population wholesale: enforcement across every village festival and household observance exceeded its fiscal and coercive means, and total standardization risked the unrest it was meant to prevent. The partition was the bargain that bought the state the surface it needed at a cost it could pay.
% FOUNDING_PROBLEM_CORROBORATION: Modernization historiography and period diplomatic archives corroborate the founding problem from outside the domestic beneficiary set: treaty powers' own records document making standardization a condition of treaty revision, and fiscal records document the administrative chaos of dual reckonings. Traditional authorities corroborate only the other half — they attest the ritual domain's persistence but describe the settlement as toleration, not as meeting a state necessity; no party outside the state bureaucracy attests the fiscal-necessity framing as still live.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.58: the settlement's costs land as a standing, compounding overhead on those with no seat at the bargain — two calendars tracked, two wardrobes maintained, code-switching labor in every official transaction, festival dates sliding against official seasons — while the state collects standardization it would otherwise have to purchase with enforcement. The burden is substantial but bounded: the same settlement subsidizes the ritual life it taxes, so it does not reach the profile of a pure taking. Suppression 0.33: the founding generation needed active force to hold the public domain against flanking resistance; as the code became legible and each side's deterrence self-sustaining, active intensity decayed — but the structural requirement persists (requires_active_enforcement is true), because strategic compliance nobody believes in would collapse if the domain structure were simply abandoned. Suppression here is structural (domain policing and boundary enforcement), authored as a raw property; only extractiveness is scaled by directionality and scope, and the engine owns that arithmetic. Theater 0.40: as enforcement receded, maintenance shifted toward performance — official dress functioning as costume, state participation in festivals as ceremonial theater, ritual acknowledgment of state authority as protocol — a rising share of the settlement's upkeep is staged rather than enforced. Accessibility_collapse 0.45: convergence in either direction remains fully conceivable and was repeatedly attempted historically; the settlement forecloses it politically, not conceptually, so alternatives stay partly accessible. Resistance 0.40: flanking pressure from convergence advocates on both sides persists, though the settlement itself suppresses resistance relative to the total-standardization counterfactual. The measurement series run on one shared grid (T=0,10,20,30,40,50,60) with all three metrics authored at every point: burden accumulates as the overhead embeds, theater rises as upkeep migrates from enforcement to performance, and suppression requirement decays as the equilibrium self-organizes.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently because they occupy structurally different positions in the same arrangement. From the state's seat, the settlement is its own successful statecraft: it obtained the fiscal and diplomatic surface it needed at a fraction of the suppression bill, and it retains the option to revisit its restraint. From the ordinary practitioner's seat, the same arrangement is a lifetime spent never fully in time — official at work, old-reckoning at home, fluent in both and at home in neither, with no seat at the bargain that drew the line. From the traditional authorities' seat, it is preserved sovereignty at reduced scale: the ritual domain survives legally intact while its demographic base drains toward official time. From the excluded reformers' seat, it is a foreclosed question — the settlement's defining move is to treat their project as unnecessary on both flanks. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the state (beneficiary plus agenda-setter, arbitrage-grade exit) sits near the beneficiary end — it collects compliance and controls the boundary; treaty partners and commercial elites are pure beneficiaries with mobility, sitting nearest of all. Traditional authorities derive low-moderate directionality as beneficiaries with constrained exit. The victim declarations map the payer seats, but the plain derivation would overstate their position: ordinary practitioners and rural communities appear as victims yet retain substantial settlement benefits (legally protected ritual life, the agricultural calendar they prefer), so the override moves the moderate atom down to 0.62 — targeted, but partly subsidized. The excluded reformers carry no beneficiary or victim data, so the canonical fallback would misplace them; the override moves the powerless atom to 0.70, reflecting that the settlement's upkeep extracts from them the legitimacy of their project — their alternative is what the boundary enforcement exists to suppress.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is refusing both available mislabels. The settlement is routinely narrated as prudential toleration — a costless live-and-let-live — which hides the compounding overhead levied on seatless households and the gains accruing to the state. It is also routinely narrated as pure cultural destruction — the state grinding tradition down — which hides the genuine coordination function: the partition averts a total legitimacy war, preserves real value on both sides, and is actively maintained by two enforcement systems rather than imposed by one. The tangled_rope claim holds both halves. On mandatrophy: the acute founding pressures — fiscal chaos from dual reckonings, treaty powers conditioning revision on standardization — have lapsed, so the founding problem status is contested rather than live; the arrangement now persists as equilibrium rather than necessity. If the founding problem is read as dead while the world still rearranges around the settlement (disappearance_verdict: world_rearranges), that mismatch is the capture/zombie signature the engine cross-checks against the computed theater path — the settlement may be drifting from bargain toward vestige even while both enforcement systems still operate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates one reading — the dual-practice equilibrium — of the contested kernel legitimacy_of_practice_standardization. Is the domain-partitioned settlement a stable equilibrium in its own right, or a transitional truce that one of the sibling readings will eventually absorb: endogenous displacement (private practices converging voluntarily until the partition empties) or exogenous override (the state decreeing standardization into the ritual domain)?',
    'Longitudinal tracking of both domains: whether private-domain practice converges voluntarily across generations without decree (sibling: endogenous displacement), whether the state extends decree across the boundary (sibling: exogenous override), or whether the bifurcation persists indefinitely with strategic compliance (this reading).',
    'The authored epsilon and classification describe the partition-as-equilibrium. If a sibling absorbs the case, the same observable arrangement is re-accounted as transition cost (displacement) or incomplete decree (override), and the payer seats'' position changes from equilibrium overhead to waiting-room burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'One-reading-of-kernel uncertainty: partition equilibrium versus transitional truce absorbed by a sibling reading.').

omega_variable(
    strategic_vs_internalized_compliance,
    'The reading''s signature claim is that compliance is strategic, not internalized. Has public-domain compliance — official calendar, work dress, procedure — remained strategic performance, or have generations raised entirely inside the settlement internalized it as natural?',
    'Cohort comparison of compliance behavior and stated preference across generations socialized wholly under the settlement, including behavior where enforcement is absent or unobserved.',
    'If compliance has internalized, the settlement''s suppression requirement is lower than authored and the case drifts toward the endogenous-displacement sibling; if strategic, the settlement remains enforcement-dependent and this reading''s classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_internalized_compliance, empirical, 'Whether the settlement''s public-domain compliance is performed or believed.').

omega_variable(
    dual_maintenance_cost_incidence,
    'Who actually bears the dual-maintenance overhead — two calendars, two wardrobes, code-switching labor — and is it concentrated on the households least able to carry it?',
    'Household expenditure and time-use records across the settlement''s domains, comparing the proportional cost of maintaining both systems by income and locale.',
    'Concentrated incidence on resource-poor households raises the effective burden on the payer seats beyond the authored scalar; diffuse incidence recasts the overhead as a broadly shared cost of the settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_maintenance_cost_incidence, empirical, 'Distributional incidence of the dual-practice overhead across households.').

omega_variable(
    boundary_migration_drift,
    'The settlement requires a public/private line, but boundary cases — civil registration of marriages, official funerals, school ceremonies, state participation in festivals — recur. Is the line stable, or does it migrate case by case toward the state''s side?',
    'Track boundary rulings and administrative practice over time: which ceremonies get reclassified as public business, and whether traditional authorities retain final say over any domain the state has entered.',
    'Sustained migration of the line converts the equilibrium into incremental exogenous override (the sibling reading) and would date the end of this reading''s applicability; a stable line supports the equilibrium classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_migration_drift, empirical, 'Whether the settlement''s domain boundary is stable or migrating toward state absorption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(legi_tr_t20, observed).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(legi_tr_t30, observed).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement_basis(legi_tr_t40, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(legi_tr_t50, observed).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(legi_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(legi_be_t20, observed).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(legi_be_t30, observed).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(legi_be_t40, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement_basis(legi_be_t50, observed).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(legi_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(legi_su_t20, observed).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(legi_su_t30, observed).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement_basis(legi_su_t40, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement_basis(legi_su_t50, observed).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 60, 0.33).
narrative_ontology:measurement_basis(legi_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'was practice standardization legitimate?' decomposes into three structurally distinct constraints over one kernel. This story authors epsilon for the standing partition arrangement as the dual-practice reading sees it: moderate, compounding overhead on seatless households, with real retained benefits on both sides. The sibling stories author epsilon for their own referents — voluntary-displacement arrangements (low extraction, change legitimated by adoption) and decree arrangements (high extraction wherever decree reaches ritual life). The readings are linked, not merged: each has its own epsilon, victim set, and classification; this file's network edges carry the family linkage, and the upstream/downstream structure runs through the partition reading, whose institutionalization changes the operating environment of both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, moderate, 0.62).
constraint_indexing:directionality_override(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
