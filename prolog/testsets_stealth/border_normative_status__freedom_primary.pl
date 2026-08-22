% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Global Border-Control Regime as Impermissible Restriction of Movement (Freedom-Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This story instantiates the freedom_primary reading of the
 *   border_normative_status kernel. The standing arrangement under assessment
 *   is the global border-control regime: standardized passports, visa
 *   hierarchies, carrier sanctions, physical barriers, interception and
 *   pushback operations, detention and deportation machinery, and
 *   destination-funded externalization of enforcement to transit states.
 *   Assessed by this reading's own lights, that arrangement is a mass rights
 *   violation: freedom of movement is a fundamental human right, borders
 *   impermissibly restrict it, and the extraordinary justification that could
 *   license exclusion is absent for the origin-based, wealth-based mass
 *   allocation the regime actually performs. Constraint-family note: the
 *   colloquial question 'are borders legitimate?' decomposes into three
 *   structurally distinct stories over ONE shared referent with
 *   reading-indexed epsilon (OQ-26) — sovereignty_primary authors low epsilon
 *   over the same regime (it sees legitimate collective self-determination),
 *   qualified_sovereignty authors intermediate epsilon (it sees regulable
 *   policy), and this story authors high epsilon. Expected structural delta
 *   if this reading becomes operative: excluded migrants exit the victim set
 *   (no legitimate exclusion persists to place them there), displaced
 *   domestic workers in sheltered sectors enter it, and border enforcement
 *   converts from administration into rights violation requiring
 *   justification. Claim and metrics are independent authored facts: the
 *   snare claim is what this reading believes structurally true of the
 *   standing arrangement; the metrics are what it believes descriptively
 *   true; sibling seats will compute differently and that divergence is the
 *   datum.
 *
 * KEY AGENTS:
 *   - - destination_state_governments: Agenda setter (institutional/arbitrage) — writes visa rules, runs enforcement, externalizes coercion
 *   - - destination_state_citizens: Primary beneficiary (organized/constrained) — receives the access monopoly's wage and fiscal effects, funds enforcement, votes its severity
 *   - - border_enforcement_industry: Secondary beneficiary (institutional/arbitrage) — collects budgets that scale with enforcement mandates
 *   - - excluded_would_be_migrants: Primary target (powerless/trapped) — bears denied movement, foregone life chances, route risk
 *   - - refugees_and_asylum_seekers: Primary target (powerless/trapped) — bears pushback, externalized processing, refoulement risk
 *   - - transnational_families_separated_by_borders: Secondary target (moderate/constrained) — bears separation costs under sponsorship thresholds
 *   - - origin_state_governments: Dual-positioned (institutional/constrained) — remittance and pressure-release gains against brain-drain and readmission-complicity costs
 *   - - migrant_rights_advocates: Excluded voice (organized/constrained) — litigates and documents from outside the rooms where rules are written
 *   - - international_human_rights_bodies: Analytical observer (institutional/analytical) — articulates standards without admission-decision power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.85).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.86).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Global Border-Control Regime as Impermissible Restriction of Movement (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '3666ec1d-6a84-4966-9e16-729a70fce155').
narrative_ontology:cs_kernel_codification('3666ec1d-6a84-4966-9e16-729a70fce155', distributed).
narrative_ontology:cs_authority_grounding('3666ec1d-6a84-4966-9e16-729a70fce155', distributed).
narrative_ontology:cs_reading_relation('3666ec1d-6a84-4966-9e16-729a70fce155', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('3666ec1d-6a84-4966-9e16-729a70fce155', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('3666ec1d-6a84-4966-9e16-729a70fce155', foundational, movement_fundamental_right_presumption).
narrative_ontology:cs_axiom_status(movement_fundamental_right_presumption, holdable).
narrative_ontology:cs_axiom_grounding('3666ec1d-6a84-4966-9e16-729a70fce155', movement_fundamental_right_presumption, deontological).
narrative_ontology:cs_axiom('3666ec1d-6a84-4966-9e16-729a70fce155', foundational, exclusion_extraordinary_justification_burden).
narrative_ontology:cs_axiom_status(exclusion_extraordinary_justification_burden, holdable).
narrative_ontology:cs_axiom_grounding('3666ec1d-6a84-4966-9e16-729a70fce155', exclusion_extraordinary_justification_burden, deontological).
narrative_ontology:cs_reference_frame('3666ec1d-6a84-4966-9e16-729a70fce155', free_movement_presumption_baseline).
narrative_ontology:cs_drift_state('3666ec1d-6a84-4966-9e16-729a70fce155', contemporary_externalization_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('3666ec1d-6a84-4966-9e16-729a70fce155', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, destination_state_citizens).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, destination_state_governments).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, refugees_and_asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, transnational_families_separated_by_borders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, origin_state_governments).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, destination_state_citizens).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, origin_state_governments).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, movement_as_fundamental_right).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, equal_moral_status_of_persons_regardless_of_birthplace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set visa categories, admission quotas, and asylum-procedure rules; operate border agencies and negotiate externalization agreements with transit states. Gain electorally from visible control and diplomatically from cooperation deals. Can redirect enforcement outward or adjust categories unilaterally; their principal exposure is backlash from restrictionist or humanitarian constituencies at the next election.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, destination_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold membership in states whose labor markets, welfare systems, and electorates are bounded by admission control. Receive the wage and fiscal effects of restricted labor supply and vote on the regime's severity. Fund enforcement through taxation, and some compete with admitted migrants in specific sectors. Emigration would mean surrendering membership advantages, so even dissenters remain enrolled participants.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, destination_state_citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, destination_state_citizens, payer).

% Border agencies, detention operators, surveillance and biometric vendors, and contracting firms whose budgets scale with enforcement mandates. Revenue follows appropriations for barriers, patrols, processing, and detention capacity; portfolios diversify across jurisdictions so demand shocks in one country are absorbed elsewhere.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Seek to enter destination states for work, study, or family and are denied visas or intercepted en route. Bear foregone earnings measured in multiples of home wages, family separation, smuggling debts, and physical risk on irregular routes. Lawful channels are narrow, income-gated, or oversubscribed; turning back abandons the attempt, continuing means criminalized crossing.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Flee persecution, war, or state collapse and meet pushbacks at borders, externally processed claims in transit states, and multi-year waits in camps or detention. Protection turns on documentation and destination-state discretion; the alternative to waiting is the irregular route, with refoulement risk attached.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, refugees_and_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Households split by visa denials, income-sponsorship thresholds, and processing backlogs. Maintain relationships across distance at recurring cost; the member abroad absorbs care deficits while the excluded member absorbs foregone income. Regularization depends on the same admission rules that separate them.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, transnational_families_separated_by_borders, payer,
    moderate, biographical, constrained, global).

% Receive remittance inflows exceeding aid budgets in many cases and benefit from emigration as a safety valve for unemployment and dissent. Simultaneously lose trained professionals, sign readmission and border-control agreements under aid conditionality, and administer returnee reception. Bargaining position is weak against destination states that control visa access.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, origin_state_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, origin_state_governments, payer).

% NGOs, lawyers, faith groups, and scholars who litigate pushbacks, document deaths at borders, and campaign for expanded admission. Hold legal and moral arguments but sit outside the bilateral negotiations and interior ministries where admission rules are written; influence arrives late, through courts and crises.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, migrant_rights_advocates, excluded,
    organized, biographical, constrained, global).

% UN treaty bodies, regional courts, and special rapporteurs that articulate movement and non-refoulement standards and review state compliance. Findings carry interpretive authority but no direct power over admission decisions; leverage runs through reputational cost and domestic incorporation.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, destination_state_citizens).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes decisions about who may cross territorial boundaries: identity verification, security screening, and allocation of scarce admission slots are administered once, by states, rather than negotiated per movement. This reading grants that individualized screening for genuine threats coordinates something real; it denies that allocation by origin and wealth performs that function.
% TRANSFER_FUNCTION: Moves territorial access — and the life chances attached to it — from would-be entrants to incumbent members of wealthy states; moves enforcement budgets from taxpayers to enforcement institutions; moves asylum-processing burdens onto transit states through externalization deals.
% ABSENT_VOICES: The excluded themselves: would-be migrants have no vote in the destination polities that decide their access; transit-state populations absorb externalized enforcement without having consented to it; future cohorts inherit a birthplace lottery their predecessors' elections cemented. They object from outside every constituency in the room.
% DISAPPEARANCE_RATIONALE: If the border-control arrangement vanished overnight, labor markets, welfare-state financing, remittance economies, the enforcement industry, and externalization diplomacy would all reorganize within years; tens of millions would relocate toward opportunity, compressing wage gaps and forcing visible adjustment in destination states.
% FOUNDING_PROBLEM: Managing membership for territorially bounded political communities: after WWI, standardized passports and visa controls solved security screening, labor allocation, and democratic-boundary problems for states becoming bounded welfare democracies.
% FOUNDING_PROBLEM_CORROBORATION: Interwar diplomatic historians and IOM/ILO analysts, writing outside the beneficiary set, attest the original coordination problem was real. Human rights treaty bodies and sending-state governments attest that the arrangement as now operated exceeds any surviving version of that problem. No source outside the benefiting parties attests that origin-based mass exclusion remains proportionate to the founding problem — the strongest contemporary defenses come from the arrangement's beneficiaries themselves.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.85) because the reading measures the regime's yield in a fundamental liberty plus the attached life chances: confinement to the territory of one's citizenship is, on this seat, a continuous taking, and the welfare magnitude at stake dwarfs any coordination service the regime renders. Suppression is authored raw and unscaled (0.86) — only extractiveness is scaled by the engine — reflecting the machinery the regime requires: visa denial, carrier sanctions, pushbacks, detention, deportation, and externalized pullbacks; the arrangement cannot persist by acquiescence because its largest affected class never consented and cannot vote. Theater ratio (0.38) reflects a mixed body: interception and processing are functionally coercive, while barrier construction beyond demonstrated deterrence value, spectacle deportation flights, and proceduralism that launders refusal as queue management are increasingly performative. Accessibility collapse (0.68): alternatives do not vanish but narrow to asylum channels for the qualifying few, wealth-gated visas, lotteries, and the criminalized irregular route — understanding the regime does not reveal a workable lawful path for most movers. Resistance (0.62): migrant-rights movements, strategic litigation, rescue operations, sanctuary networks, and irregular crossing itself constitute persistent, costly resistance that the regime must continuously defeat. The measurement series run on one shared grid (points 0,5,10,15,20,25,30) with every tracked metric authored at every point; trajectories rise monotonically rather than cyclically — an enforcement-intensification ratchet (securitization after 2001-era shocks, externalization after 2015-era arrivals) layered on accumulating extraction, which should trip the T17 accumulation hypothesis for investigation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute opposite types from identical structural facts. From the excluded migrant's position the regime is experienced as a cage: total, coercive, unconsented. From the destination citizen's position it is experienced as the boundary of the demos: self-government, wage protection, welfare solvency. From the ministry's position it is a dial of statecraft adjustable by decree and purchasable abroad. The engine derives these per-seat classifications from the authored power/exit data; the divergence between seats is the measurement, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Trapped victims (excluded_would_be_migrants, refugees_and_asylum_seekers) sit near the full-target end: the regime takes movement and life chances and offers no lawful exit from the taking. Constrained victims (transnational families) sit just inside them. destination_state_citizens derive low-but-nonzero d: they collect the access monopoly's benefits but pay enforcement through taxation and bear sectoral competition, and their constrained exit keeps them enrolled rather than arbitraging. border_enforcement_industry, with arbitrage-grade exit across jurisdictions, sits nearest the beneficiary pole. destination_state_governments combine agenda-setting with direct collection. origin_state_governments carry a dual declaration (beneficiary with secondary payer): remittances and pressure-release offset brain drain and readmission complicity, placing them near symmetric. Observers are analytical and carry no stake. Global spatial scope amplifies effective extraction modestly for targets, since verification of exclusion's necessity at planetary scale is hardest exactly where the regime operates most absolutely.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare claim from this seat blocks two mislabels. Rope would book the regime's costs as coordination overhead and launder origin-based allocation as a service rendered — precisely the cover story this reading exists to strip. Piton would require inertia and decayed function, whereas this arrangement is actively intensified: enforcement budgets, barrier kilometers, and externalization treaties grow over the interval, the opposite of theatrical maintenance of a dead mandate. Mandatrophy status: the founding problem (membership management for bounded political communities) is contested rather than dead, so mandatrophy_resolved is deliberately left undeclared — the arrangement persists not because its mandate quietly expired but because its beneficiaries actively renew it. The classification therefore prevents the symmetrical error as well: reading the regime as pure timeless necessity (mountain-flavored) would erase the identifiable victims whose exclusion is the regime's product.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the border_normative_status kernel; would instantiating a sibling reading change the victim structure and classification of the same standing arrangement?',
    'Compare compiled victim sets across the three sibling stories: sovereignty_primary removes excluded migrants from the victim set entirely (exclusion is legitimate self-determination); qualified_sovereignty retains only disproportionately-excluded migrants; freedom_primary retains all excluded movers absent extraordinary justification.',
    'The same walls compute as legitimate self-governance, as regulable policy, or as rights violation depending on the reading seated. Cross-reading comparison is the measurement the corpus exists to take; within this reading, epsilon stays invariant over the fixed referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Reading-indexed classification of a shared kernel referent; sibling swaps flip victim sets.').

omega_variable(
    extraordinary_justification_boundary,
    'Where does the extraordinary-justification bar sit — which exclusions, if any, survive this reading''s own test (pandemic quarantine, individual fugitives from justice, verifiable carrying-capacity limits)?',
    'Case-by-case adjudication applying the reading''s test to candidate justifications, plus scholarly mapping of narrow versus broad exception classes.',
    'A narrow bar preserves the assessment of the standing regime as coercion with a thin coordination residue; a bar admitting broad origin-based exceptions collapses this reading toward qualified_sovereignty and shrinks the victim set accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_boundary, conceptual, 'Location of the exception bar internal to the freedom-primary reading.').

omega_variable(
    operative_world_distributional_shift,
    'Under this reading made operative, displaced domestic workers in sheltered sectors of destination states enter the victim set — do adjustment costs modify the accounting of who bears the arrangement''s burdens?',
    'Wage and employment studies of migration shocks (natural-experiment literatures on sudden labor-supply increases) estimating displacement incidence and adjustment speed.',
    'If displacement is concentrated and slow-adjusting, the operative reading-world acquires a new payer seat and the freedom-versus-adjustment tradeoff enters classification; if adjustment is fast and diffuse, the victim structure remains dominated by excluded movers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_world_distributional_shift, empirical, 'Structural delta consequence: domestic displacement victims under the operative reading.').

omega_variable(
    enforcement_theater_share,
    'What share of border-enforcement activity is performative sovereignty (barriers exceeding deterrence value, spectacle deportations, procedural processing that launders refusal as administration) versus functional prevention?',
    'Deterrence-effectiveness studies set against enforcement expenditure; audits of procedural volume against protection and admission outcomes.',
    'A higher performative share indicates the regime''s coordination story is increasingly cover even on its own terms; a lower share confirms dense functional coercion sustaining the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_theater_share, empirical, 'Functional versus theatrical composition of enforcement activity.').

omega_variable(
    externalization_liability_location,
    'When enforcement is externalized to transit states (destination-funded processing zones, pullback arrangements, offshore detention), where does responsibility for the resulting coercion sit?',
    'Litigation outcomes on pushback and non-refoulement, treaty text, and funding-conditionality records tracing operational control.',
    'Locating liability with funding destination states widens the target set to include proxy-run coercion; locating it with executing transit states fragments accountability and complicates the mapping from victims to the seats that set the agenda.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externalization_liability_location, conceptual, 'Attribution of coercive force under externalized enforcement architectures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__freedom_primary, theater_ratio, 0, 0.16).
narrative_ontology:measurement(bord_tr_t5, border_normative_status__freedom_primary, theater_ratio, 5, 0.19).
narrative_ontology:measurement(bord_tr_t10, border_normative_status__freedom_primary, theater_ratio, 10, 0.22).
narrative_ontology:measurement(bord_tr_t15, border_normative_status__freedom_primary, theater_ratio, 15, 0.26).
narrative_ontology:measurement(bord_tr_t20, border_normative_status__freedom_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement(bord_tr_t25, border_normative_status__freedom_primary, theater_ratio, 25, 0.34).
narrative_ontology:measurement(bord_tr_t30, border_normative_status__freedom_primary, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__freedom_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(bord_be_t5, border_normative_status__freedom_primary, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(bord_be_t10, border_normative_status__freedom_primary, base_extractiveness, 10, 0.67).
narrative_ontology:measurement(bord_be_t15, border_normative_status__freedom_primary, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(bord_be_t20, border_normative_status__freedom_primary, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(bord_be_t25, border_normative_status__freedom_primary, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(bord_be_t30, border_normative_status__freedom_primary, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__freedom_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bord_su_t5, border_normative_status__freedom_primary, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(bord_su_t10, border_normative_status__freedom_primary, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(bord_su_t15, border_normative_status__freedom_primary, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(bord_su_t20, border_normative_status__freedom_primary, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(bord_su_t25, border_normative_status__freedom_primary, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(bord_su_t30, border_normative_status__freedom_primary, suppression_requirement, 30, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, resource_allocation).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the legitimacy of borders' covers three structurally distinct claims and is decomposed into three stories sharing one referent. sovereignty_primary is the historically upstream reading — it fed the positive-law settlement (right to leave without right to enter) and is cited as settled background by the other two. qualified_sovereignty mediates: it accepts the sovereignty premise but subjects exercise to proportionality review. freedom_primary (this story) is downstream-contesting: it rejects the sovereignty premise's foundational status and re-derives the burden. Epsilon differs across the family because the readings differ, not because the referent varies; each story holds one stable epsilon over the fixed arrangement. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
