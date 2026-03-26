% ============================================================================
% CONSTRAINT STORY: living_donor_coercion_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_donor_coercion_risk, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_donor_coercion_risk
 *   human_readable: Living Donor Coercion Risk in Organ Transplantation
 *   domain: medical/bioethics/healthcare_access
 *
 * SUMMARY:
 *   Living donor organ transplantation exemplifies extraction through medical
 *   and family normalization. The constraint operates at the intersection of
 *   organ scarcity (legitimate medical problem), economic vulnerability
 *   (structural feature of healthcare access in many societies), and familial
 *   obligation (internalized cultural norm). Legally framed as 'informed
 *   voluntary choice,' living donation structurally extracts from
 *   economically desperate individuals and family members under obligation
 *   pressure. The system's theater (informed consent protocols, medical
 *   evaluation, psychological screening) appears to protect autonomy but
 *   functions primarily to legitimize donation regardless of actual donor
 *   coercion context. The constraint's extractiveness has increased over the
 *   measurement interval as living donation has become normalized and
 *   systematized — transplant centers now actively recruit living donors, and
 *   donation is increasingly presented as a moral solution to organ scarcity
 *   rather than as a problematic extraction mechanism. The theater ratio
 *   reflects the gap between formal consent procedures (which donors undergo)
 *   and actual autonomy (which is constrained by desperation or obligation).
 *
 * KEY AGENTS:
 *   - Living Donor (economically vulnerable): Primary victim (powerless/trapped or identity_locked) — structurally coerced by economic desperation or family obligation; bears full risk of health complications
 *   - Living Donor (family member): Primary victim (powerless/identity_locked) — identity-locked through familial caregiving norms; structurally mobile but cannot exercise exit without becoming different person
 *   - Transplant Recipient: Beneficiary (institutional/arbitrage) — gains access to functioning organ and life extension; typically institutional position within healthcare system
 *   - Transplant Program / Medical Institution: Beneficiary (institutional/arbitrage) — benefits from living donor expansion (reduces wait times, improves outcomes metrics, increases patient volume); experiences constraint as coordination solution
 *   - Organ Procurement Organization: Beneficiary (institutional/arbitrage) — incentivized to expand living donor recruitment; benefits from donation rate increases
 *   - Informed Consent Protocol / Regulatory System: Piton actor (institutional/arbitrage) — maintains performative function; persists through legal requirement despite limited coercion prevention
 *   - Patient Advocacy Coalition: Organized victim/beneficiary (organized/constrained) — benefits from organ availability but constrained by ethical concerns about donor exploitation; conflicted perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_donor_coercion_risk, 0.62).
domain_priors:suppression_score(living_donor_coercion_risk, 0.75).
domain_priors:theater_ratio(living_donor_coercion_risk, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_donor_coercion_risk, extractiveness, 0.62).
narrative_ontology:constraint_metric(living_donor_coercion_risk, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(living_donor_coercion_risk, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_donor_coercion_risk, snare).
narrative_ontology:human_readable(living_donor_coercion_risk, "Living Donor Coercion Risk in Organ Transplantation").
narrative_ontology:topic_domain(living_donor_coercion_risk, "medical/bioethics/healthcare_access").

domain_priors:requires_active_enforcement(living_donor_coercion_risk).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_donor_coercion_risk, transplant_recipients).
narrative_ontology:constraint_beneficiary(living_donor_coercion_risk, transplant_surgeons).
narrative_ontology:constraint_beneficiary(living_donor_coercion_risk, organ_procurement_organizations).
narrative_ontology:constraint_victim(living_donor_coercion_risk, living_donors).
narrative_ontology:constraint_victim(living_donor_coercion_risk, economically_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY VULNERABLE DONOR (SNARE) — Faces material desperation (debt, medical costs, family survival needs) that makes organ donation appear as coerced choice. Legally 'voluntary' but structurally trapped by economic circumstances. No real exit: refusing donation means abandoning family member or losing critical income. Experiences maximum extraction.
constraint_indexing:constraint_classification(living_donor_coercion_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FAMILY-OBLIGATED DONOR (SNARE) — Structurally mobile (could refuse donation) but identity-locked through familial bonds and caregiving norms. Self-concept constituted as 'the one who saves the family member.' Exit would require abandoning identity as responsible family member. Experiences extraction through internalized obligation rather than external coercion alone.
constraint_indexing:constraint_classification(living_donor_coercion_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: TRANSPLANT PROGRAM (ROPE) — Experiences constraint as coordination mechanism: living donor transplantation solves organ scarcity problem, enables patient survival, creates medical success narratives. Benefits from expansion of living donor pool. Sees donation as consensual arrangement that benefits both parties.
constraint_indexing:constraint_classification(living_donor_coercion_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MIXED MOTIVE DONOR (TANGLED ROPE) — Motivated by both genuine altruism and financial compensation or cost-recovery. Experiences constraint as mixed: genuine benefit from helping recipient AND extraction through payment asymmetry or pressure to assist relative. Has some agency (could donate to stranger, could refuse) but faces significant social and economic constraints.
constraint_indexing:constraint_classification(living_donor_coercion_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INFORMED CONSENT PROTOCOL (PITON) — Regulatory requirement for living donor transplantation. Performs cognitive function (donors do receive information) but largely theater: donors under family/economic pressure cannot exercise real choice even when fully informed. Protocol persists through institutional inertia and legal requirement despite limited effectiveness in preventing coerced donation.
constraint_indexing:constraint_classification(living_donor_coercion_risk, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ADVOCACY COALITION (TANGLED ROPE) — Patient advocacy groups, transplant networks, and disability organizations see living donation as both coordinating solution (increases organ availability for patients in organ-scarce nations) and extractive (exploits economically vulnerable donors). Face constraints in reforming system without reducing transplant access.
constraint_indexing:constraint_classification(living_donor_coercion_risk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ORGAN SCARCITY NATURAL LAW (MOUNTAIN) — From civilizational scope, organ scarcity is presented as immutable biological constraint: insufficient cadaver organs to meet medical need. Living donation appears as natural solution to fixed shortage. However, the structural data reveals this as false summit: organ scarcity is partly contingent on procurement system design (opt-in vs opt-out), allocation criteria, and transplant rates. The constraint naturalizes an institutional choice as immutable law.
constraint_indexing:constraint_classification(living_donor_coercion_risk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_donor_coercion_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(living_donor_coercion_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(living_donor_coercion_risk, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_donor_coercion_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(living_donor_coercion_risk, TR),
    TR >= 0.70.

:- end_tests(living_donor_coercion_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. Living donation extracts from economically vulnerable and obligated donors through structural desperation and social pressure. The extraction has grown over time as donation has become systematized and normalized. The value reflects that the extraction is not total (some donors do have genuine choice), but is substantial and concentrated on powerless agents. Suppression (0.75): High. Multiple barriers prevent exit: economic desperation removes material alternatives; family obligation removes psychological alternatives; medical framing of donation as 'gift' and 'moral choice' removes cognitive alternatives; liability structures and stigma against refusing donation suppress acknowledgment of coercion. Theater ratio (0.58): Moderate-high. Informed consent protocols perform cognitive function (donors do receive information) but largely theater: the protocols cannot and do not prevent coerced donation when coercion stems from economic desperation or family obligation. Psychological screening and medical evaluation serve to legitimate the system rather than protect autonomy. Theater has increased as systems have professionalized consent procedures while extraction mechanisms remain unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint operates through normalizing extraction as medical coordination. Beneficiaries (transplant programs, recipients) experience genuine problem-solving (coordination). Victims experience structural coercion masked by consent theater. The informed consent protocol creates the gap by appearing to protect autonomy while actually legitimizing extraction. The analytical mountain view (organ scarcity as immutable) naturalizes what is partly a contingent institutional choice (procurement policies, allocation criteria, transplant culture development). The gap between powerless victim's snare and institutional beneficiary's rope is widest on this constraint because the system explicitly frames extraction as voluntary gift.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is derived from their structural benefit/harm relationship to the constraint. Economically vulnerable donors: beneficiary status = none (bear full cost), exit options = trapped (no material alternatives), power = powerless → d → 0.95 (near maximum target). Family-obligated donors: beneficiary status = none (bear full cost), exit options = identity_locked (structurally mobile but identity-fused), power = powerless → d → 0.89 (high target, lower than trapped because cognitive escape is theoretically possible if identity frame broke). Transplant programs: beneficiary status = yes (benefit from donor expansion), exit options = arbitrage (can source organs via multiple mechanisms), power = institutional → d → 0.05 (near full beneficiary). The directionality derivation explains why the beneficiary sees Rope and the victim sees Snare: their d values are opposite poles of the sigmoid function.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has strong mandatrophy resolution signatures. The beneficiary's Rope classification is genuine — transplant programs do solve the legitimate coordination problem of organ allocation. But the victim's Snare is equally genuine — economically desperate individuals are structurally coerced into donation. The mandatrophy resolves by showing these are not contradictory classifications but perspectival reads from opposite structural positions. The 'volunteerness' framing is coherent only from the beneficiary's vantage (this solves my coordination problem, the donor chose to help). From the victim's vantage, the system is a snare (I had no real choice, coercion was structural). The false summit risk is the analytical view that organ scarcity is immutable — but the empirical fact is that procurement policy, deceased donor registration systems, and transplant culture are all contingent choices. The mandatrophy prevents the system from claiming to be a natural law while exploiting economically vulnerable agents; it forces the recognition that living donation extraction is a policy choice, not a biological necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_detectability,
    'Can informed consent protocols reliably distinguish voluntary living donation from coerced donation when economic desperation and familial obligation are both present?',
    'Longitudinal outcomes analysis: comparing post-donation psychological outcomes, regret rates, and outcome trajectories between economically motivated vs altruistic donors; comparison of informed consent effectiveness across countries with different economic contexts',
    'If protocols cannot distinguish: current classification stands, constraint is snare. If protocols can reliably identify coercion and prevent it: reclassify as tangled_rope or scaffold with sunset through improved screening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_detectability, empirical, 'Whether informed consent protocols can detect and prevent coerced donation').

omega_variable(
    economic_desperation_threshold,
    'At what point does economic motivation transform ''altruistic choice'' into coerced extraction?',
    'Comparative analysis of donation motivations across income quartiles; survey data on donors'' perceived alternatives; assessment of whether donors would donate if economic circumstances improved',
    'If threshold is low (any significant financial benefit = coercion): snare classification strengthens, suppression increases. If threshold is high (only explicit payment or explicit threats = coercion): rope or tangled_rope classification more appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_desperation_threshold, conceptual, 'Threshold for economic motivation to constitute coercion').

omega_variable(
    family_obligation_vs_authentic_choice,
    'Is familial obligation to donate an authentic expression of the donor''s values or a structural coercion mechanism?',
    'Counterfactual analysis: tracking donors who refused to donate to relatives and their long-term outcomes and family relationships; comparison of choice satisfaction across cultures with different kinship obligation norms',
    'If authentic: identity_locked exit appropriately captures the structural dynamic. If coerced: reclassify as trapped exit, increasing chi and confirming snare. Identity lock vs trap distinction determines whether the binding is cognitive or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_obligation_vs_authentic_choice, preference, 'Whether family obligation to donate reflects authentic values or coercion').

omega_variable(
    supply_side_alternatives,
    'How much does living donation extraction depend on scarcity created by institutional choices vs immutable biological limits?',
    'Comparative study of transplant systems: countries with different procurement policies (opt-in/opt-out), allocation criteria, and deceased donor rates; analysis of whether living donation rates correlate with organ scarcity or with payment/family norm policies',
    'If high dependence on institutional design: constraint is snare but potentially reformable through system change. If high dependence on biological scarcity: mountain classification for underlying organ shortage is appropriate (though coercion snare remains distinct constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_side_alternatives, empirical, 'Whether living donation extraction is driven by scarcity or by institutional design').

omega_variable(
    paid_donation_harm_equivalence,
    'Does compensated living donation (paid for organs) increase coercion risk compared to uncompensated donation?',
    'Cross-national comparison: donation outcomes, regret rates, and donor health outcomes in countries with explicit payment (Iran, compensation models) vs altruistic-only systems; analysis of whether payment increases or decreases selection into economically vulnerable populations',
    'If payment increases coercion: supports snare classification, extraction explicitly visible. If payment reduces coercion by enabling ethical accounting: reframes as tangled_rope with transparent extraction rather than hidden coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(paid_donation_harm_equivalence, empirical, 'Whether paid donation increases coercion risk vs uncompensated donation').

omega_variable(
    post_donation_disability_accountability,
    'Are donors whose health is permanently damaged by donation systematically denied accountability or compensation by transplant systems?',
    'Longitudinal health tracking and disability outcomes for living donors; analysis of liability frameworks and successful compensation claims for donation-related injuries; comparison across countries and institutions',
    'If accountability is systematically avoided: suppression and extraction increase, snare classification strengthens. If accountability structures exist and function: constrains suppression metric and may reclassify as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_donation_disability_accountability, empirical, 'Whether transplant systems systematically avoid accountability for donor disability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_donor_coercion_risk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(donor_tr_t0, living_donor_coercion_risk, theater_ratio, 0, 0.42).
narrative_ontology:measurement(donor_tr_t5, living_donor_coercion_risk, theater_ratio, 5, 0.5).
narrative_ontology:measurement(donor_tr_t10, living_donor_coercion_risk, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(donor_be_t0, living_donor_coercion_risk, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(donor_be_t5, living_donor_coercion_risk, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(donor_be_t10, living_donor_coercion_risk, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_donor_coercion_risk, resource_allocation).
narrative_ontology:affects_constraint(living_donor_coercion_risk, organ_scarcity_procurement_policy).
narrative_ontology:affects_constraint(living_donor_coercion_risk, healthcare_access_economic_coercion).

% DUAL FORMULATION NOTE:
% Living donor coercion is downstream of organ scarcity constraints and healthcare access constraints. The upstream organ scarcity constraint (ε ≈ 0.15, Mountain or Tangled Rope depending on procurement system design) determines the structural pressure for living donation. The upstream healthcare access constraint (ε ≈ 0.55, Tangled Rope) determines economic vulnerability of donor population. Living donor coercion risk has its own extractiveness (0.62) reflecting the specific mechanism by which donor vulnerability is exploited. Each constraint in the family has distinct ε and should be evaluated separately, linked through network dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_donor_coercion_risk, institutional, 0.05).
constraint_indexing:directionality_override(living_donor_coercion_risk, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
