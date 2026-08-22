% ============================================================================
% CONSTRAINT STORY: substance_control_authority__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_authority__harm_reduction_reading
 *   human_readable: Harm Reduction Regime over Personal Drug Use (Public Health Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   A state accepts that personal drug use will continue and organizes its
 *   authority around keeping users alive and uninfected: possession is
 *   depenalized below defined thresholds, sterile equipment and supervised
 *   consumption are provided, treatment linkage replaces arrest as the
 *   default contact, and commercial supply remains criminal. The arrangement
 *   solves a real collective-action problem (bloodborne epidemics, overdose
 *   mortality, public disorder) while leaving identifiable parties bearing
 *   costs through the same structure: users remain exposed to an unregulated
 *   toxic supply chain because the regime stops short of regulating markets,
 *   and the neighborhoods hosting services absorb concentrated amenity
 *   burdens. This story instantiates ONLY the harm_reduction_reading of the
 *   substance_control_authority kernel; the prohibition and legalization
 *   readings are separate constraint files with their own epsilon and victim
 *   sets, linked through the network block. Claim and metrics are authored
 *   independently: the tangled_rope claim reflects the judged structure
 *   (genuine coordination plus bounded asymmetric extraction), and the
 *   metrics describe the arrangement's observed operation without being tuned
 *   to any predicted verdict.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: dual-positioned principal (powerless/constrained) — net beneficiary of services and non-criminalization, residual bearer of unregulated-supply health harms
 *   - state_health_authority: agenda setter (institutional/constrained) — sets thresholds, licenses sites, defends the framework politically
 *   - public_health_service_sector: institutional beneficiary (institutional/identity_locked) — operates services, gains budget and mandate, professionally fused with the model
 *   - general_taxpayer_base: diffuse beneficiary (moderate/mobile) — funds services, receives reduced emergency and justice costs
 *   - service_district_residents: concentrated payer (moderate/constrained) — bears the localized disorder costs the coordination function generates
 *   - illicit_trafficking_networks: hidden beneficiary (organized/arbitrage) — retains the entire underground supply market the regime declines to legalize
 *   - rural_unserviced_users: excluded voice (powerless/trapped) — outside the service geography and the design conversation
 *   - epidemiological_research_community: analytical observer (analytical/analytical) — measures outcomes across competing arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__harm_reduction_reading, 0.4).
domain_priors:suppression_score(substance_control_authority__harm_reduction_reading, 0.3).
domain_priors:theater_ratio(substance_control_authority__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(substance_control_authority__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__harm_reduction_reading, "Harm Reduction Regime over Personal Drug Use (Public Health Reading)").
narrative_ontology:topic_domain(substance_control_authority__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__harm_reduction_reading, 'ec5758f1-003a-4bbd-b029-be8df5f623d9').
narrative_ontology:cs_kernel_codification('ec5758f1-003a-4bbd-b029-be8df5f623d9', formalized).
narrative_ontology:cs_authority_grounding('ec5758f1-003a-4bbd-b029-be8df5f623d9', expertise).
narrative_ontology:cs_interpretation_layer_present('ec5758f1-003a-4bbd-b029-be8df5f623d9').
narrative_ontology:cs_reading_relation('ec5758f1-003a-4bbd-b029-be8df5f623d9', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('ec5758f1-003a-4bbd-b029-be8df5f623d9', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('ec5758f1-003a-4bbd-b029-be8df5f623d9', foundational, health_outcome_supremacy).
narrative_ontology:cs_axiom_status(health_outcome_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('ec5758f1-003a-4bbd-b029-be8df5f623d9', health_outcome_supremacy, instrumental).
narrative_ontology:cs_axiom('ec5758f1-003a-4bbd-b029-be8df5f623d9', foundational, user_dignity_noncondemnation).
narrative_ontology:cs_axiom_status(user_dignity_noncondemnation, holdable).
narrative_ontology:cs_axiom_grounding('ec5758f1-003a-4bbd-b029-be8df5f623d9', user_dignity_noncondemnation, deontological).
narrative_ontology:cs_reference_frame('ec5758f1-003a-4bbd-b029-be8df5f623d9', public_health_stewardship_framework).
narrative_ontology:cs_drift_state('ec5758f1-003a-4bbd-b029-be8df5f623d9', contemporary_fentanyl_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec5758f1-003a-4bbd-b029-be8df5f623d9', '').
narrative_ontology:cs_kernel_id(substance_control_authority__harm_reduction_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, general_taxpayer_base).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, public_health_service_sector).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_authority__harm_reduction_reading, service_district_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__harm_reduction_reading, illicit_trafficking_networks).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, public_health_primacy_doctrine).
narrative_ontology:constraint_vindicates(substance_control_authority__harm_reduction_reading, decarceral_efficacy_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses opioids, stimulants, or other controlled substances. Personal possession falls below enforcement thresholds: no arrest, instead referral to care. Accesses sterile injecting equipment, naloxone, supervised consumption rooms, and substitution therapy. Continues buying from an unregulated street market whose potency and contamination no one controls. Stopping use means withdrawal, treatment waitlists, or relocating away from familiar supply and people; most stay.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__harm_reduction_reading, people_who_use_drugs, payer).

% Sets possession thresholds, licenses consumption sites, issues clinical guidelines, and reports outcome data to the legislature and press. Defends the framework annually against repeal bills and treaty-compliance complaints. Its discretion is bounded by statute, international conventions, and electoral cycles; it cannot unilaterally legalize supply or re-criminalize possession.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, state_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Operates needle exchanges, supervised consumption sites, substitution therapy programs, and outreach teams. Gains budgets, staffing, institutional mandate, and professional purpose from the model's continuation. Training pipelines, journals, and career ladders are built around harm reduction practice; senior figures have spent careers constructing it. Shifting to a different model would strand that accumulated institutional investment.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, public_health_service_sector, beneficiary,
    institutional, generational, identity_locked, regional).

% Funds the service network through municipal and national budgets. Receives the return in fewer ambulance callouts, slower HIV and hepatitis C incidence, lower prison populations, and reduced emergency-room burden. Also absorbs residual costs of visible street disorder and retail theft that persist alongside open drug scenes. Can relocate to jurisdictions with different arrangements at the cost of jobs and family ties.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, general_taxpayer_base, beneficiary,
    moderate, biographical, mobile, national).

% Live and run businesses around the blocks where services concentrate. Encounter discarded paraphernalia, public injecting, loitering, and occasional shoplifting daily. Citywide benefits reach them like everyone else, but the burdens land on their sidewalks specifically. Moving away means selling homes in a stigmatized market; staying means organizing against siting decisions.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, service_district_residents, payer,
    moderate, biographical, constrained, local).

% Supply the retail market that the arrangement leaves illegal. Because personal use is tolerated but sale remains criminal, the entire distribution chain stays underground and priced at prohibition premiums. Enforcement attention shifted toward large shipments leaves mid-tier operations comparatively stable. They adapt routes and formulations faster than regulators revise schedules.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, illicit_trafficking_networks, beneficiary,
    organized, biographical, arbitrage, continental).

% Use drugs in towns and counties where no exchange, consumption room, or substitution clinic operates. They face the older enforcement posture — possession arrests, court mandates — without the service layer the urban model provides. They are not represented on the advisory boards and municipal consultations where the framework's design is argued.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, rural_unserviced_users, excluded,
    powerless, immediate, trapped, regional).

% Tracks overdose mortality, bloodborne infection incidence, service uptake, and crime statistics across jurisdictions running different arrangements. Publishes comparative evaluations, advises committees, and supplies the evidence both defenders and opponents of the framework cite. Holds no operational stake in which model prevails.
narrative_ontology:constraint_stakeholder(substance_control_authority__harm_reduction_reading, epidemiological_research_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__harm_reduction_reading, illicit_trafficking_networks).
narrative_ontology:fixing_cost_class(substance_control_authority__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools sterile equipment, supervised consumption capacity, naloxone distribution, substitution therapy, and treatment linkage so that the infectious-disease and overdose externalities of drug use are managed once, centrally, instead of being borne separately by users, hospitals, police, and the public.
% TRANSFER_FUNCTION: Moves public money from taxpayers to service operators and clinical staff; moves arrest risk off users and onto commercial suppliers; leaves the health risks of an unregulated supply chain on users themselves; concentrates the amenity costs of open drug scenes on the neighborhoods hosting services.
% ABSENT_VOICES: Rural users without service access, recovery-community members who read the framework as abandoning cure, and residents consulted only after siting decisions are made. They stand outside the advisory boards and municipal consultations where service providers, user advocates, and health officials design the framework.
% DISAPPEARANCE_RATIONALE: Overdose deaths and new HIV and hepatitis C infections would climb back toward pre-intervention curves within years; tens of thousands of possession arrests would resume; the service workforce and site infrastructure would dismantle; illicit retail margins would widen as enforcement reverted to users. Municipal budgets would shift from health spending back to criminal justice spending.
% FOUNDING_PROBLEM: The HIV/AIDS crisis of the late 1980s was spreading rapidly through injection drug use, overdose deaths were mounting, and decades of enforcement-led control had filled prisons without reducing use. The framework was built to keep users alive and uninfected while accepting that use would continue.
% FOUNDING_PROBLEM_CORROBORATION: National vital-statistics agencies and WHO/CDC surveillance independently record the ongoing overdose and infection toll; coroners and police leadership publicly attest that enforcement alone has not reduced it; even prohibitionist scholars concede the epidemic's scale while disputing the remedy. The problem's persistence is attested from outside the service sector that benefits from addressing it.
narrative_ontology:disappearance_verdict(substance_control_authority__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__harm_reduction_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__harm_reduction_reading_tests).
:- end_tests(substance_control_authority__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.40): the regime transfers little wealth and punishes little, but it leaves users bearing preventable harms from an unregulated supply chain the political settlement declines to touch, and it concentrates uncompensated burdens on host neighborhoods. Suppression is low-moderate (0.30) and is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled in the engine's computation. Suppression here is structural, not internalized: continued supply-side criminalization, administrative sanction panels, and site-licensing discretion do the coercive work. Theater is low (0.20): the services do real work, though unevaluated pilots and symbolic decriminalization grow as the model institutionalizes. Accessibility_collapse is low (0.25) because prohibition and legalization remain fully live political alternatives — understanding this arrangement does not eliminate rival designs. Resistance is substantial (0.55): repeal campaigns, site-siting opposition, law-enforcement institutional culture, and treaty-compliance pressure all actively contest the framework. The temporal series run on one shared grid (points 0, 7, 14, 21, 28, 34, 40) with every tracked metric authored at every point; all three trajectories rise gently as the arrangement scales from grassroots exchanges to licensed infrastructure — extraction accumulates mildly with administrative layering, theater grows with institutionalization, and the enforcement/administrative machinery needed to hold the regime (threshold policing protocols, licensing, diversion panels) matures steadily, which is why suppression_requirement is traced rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience structurally different arrangements under the same rules. From the user seat the framework is a lifeline: services, non-criminalization, survival — a coordination structure they would defend. From the service-district resident seat the same structure is an imposed burden: citywide benefits, sidewalk-level costs, no exit that does not sacrifice home equity. From the trafficking-network seat the regime is market protection: the retained illegality of commerce preserves prohibition pricing for the one actor formally outside the law. From the authority seat it is mandate legitimation: budget, expertise, and a defensible public narrative. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Users appear in BOTH the beneficiary and victim arrays — the derivation should place them mid-range, slightly beneficiary-side (their net position is positive: life-saving services and non-arrest outweigh residual supply harms), reflecting the dual role rather than averaging it away. Taxpayers are declared beneficiaries with mobile exit, placing them near the subsidy end. Service-district residents are declared victims with constrained exit and local scope, placing them near the full-target end; the national scope of the framework modestly amplifies effective extraction on such trapped targets because verification of local burden-sharing is harder. Two overrides are declared where the derivation chain cannot see the true relationship. First, organized -> 0.15: trafficking networks appear in NO declaration array (they are formally criminalized), yet structurally they sit near the beneficiary pole because the retained supply prohibition protects their market — without the override they would receive a neutral or target-side d that misdescribes their position. Second, institutional -> 0.20: the two institutional seats (state_health_authority, public_health_service_sector) lack direct beneficiary/victim declarations and would fall to the canonical fallback; both in fact lean beneficiary (mandate expansion, budget, professional purpose), so the override fixes them near the subsidy end. No override is authored for the powerless atom because it would collide: users (net beneficiary-leaning) and rural unserviced users (pure target-side, receiving none of the benefits) share that atom and must derive differently from their own structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — epidemic-scale overdose and infection deaths that enforcement failed to stop — is live and intensifying in the fentanyl era, so no mandatrophy is declared and the R5 status x verdict pair (live x world_rearranges) is consistent: the arrangement persists because its problem persists, not out of inertia. The tangled_rope classification prevents two symmetrical mislabelings: prohibition advocates read the arrangement as a snare ('state-sponsored addiction'), which ignores the dominant genuine coordination function; uncritical advocacy reads it as pure rope, which ignores the real victim set (users exposed to unregulated supply, host neighborhoods bearing concentrated costs). The forward risk worth flagging is piton drift: the service sector's identity_lock (careers, journals, and institutions constituted by the model) plus growing theater would let the arrangement persist theatrically if a superior design emerged — the theater_ratio series is the early-warning indicator for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading of the substance_control_authority kernel — the harm reduction reading. Could the same standing arrangement be legitimately re-read under a sibling reading, and what would change?',
    'Re-read the arrangement''s operative rules at the boundary cases: if supply-side commerce becomes licensed and taxed, the arrangement crosses into the legalization reading''s constraint with a different epsilon and victim set; if possession thresholds collapse and arrests resume, it reverts toward the prohibition reading. Signals guiding the current choice: services are the primary mechanism and possession is depenalized, which is the harm reduction signature.',
    'Classification is indexical to the reading: the user victim set, third-party burden allocation, and epsilon all differ across siblings. Comparing this story''s computed verdicts to sibling stories'' verdicts without the reading index would fabricate disagreement where the structure merely differs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which reading of the substance-control kernel this arrangement instantiates, and what a sibling re-reading would change.').

omega_variable(
    supply_side_boundary_cost,
    'How much of the residual health harm users bear is produced by the retained illegality of the supply chain (toxic, variable street product) rather than by pharmacology itself?',
    'Compare user-cohort morbidity and mortality in jurisdictions that moved to regulated legal supply against matched decriminalization-only jurisdictions holding service levels constant.',
    'If most residual harm tracks unregulated supply, the arrangement''s effective extraction on the user seat is understated by this story''s metrics and the structure sits closer to enforced extraction than described; if the residual is pharmacological, it is intrinsic to accepted use and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_boundary_cost, empirical, 'Whether the user-side residual harm is a product of the regime''s supply-side boundary or of drug use as such.').

omega_variable(
    third_party_burden_contingency,
    'Are the concentrated neighborhood burdens around service districts an unavoidable physical consequence of concentrating services, or a contingent product of siting, sanitation, and enforcement choices?',
    'Within-city variation: compare districts with dispersed low-threshold services, managed consumption rooms, and active sanitation protocols against districts with open scenes and no amenities, controlling for baseline deprivation.',
    'If burdens are contingent, the third-party victim set shrinks under better administration and the arrangement moves toward pure coordination; if intrinsic, the victims are structural and the extraction asymmetry is a permanent feature of the design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_burden_contingency, empirical, 'Whether service-district victimhood is administrative or structural.').

omega_variable(
    service_sector_budget_capture,
    'Does the service sector''s budgetary and professional stake in the model''s continuation distort program design toward persistence over client outcomes?',
    'Audit programs continued past null or negative evaluations; track whether funding follows demonstrated outcome or established provider; examine whether pilot programs are ever allowed to fail.',
    'Confirmed capture would raise the agenda-setter and service-sector seats'' effective extraction, elevate theater_ratio over time, and push the arrangement toward degraded theatrical maintenance; its absence would support the coordination-first reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(service_sector_budget_capture, empirical, 'Whether the institutional beneficiary of the model captures its administration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__harm_reduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harm_reduction_reading_tr_t0, substance_control_authority__harm_reduction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(harm_reduction_reading_tr_t7, substance_control_authority__harm_reduction_reading, theater_ratio, 7, 0.07).
narrative_ontology:measurement(harm_reduction_reading_tr_t14, substance_control_authority__harm_reduction_reading, theater_ratio, 14, 0.09).
narrative_ontology:measurement(harm_reduction_reading_tr_t21, substance_control_authority__harm_reduction_reading, theater_ratio, 21, 0.12).
narrative_ontology:measurement(harm_reduction_reading_tr_t28, substance_control_authority__harm_reduction_reading, theater_ratio, 28, 0.15).
narrative_ontology:measurement(harm_reduction_reading_tr_t34, substance_control_authority__harm_reduction_reading, theater_ratio, 34, 0.18).
narrative_ontology:measurement(harm_reduction_reading_tr_t40, substance_control_authority__harm_reduction_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(harm_reduction_reading_be_t0, substance_control_authority__harm_reduction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(harm_reduction_reading_be_t7, substance_control_authority__harm_reduction_reading, base_extractiveness, 7, 0.27).
narrative_ontology:measurement(harm_reduction_reading_be_t14, substance_control_authority__harm_reduction_reading, base_extractiveness, 14, 0.3).
narrative_ontology:measurement(harm_reduction_reading_be_t21, substance_control_authority__harm_reduction_reading, base_extractiveness, 21, 0.33).
narrative_ontology:measurement(harm_reduction_reading_be_t28, substance_control_authority__harm_reduction_reading, base_extractiveness, 28, 0.35).
narrative_ontology:measurement(harm_reduction_reading_be_t34, substance_control_authority__harm_reduction_reading, base_extractiveness, 34, 0.38).
narrative_ontology:measurement(harm_reduction_reading_be_t40, substance_control_authority__harm_reduction_reading, base_extractiveness, 40, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(harm_reduction_reading_su_t0, substance_control_authority__harm_reduction_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(harm_reduction_reading_su_t7, substance_control_authority__harm_reduction_reading, suppression_requirement, 7, 0.13).
narrative_ontology:measurement(harm_reduction_reading_su_t14, substance_control_authority__harm_reduction_reading, suppression_requirement, 14, 0.16).
narrative_ontology:measurement(harm_reduction_reading_su_t21, substance_control_authority__harm_reduction_reading, suppression_requirement, 21, 0.19).
narrative_ontology:measurement(harm_reduction_reading_su_t28, substance_control_authority__harm_reduction_reading, suppression_requirement, 28, 0.23).
narrative_ontology:measurement(harm_reduction_reading_su_t34, substance_control_authority__harm_reduction_reading, suppression_requirement, 34, 0.26).
narrative_ontology:measurement(harm_reduction_reading_su_t40, substance_control_authority__harm_reduction_reading, suppression_requirement, 40, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__harm_reduction_reading, international_narcotics_treaty_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'drug policy' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of the substance_control_authority kernel. This story (harm_reduction_reading) authors the decriminalize-use/serve-harms arrangement: moderate epsilon, users dual-positioned, third parties bearing concentrated residuals, services as mechanism. The prohibition_reading authors the criminalize-use arrangement (high epsilon on users, different victim set, enforcement as mechanism); the legalization_reading authors the regulated-commerce arrangement (different extraction geometry entirely, market regulation as mechanism). The readings are linked via affects_constraints because they compete for the same statutory space and cite the same evidence base; the upstream evidence infrastructure this reading builds (supervised-site outcome data) feeds the legalization reading's legitimacy conditions, while treaty-regime constraints bind all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, organized, 0.15).
constraint_indexing:directionality_override(substance_control_authority__harm_reduction_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
