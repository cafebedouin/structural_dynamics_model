% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Disaster Preparedness: Competent Inspection, Ritualized Drills
 *   domain: institutional/safety/governance
 *
 * SUMMARY:
 *   Disaster preparedness regimes stratify into components that retain
 *   operational competence (engineering inspections of dams, bridges, nuclear
 *   facilities) and components that have degraded into ritual performance
 *   (mass evacuation drills, tabletop exercises, shelter-in-place
 *   simulations). The inspection regime is maintained by genuine technical
 *   necessity and external validation — a Mountain-like backbone. The drill
 *   regime persists through institutional inertia, compliance checkboxes, and
 *   liability theater — a Piton-like shell. The hybrid reading asserts this
 *   stratification is structural, not incidental: the same preparedness
 *   mandate produces both a competent core and a ritualized periphery, with
 *   extraction localized to the periphery via consultant fees, compliance
 *   software, and training contracts that have no operational payoff.
 *
 * KEY AGENTS:
 *   - inspection_agencies: Primary agenda_setter (institutional/arbitrage) — sets standards, collects fees, shields liability
 *   - compliance_consultants: Primary beneficiary (organized/mobile) — sells drill packages, certification software, after-action reports
 *   - frontline_responders: Primary payer (organized/constrained) — performs drills, absorbs opportunity cost, bears ritual burden
 *   - vulnerable_communities: Secondary payer (powerless/trapped) — experiences drill theater as substitute for material mitigation
 *   - municipal_budgets: Secondary payer (moderate/constrained) — funds both competent inspection and ritualized drills from same line item
 *   - liability_insurers: Beneficiary (institutional/arbitrage) — uses drill completion as underwriting proxy regardless of realism
 *   - engineering_corps: Observer (analytical/analytical) — maintains inspection competence, watches drill degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.38).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.22).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Disaster Preparedness: Competent Inspection, Ritualized Drills").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/safety/governance").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'af556983-06a9-4b1a-91bc-077922ac7a91').
narrative_ontology:cs_kernel_codification('af556983-06a9-4b1a-91bc-077922ac7a91', formalized).
narrative_ontology:cs_authority_grounding('af556983-06a9-4b1a-91bc-077922ac7a91', extraction).
narrative_ontology:cs_interpretation_layer_present('af556983-06a9-4b1a-91bc-077922ac7a91').
narrative_ontology:cs_reading_relation('af556983-06a9-4b1a-91bc-077922ac7a91', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('af556983-06a9-4b1a-91bc-077922ac7a91', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('af556983-06a9-4b1a-91bc-077922ac7a91', foundational, preparedness_is_stratified_not_unitary).
narrative_ontology:cs_axiom_status(preparedness_is_stratified_not_unitary, holdable).
narrative_ontology:cs_axiom_grounding('af556983-06a9-4b1a-91bc-077922ac7a91', preparedness_is_stratified_not_unitary, empirically_contingent).
narrative_ontology:cs_axiom('af556983-06a9-4b1a-91bc-077922ac7a91', secondary, extraction_localizes_to_ritualized_subsystems).
narrative_ontology:cs_axiom_status(extraction_localizes_to_ritualized_subsystems, holdable).
narrative_ontology:cs_axiom_grounding('af556983-06a9-4b1a-91bc-077922ac7a91', extraction_localizes_to_ritualized_subsystems, empirically_contingent).
narrative_ontology:cs_reference_frame('af556983-06a9-4b1a-91bc-077922ac7a91', unified_preparedness_mandate).
narrative_ontology:cs_drift_state('af556983-06a9-4b1a-91bc-077922ac7a91', post_911_homeland_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af556983-06a9-4b1a-91bc-077922ac7a91', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, inspection_agencies).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, compliance_consultants).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, liability_insurers).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, vulnerable_communities).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, municipal_budgets).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, institutional_continuity_doctrine).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, due_diligence_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets engineering inspection standards for critical infrastructure (dams, bridges, nuclear). Collects fees for certification and renewal. Controls the technical criteria that define compliance. Shields liability for certified facilities. Can jurisdiction-shop or privatize functions if political pressure mounts.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, inspection_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Sells evacuation drill packages, tabletop exercise facilitation, after-action report templates, and compliance tracking software to municipalities and agencies. Revenue scales with mandate scope, not drill realism. Can pivot to other compliance verticals (cyber, privacy, ESG) if preparedness market contracts.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, compliance_consultants, beneficiary,
    organized, biographical, mobile, national).

% Fire, EMS, and law enforcement personnel required to participate in mandated drills. Drills consume training hours that could go to operational skills. Professional identity fuses with 'being prepared' — refusing drills reads as dereliction. No individual exit without career exit. Collective bargaining has not secured drill reform.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_responders, payer,
    organized, biographical, identity_locked, regional).

% Low-income, elderly, disabled, and non-English-speaking populations in hazard zones. Experience drills as disruption without material benefit — no evacuation transport, no shelter capacity, no communication in their languages. Inspection regime ignores their housing stock (unreinforced masonry, mobile homes). Cannot relocate; no political voice in drill design.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, vulnerable_communities, payer,
    powerless, immediate, trapped, local).

% Single budget line funds both competent inspection (state-mandated, fee-supported) and ritualized drills (locally mandated, grant-dependent). Reallocation requires political will and inter-agency negotiation. Federal preparedness grants tie drill compliance to funding, creating perverse incentive to perform drills regardless of utility.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, municipal_budgets, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_non_agent(preparedness_persistence__hybrid_reading, municipal_budgets).

% Uses drill completion certificates and after-action reports as underwriting proxies for municipal and facility risk. Premium discounts awarded for compliance paperwork, not measured readiness. No incentive to audit drill realism — the paperwork is the product. Can reprice or withdraw coverage if mandate changes.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, liability_insurers, beneficiary,
    institutional, generational, arbitrage, national).

% Maintains the inspection regime's technical competence through peer review, failure analysis, and standard evolution. Watches drill degradation from outside the emergency management hierarchy. Publishes post-event forensics showing drill assumptions falsified by reality. No authority to restructure mandates.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_corps, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, compliance_consultants).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Engineering inspection coordinates genuine technical consensus on infrastructure failure modes, materials science, and risk thresholds — a real coordination problem solved by shared standards. Evacuation drills nominally coordinate multi-agency response and public familiarity, but the coordination function has atrophied: scripts are recycled, failures are not fed back, and no interoperability is tested.
% TRANSFER_FUNCTION: Moves municipal training budgets and responder hours from operational readiness to drill performance. Moves consultant fees from municipalities to compliance vendors. Moves liability risk from insurers to insureds via compliance proxies. Moves political credit from mitigation investment to drill participation counts.
% ABSENT_VOICES: Disaster survivors who experienced drill failures (e.g., Paradise CA, Lahaina HI) — their testimony is excluded from after-action processes. Community organizations in hazard zones — not invited to drill design. Independent researchers studying drill effectiveness — findings not integrated into mandate revision.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight: inspection regime would persist via industry standards, insurance requirements, and tort liability (Mountain backbone). Drill regime would collapse within 2 budget cycles — no constituency funds it voluntarily (Piton shell). Municipalities would reallocate drill hours to operational training. Consultants would pivot. Vulnerable communities would lose even performative attention but gain no material mitigation. The world rearranges asymmetrically.
% FOUNDING_PROBLEM: Post-WWII civil defense and Cold War nuclear preparedness created a unified mandate: credible population protection through exercised readiness. Engineering inspection was added later for technological hazards (dams, nuclear). The unified mandate assumed drills and inspections were the same kind of activity — practiced competence.
% FOUNDING_PROBLEM_CORROBORATION: The 2019 National Academies review 'Assessing Disaster Preparedness' found no correlation between drill frequency and survival outcomes. FEMA's own 2021 After-Action Reports for major disasters consistently note 'drill assumptions not met.' The inspection subsystem's founding problem (preventing catastrophic infrastructure failure) remains live per ASME and FERC testimony. No corroboration from outside the emergency management establishment supports the drill subsystem's founding problem.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38) reflects the hybrid structure: inspection extraction is near-zero (genuine coordination), drill extraction is moderate (consultant fees, wasted staff hours) but localized. Suppression (0.22) is low overall because inspection is voluntary-adopted by facility operators and drills are rarely actively enforced against communities — the constraint persists through institutional habit, not coercion. Theater ratio (0.55) is the signal: drills are >50% performative (scripted scenarios, no-fail outcomes, after-action reports filed and ignored). Accessibility collapse (0.42) is moderate: communities cannot exit the drill mandate, but facilities can choose inspection regimes. Resistance (0.35) is meaningful but fragmented: responders complain, auditors note gaps, but no coalition forms to restructure the mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the inspection agency seat, the constraint is a Mountain — technical standards, genuine coordination, negligible extraction. From the frontline responder seat, the drill component is a Piton — ritual maintained by institutional inertia, extracting time and credibility. From the vulnerable community seat, the whole preparedness edifice reads as a Snare — drills substitute for evacuation infrastructure, inspection ignores their housing stock. The engine computes these divergences from the structural data; the hybrid reading's claim (piton) captures the drill subsystem's degradation while the inspection subsystem remains Mountain-like.
 *
 * DIRECTIONALITY LOGIC:
 *   Inspection agencies and liability insurers sit at d ≈ 0.15 (beneficiaries: collect fees/premiums, control standards, arbitrage-grade exit via jurisdictional choice). Compliance consultants sit at d ≈ 0.2 (beneficiaries with mobile exit — they sell into the mandate but could pivot). Frontline responders sit at d ≈ 0.75 (payers: constrained exit — job requires drill participation, identity_locked through professional role). Vulnerable communities sit at d ≈ 0.85 (payers: trapped — no exit from geographic risk, no voice in drill design). Municipal budgets sit at d ≈ 0.6 (payers: constrained — must fund both regimes, limited reallocation power). Engineering corps sits at d ≈ 0.5 (analytical: symmetric, sees both regimes).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible disaster response capability) is partially live for inspection (dams still fail, bridges still collapse) but dead for mass drills (no evidence drills improve survival in actual events). The mandate persists because the inspection subsystem legitimizes the drill subsystem — 'we do preparedness' points to competent inspection to cover ritualized drills. This is mandatrophy: the drill component's function has atrophied but the unified mandate prevents its sunset. The hybrid reading detects this by refusing to classify the whole as Mountain (competence_reading) or the whole as Piton (husk_reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (hybrid_reading) of the contested kernel ''preparedness_persistence''. What structural elements distinguish it from sibling readings competence_reading and husk_reading?',
    'Compare the three readings'' beneficiary/victim structures, extraction localization claims, and institutional memory trajectories. The hybrid reading asserts stratified competence — engineering inspection remains Mountain-like while evacuation drills degrade to Piton-like ritual.',
    'If stratification cannot be empirically sustained, the reading collapses toward either competence_reading (all components competent) or husk_reading (all ritualized), changing the constraint''s type profile and extraction distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel reading identity and structural differentiation from siblings').

omega_variable(
    extraction_localization_boundary,
    'Is extraction truly localized to drill subsystems, or does inspection competence extract via credential gatekeeping and liability shielding?',
    'Trace inspection fee flows, credential renewal revenue, and liability outcomes for inspected vs. non-inspected facilities. Compare municipal budget allocations for drill compliance vs. inspection compliance.',
    'If inspection extracts via credential rents, the Mountain-like component carries hidden extraction, shifting the hybrid reading toward a more uniformly extractive profile (tangled_rope or snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_localization_boundary, empirical, 'Whether extraction is genuinely stratified or merely appears so').

omega_variable(
    drill_ritualization_mechanism,
    'Is drill ritualization driven by internalized suppression (responders believe drills work) or structural suppression (budget/time constraints make realistic drills impossible)?',
    'Post-drill surveys measuring perceived vs. actual readiness; track drill realism budgets over time; compare jurisdictions with mandated vs. voluntary drill regimes.',
    'If internalized, suppression persists after budget relief — the constraint carries its own reproduction mechanism. If structural, suppression drops when resources return.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_ritualization_mechanism, empirical, 'Structural vs. internalized suppression in drill ritualization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_pers_hybrid_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prep_pers_hybrid_tr_t5, preparedness_persistence__hybrid_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(prep_pers_hybrid_tr_t10, preparedness_persistence__hybrid_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(prep_pers_hybrid_tr_t15, preparedness_persistence__hybrid_reading, theater_ratio, 15, 0.49).
narrative_ontology:measurement(prep_pers_hybrid_tr_t20, preparedness_persistence__hybrid_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(prep_pers_hybrid_tr_t25, preparedness_persistence__hybrid_reading, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(prep_pers_hybrid_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(prep_pers_hybrid_be_t5, preparedness_persistence__hybrid_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(prep_pers_hybrid_be_t10, preparedness_persistence__hybrid_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(prep_pers_hybrid_be_t15, preparedness_persistence__hybrid_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(prep_pers_hybrid_be_t20, preparedness_persistence__hybrid_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(prep_pers_hybrid_be_t25, preparedness_persistence__hybrid_reading, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prep_pers_hybrid_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(prep_pers_hybrid_su_t5, preparedness_persistence__hybrid_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(prep_pers_hybrid_su_t10, preparedness_persistence__hybrid_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(prep_pers_hybrid_su_t15, preparedness_persistence__hybrid_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(prep_pers_hybrid_su_t20, preparedness_persistence__hybrid_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(prep_pers_hybrid_su_t25, preparedness_persistence__hybrid_reading, suppression_requirement, 25, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, infrastructure_inspection_mandate).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, emergency_management_funding).

% DUAL FORMULATION NOTE:
% This constraint decomposes the kernel 'preparedness_persistence' into stratified components. The competence_reading treats the kernel as uniformly Mountain (low extraction, negligible suppression). The husk_reading treats it as uniformly Piton (high theater, atrophied function). The hybrid reading asserts the kernel is not unitary — it contains a Mountain backbone (inspection) and a Piton shell (drills). Extraction (0.38) and theater (0.55) are composite metrics weighted by subsystem budgets and personnel-hours.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, organized, 0.2).
constraint_indexing:directionality_override(preparedness_persistence__hybrid_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
