% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__legalization_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Legalized Substance Access with Third-Party Harm Regulation
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   Under the legalization reading of substance control legitimacy, competent
 *   adults retain autonomy over personal substance decisions within a legal
 *   market. The state's authority is limited to preventing third-party harms:
 *   impaired driving, secondhand exposure in shared spaces, and unsafe work
 *   conditions. This reading decomposes the original substance-control kernel
 *   into three structurally distinct constraints. The legalization reading
 *   shifts the victim set from all users (under prohibition) to third parties
 *   who bear externalities (under legalization). It also introduces corporate
 *   market operators as beneficiaries—a new seat created by legalization
 *   itself. The constraint is CLAIMED as tangled rope (genuine coordination
 *   of autonomy + third-party harm, plus asymmetric extraction via market
 *   operators) while the authored metrics describe moderately extractive
 *   operation with stabilizing but incomplete suppression. This divergence is
 *   intentional and measured by the engine.
 *
 * KEY AGENTS:
 *   - adult_substance_users: primary beneficiary (autonomy protected, exit mobile) — moderate power, biographical horizon
 *   - legal_market_operators: agenda-setter and secondary beneficiary (set product standards, extract rents) — institutional power, generational horizon, arbitrage exit
 *   - impaired_driving_third_parties: powerless victims (trapped in shared road system, bear accident risk) — powerless, immediate horizon
 *   - secondhand_exposure_affected_populations: constrained victims (bear physiological burden in shared spaces) — moderate power, biographical horizon, constrained exit
 *   - regulatory_enforcement_agencies: co-agenda-setter (license markets, enforce third-party harm rules) — institutional power, generational horizon, analytical exit
 *   - harm_reduction_advocates and prohibitionists: excluded, structurally absent from legalization governance — both organized power, both contest the autonomy premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.68).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.42).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Legalized Substance Access with Third-Party Harm Regulation").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, 'cdee180e-8a23-4d55-8978-14d5e34210d6').
narrative_ontology:cs_kernel_codification('cdee180e-8a23-4d55-8978-14d5e34210d6', fixed_text).
narrative_ontology:cs_authority_grounding('cdee180e-8a23-4d55-8978-14d5e34210d6', distributed).
narrative_ontology:cs_reading_relation('cdee180e-8a23-4d55-8978-14d5e34210d6', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdee180e-8a23-4d55-8978-14d5e34210d6', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('cdee180e-8a23-4d55-8978-14d5e34210d6', foundational, competent_adult_autonomy_over_substance).
narrative_ontology:cs_axiom_status(competent_adult_autonomy_over_substance, holdable).
narrative_ontology:cs_axiom_grounding('cdee180e-8a23-4d55-8978-14d5e34210d6', competent_adult_autonomy_over_substance, deontological).
narrative_ontology:cs_axiom('cdee180e-8a23-4d55-8978-14d5e34210d6', foundational, state_authority_limited_to_third_party_harm_prevention).
narrative_ontology:cs_axiom_status(state_authority_limited_to_third_party_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('cdee180e-8a23-4d55-8978-14d5e34210d6', state_authority_limited_to_third_party_harm_prevention, deontological).
narrative_ontology:cs_reference_frame('cdee180e-8a23-4d55-8978-14d5e34210d6', liberal_autonomy_framework).
narrative_ontology:cs_drift_state('cdee180e-8a23-4d55-8978-14d5e34210d6', mature_legalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cdee180e-8a23-4d55-8978-14d5e34210d6', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, legal_market_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, adult_users_autonomy_protected).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, impaired_driving_third_parties).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, secondhand_exposure_affected_populations).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, workplace_safety_dependent_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, adult_substance_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the legalization reading, competent adults retain autonomy over personal substance use. They access legal markets without criminalization. Their constraint is third-party harm prevention (cannot drive impaired, cannot expose others to secondhand effects in shared spaces). Exit looks like: relocating to different jurisdictions, choosing abstinence, or complying with harm-prevention rules while exercising personal choice. They benefit from decriminalization and legal market access; they pay through regulatory constraints on use in shared spaces.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, adult_substance_users, beneficiary,
    moderate, biographical, mobile, national).

% Licensed producers, retailers, and service providers in the legal market. They set product standards, pricing, marketing boundaries, and enforce age restrictions. They extract rents through brand differentiation and market consolidation—replacing the underground market that existed under prohibition. They also bear regulatory compliance costs, public health reporting requirements, and pressure from both market deregulation advocates (wanting lower barriers) and harm-reduction advocates (wanting stricter controls). Their agenda-setting power comes from operational control of the market; their arbitrage exit is shifting to different substances or jurisdictions.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, legal_market_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, legal_market_operators, beneficiary).

% Pedestrians, cyclists, other drivers, and passengers who face elevated risk from substance-impaired operators. They cannot opt out of shared road systems. Their constraint is third-party harm enforcement (sobriety checkpoints, DUI criminal penalties, vehicle ignition interlocks, education campaigns). They bear the cost of incidents—injury, death, property damage—without having chosen to enter the risk situation. Their only exit is geographic (relocate) or modal (use alternative transport), both costly and socially limited.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, impaired_driving_third_parties, payer,
    powerless, immediate, trapped, local).

% Inhabitants of shared indoor spaces (bars, restaurants, workplaces, rental housing) exposed to secondhand substance effects (smoke, vapor, aerosols). They have constrained options: leave the venue (economic/social cost), advocate for indoor air standards, or bear the physiological burden. Venues may be employment sites (high cost to leave), social gathering places (high social cost), or required access (medical settings, transportation). Their constraint is indoor-air standards enforcement and venue-operator compliance. They pay through exposure and advocacy effort; their exit is costly to exercise.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, secondhand_exposure_affected_populations, payer,
    moderate, biographical, constrained, local).

% Workers in safety-critical occupations (aviation, heavy equipment operation, healthcare, nuclear facilities, transportation) subject to workplace substance testing, impairment screening, and mandatory reporting. Under legalization, they retain autonomy over off-duty personal use but face constraints during work hours and between-shift windows. Their exit is occupational retraining (costly, often identity-fused for specialized workers); their constraint is workplace testing and impairment protocols. They pay through invasive monitoring and risk of job loss if impairment is detected or disclosed use conflicts with policy.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, workplace_safety_dependent_groups, payer,
    moderate, biographical, constrained, regional).

% State and local authorities that license legal markets, enforce age restrictions, prevent driving impairment, monitor secondhand exposure in public spaces, and maintain product safety standards. They administer the legalization framework itself—defining competency thresholds for autonomy, classifying actionable third-party harms, and setting enforcement penalties. They face resource constraints and political capture pressure from market operators (seeking deregulation) and harm-reduction advocates (seeking stricter intervention). Their analytical exit is policy analysis and research; their operational exit is political pressure.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, regulatory_enforcement_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Public health professionals, addiction medicine providers, and advocacy organizations that contest the legalization reading's autonomy framing. They argue that addiction, developmental vulnerability (youth), and impaired judgment narrow the set of 'competent adults' below the legalization threshold. They advocate for state authority to extend beyond third-party harm to include mandatory access to treatment, prevention infrastructure, and public health surveillance. They are structurally excluded from legalization governance; their voice would argue for mandatory harm reduction and public health authority over market access.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, harm_reduction_advocates, excluded,
    organized, biographical, constrained, national).

% Religious, moral, and law-enforcement constituencies that contest legalization's core premise. They argue substance use is inherently harmful and state authority derives from moral duty to prevent harm through criminalization and supply-side enforcement. They hold that autonomy over substance use is not legitimate because the harms outweigh the choice value. They are excluded from legalization governance; their political power lies in mobilization to overturn the reading through legislative change or jurisdiction switching.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, prohibitionist_constituencies, excluded,
    powerful, biographical, constrained, national).

% Epidemiologists, research institutions, and public health departments that track health outcomes under legalization: addiction incidence, overdose mortality, use disorder prevalence, cannabis-use disorder trends, alcohol-related disease burden, workplace safety incident rates, secondhand exposure health effects. They produce evidence for assessing whether the constraint's operation matches its stated legitimacy (autonomy + third-party harm prevention). Their analysis feeds policy adjustment, jurisdiction comparison, and international debate. They are analytical observers without direct stakes in the legalization governance.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, public_health_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Legalization coordinates adult autonomy with third-party harm prevention by replacing criminal enforcement (which targeted all use) with regulatory enforcement (which targets impairment and shared-space effects). The coordination problem solved: how to decriminalize personal choice while protecting third parties from non-consensual externalities. This is distinct from the harm-reduction reading's coordination problem (minimize total substance harm via treatment access) and the prohibition reading's (prevent all substance harm via criminalization).
% TRANSFER_FUNCTION: Moves revenue from legal market prices (paid by consumers) to: (1) legal market operators (pricing power, brand rents), (2) tax authorities (excise and sales tax on legal sales), and (3) regulatory agencies (licensing fees, enforcement budgets). In jurisdictions with legalization, the black market shrinks and underground producers lose rents. Users lose some of the price subsidy they received from black-market competition and bear the cost of regulation (higher prices, product taxes, testing requirements).
% ABSENT_VOICES: Harm-reduction advocates and prohibitionist constituencies are structurally excluded from legalization governance. Harm-reduction voices would argue the founding problem is ongoing substance harm and state authority should fund treatment and prevention, not just third-party protection. Prohibitionist voices would argue the founding problem is substance harm itself and state authority should prevent all use via criminalization. Both are excluded from the legalization decision table; their absence from consensus reflects the deep reading contest over legitimate state authority.
% DISAPPEARANCE_RATIONALE: If legalization and its third-party harm framework disappeared overnight, legal market operators would close or shift to black market, tax revenue would collapse, impaired driving enforcement would shift toward criminalization of all use (not just impairment), and the constraint itself would flip to prohibition or revert to harm-reduction governance. The arrangement is not self-sustaining; its persistence requires ongoing regulatory maintenance, market licensing, enforcement of age restrictions and driving impairment, and political commitment against prohibitionist reversal.
% FOUNDING_PROBLEM: Prohibition created cascading secondary harms: criminalized supply chains, incarcerated users (disproportionately poor and racial minorities), violent underground markets, enforcement mechanisms that treated all users as criminals regardless of harm level, and barriers to treatment access due to legal stigma. The legalization reading was built to solve the harms of prohibition itself, not the harms of substance use. The founding problem is: 'How do we decriminalize use while protecting third parties from externalities, without recreating prohibition's secondary harms?'
% FOUNDING_PROBLEM_CORROBORATION: Legalization advocates (market operators, civil liberties organizations, public health economists in jurisdictions with legalization) attest the founding problem (prohibition's secondary harms) is live and that legalization solves it via decriminalization and tax revenue reallocation. Harm-reduction researchers attest both are live: prohibition's harms persist AND substance use harms persist—legalization addresses one but not the other. Prohibitionist constituencies attest the founding problem is misidentified entirely—the real problem is substance harm itself, which legalization exacerbates by expanding market access. No consensus outside the benefiting parties. Evidence comes from jurisdictions with legalization (reduced incarceration, tax revenue, but also rising addiction rates in some populations). Interpretation is reading-indexed: legalization advocates cite decriminalization success; harm-reduction advocates cite addiction persistence; prohibitionists cite rising overdose rates. No outside corroborator with neutral stake.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The legalization reading generates extractiveness that rises over time (0.45 at t0 to 0.68 at t25) because legal market operators consolidate and brand differentiation creates rents decoupled from marginal cost. Theater ratio stays low-to-moderate (0.18–0.31) because third-party harm enforcement is real (DUI checkpoints, breath testing, ventilation standards) but market operators increasingly leverage the autonomy framing to resist stricter regulation. Suppression is moderate and flat (0.35–0.42) because legalization deliberately reduces coercive enforcement compared to prohibition, replacing criminal penalties with regulatory fines and market access conditions. Accessibility_collapse is below 0.5 (0.48) because legalization keeps alternatives visible: users can exit to different jurisdictions or different substances, and operators can arbitrage regulatory differences. Resistance is high (0.72) because harm-reduction and prohibitionist constituencies actively contest the autonomy premise and push back against market-friendly regulation. The measurement grid is shared across all three metrics (one time axis for all) so the temporal analysis is coherent.
 *
 * PERSPECTIVAL GAP:
 *   The reading creates three distinct seat experiences. Users see legalization as autonomy expansion with manageable constraints (third-party harm prevention). Market operators see legalization as rent extraction opportunity: they set prices, product mix, and marketing—the autonomy framing shields them from calls for tighter regulation. Third-party victims (impaired driving, secondhand exposure) see legalization as externality-transfer: their safety burden increased when criminalization (which suppressed total consumption) gave way to legal market expansion. Enforcement agencies face pressure from both directions: market operators demand deregulation, victims and harm-reduction advocates demand stronger intervention. The legalization reading doesn't resolve this—it structures it. The engine computes divergent types from this structural data: operators may compute snare (they set terms, extract rent, exclude alternatives); users may compute rope (genuine autonomy gain); victims may compute snare (they bear costs). This divergence is the measurement the corpus captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Adult users have beneficiary directionality (d near 0.0–0.3): they gain autonomy expansion, pay only third-party harm constraints (which are real but moderate). Market operators have target directionality (d near 0.7–0.9): they extract rent through pricing power and brand loyalty in a consolidated legal market; their 'agenda-setter' role gives them institutional power to shape regulation. Third-party victims have target directionality (d near 0.8–1.0): they bear non-consensual risk from impaired driving and secondhand exposure; their exit is trapped (road systems) or constrained (shared workplaces). Enforcement agencies sit near symmetric (d near 0.4–0.6): they coordinate the autonomy + harm-prevention tension, but also bear political pressure and capture risk from market operators. The beneficiary/victim declarations map this: beneficiaries = users + operators (both gain); victims = impaired driving third parties + secondhand exposure populations + workplace safety groups (all pay without choice in the risk situation). No directionality override needed—the derived values capture the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The legalization reading avoids mandatrophy collapse (founding_problem_status=contested, disappearance_verdict=world_rearranges) by grounding its legitimacy in a defensible autonomy norm ('competent adults have capacity to choose') rather than in a solved coordination problem. However, it faces mandatrophy pressure from two directions: (1) harm-reduction reading argues the founding problem (excessive substance harm) persists and legalization's exclusive focus on autonomy neglects first-party harms; (2) prohibitionist reading argues the legalization reading misidentifies the founding problem entirely—the real problem is substance harm itself, not criminalization. Both alternatives would claim the mandate has shifted but legalization's governance persists. Mandatrophy triggers a classification check: does the computed type (via seat divergence) match the claimed type? If operators compute snare and users compute rope, the seat divergence itself signals mandatrophy risk. Mandate obsolescence would appear as: rising theater_ratio (autonomy framing decoupled from harm prevention), users reclassifying as victims (autonomy gains offset by market consolidation), or enforcement agencies reclassifying as captured (regulatory capture by operators). The measurement series shows modest theater rise and stable suppression, suggesting mandatrophy is emerging but not yet advanced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competent_autonomy_boundary,
    'What makes an adult ''competent'' to exercise autonomy over substance use? Is the boundary set by age, cognitive capacity, addiction history, or something else?',
    'Neuroscientific evidence on decision-making capacity across lifespan, addiction severity, and substance type; court rulings on competency standards in substance cases; empirical tracking of outcomes for adults near the competency boundary.',
    'A narrow competency boundary (e.g., only those without addiction history, or only above age 25 when prefrontal development completes) would reduce the victim-target set and increase beneficiary set, lowering extractiveness and shifting classification toward rope. A broad boundary (age 18+ regardless of addiction or impairment history) supports the legalization framing but increases first-party harm risk and pressure from harm-reduction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competent_autonomy_boundary, conceptual, 'How to define competence for autonomy under legalization.').

omega_variable(
    third_party_harm_scope_creep,
    'Does ''third-party harm'' include only direct externalities (impaired driving, secondhand exposure) or also indirect costs (healthcare burden from addiction, social services for dependents, lost workplace productivity)?',
    'Policy evolution tracking: as legalization matures, watch whether regulatory scope expands from direct harm (DUI, ventilation) to indirect harms (mandatory treatment funding, productivity monitoring). Jurisdictions with narrow and broad interpretations provide natural experiments.',
    'Narrow scope (direct externalities only) supports the autonomy framing and keeps suppression moderate. Broad scope (all measurable harms) narrows autonomy and increases suppression, shifting toward harm-reduction reading and increasing theater_ratio as autonomy framing decouples from enforcement reality. Scope creep is how legalization can degrade into surveillance-backed quasi-prohibition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_party_harm_scope_creep, empirical, 'Whether third-party harm expands beyond direct externalities to encompass systemic costs.').

omega_variable(
    market_operator_capture_of_autonomy_framing,
    'Do legal market operators use the autonomy framing (''adults should decide for themselves'') as cover for resisting public health regulation and pricing controls?',
    'Track discrepancy between stated autonomy justification and actual regulatory positions: operators citing autonomy to oppose age-gating, warning labels, product testing, or price regulation would indicate capture. Compare operator rhetoric to academic autonomy defenses (which usually include public health guardrails).',
    'If operators capture the framing, extractiveness rises and theater_ratio climbs (autonomy decoupled from actual user choice) while suppression of harm-reduction advocacy increases. The constraint reclassifies from tangled_rope (genuine autonomy + harm prevention) toward snare (autonomy as cover for extraction). Mandatrophy pressure intensifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_operator_capture_of_autonomy_framing, empirical, 'Whether market operators instrumentalize autonomy framing to maximize extraction.').

omega_variable(
    reading_internalization_legalization_vs_harm_reduction,
    'As legalization matures, do users internalize the autonomy framing as a personal identity (''I am an autonomous substance user''), or do they remain sensitive to harm-reduction reframing (''I am someone choosing to manage addiction with professional support'')?',
    'Post-legalization survey data tracking user identity, willingness to engage treatment, and receptivity to harm-reduction messaging; comparison of user responses in legalization-majority jurisdictions vs. harm-reduction-majority jurisdictions.',
    'Strong autonomy internalization locks users into the legalization reading and reduces susceptibility to harm-reduction reframing. Weak internalization keeps the reading contestable. Identity-lock increases if autonomy becomes self-concept rather than policy choice. High identity-lock would increase exit barriers (users exit by rejecting their autonomy identity, not just by relocating) and shift classification toward snare-flavored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_internalization_legalization_vs_harm_reduction, empirical, 'Whether autonomy framing becomes internalized identity or remains as policy choice.').

omega_variable(
    sibling_reading_foreclosure_chain,
    'Does the legalization reading''s success foreclose the harm-reduction reading, or do they coexist stably across different institutional domains?',
    'Track whether legalization spreads to new jurisdictions and whether harm-reduction disappears from discourse and policy, OR whether legalization stabilizes in some jurisdictions while harm-reduction persists in others and in advocacy.',
    'Foreclosure (one reading eliminates the other) would indicate the kernel has split and the readings are not truly siblings. Stable coexistence would support the cs_structure declaration that they coexist_with each other. The stability or instability of coexistence determines the long-term resilience of the legalization reading itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_chain, conceptual, 'Whether legalization forecloses harm-reduction or coexists with it stably.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(subs_tr_t0, projected).
narrative_ontology:measurement(subs_tr_t3, substance_control_legitimacy__legalization_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(subs_tr_t3, observed).
narrative_ontology:measurement(subs_tr_t6, substance_control_legitimacy__legalization_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(subs_tr_t6, observed).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(subs_tr_t12, observed).
narrative_ontology:measurement(subs_tr_t18, substance_control_legitimacy__legalization_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement_basis(subs_tr_t18, observed).
narrative_ontology:measurement(subs_tr_t25, substance_control_legitimacy__legalization_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(subs_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(subs_be_t0, projected).
narrative_ontology:measurement(subs_be_t3, substance_control_legitimacy__legalization_reading, base_extractiveness, 3, 0.51).
narrative_ontology:measurement_basis(subs_be_t3, observed).
narrative_ontology:measurement(subs_be_t6, substance_control_legitimacy__legalization_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(subs_be_t6, observed).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement_basis(subs_be_t12, observed).
narrative_ontology:measurement(subs_be_t18, substance_control_legitimacy__legalization_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement_basis(subs_be_t18, observed).
narrative_ontology:measurement(subs_be_t25, substance_control_legitimacy__legalization_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(subs_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(subs_su_t0, projected).
narrative_ontology:measurement(subs_su_t3, substance_control_legitimacy__legalization_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement_basis(subs_su_t3, observed).
narrative_ontology:measurement(subs_su_t6, substance_control_legitimacy__legalization_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(subs_su_t6, observed).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement_basis(subs_su_t12, observed).
narrative_ontology:measurement(subs_su_t18, substance_control_legitimacy__legalization_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement_basis(subs_su_t18, observed).
narrative_ontology:measurement(subs_su_t25, substance_control_legitimacy__legalization_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(subs_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__legalization_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, impaired_driving_enforcement_framework).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, secondhand_substance_exposure_standards).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, workplace_substance_testing_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_legitimacy kernel. The kernel decomposes into three structurally distinct constraints with different beneficiary/victim sets and extractiveness values. Legalization_reading shifts the victim set from all users (under prohibition) to third-party externality-bearers (impaired driving, secondhand exposure). It also creates legal market operators as a new beneficiary seat. The three readings coexist_with each other—they are held by different constituencies and neither logically forecloses the others. They are linked by network.affects_constraints as constraint family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
