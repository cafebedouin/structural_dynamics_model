% ============================================================================
% CONSTRAINT STORY: ua_mobilization_protector_cert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ua_mobilization_protector_cert, []).

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
 *   constraint_id: ua_mobilization_protector_cert
 *   human_readable: Ukrainian 'Protector' Certificate Mobilization System
 *   domain: political/technological
 *
 * SUMMARY:
 *   The Ukrainian Protector certificate system represents a wartime digital
 *   mobilization infrastructure designed to track conscriptable population
 *   status, verify labor market participation, and coordinate rapid
 *   deployment. Launched in 2024 as an emergency measure, the system combines
 *   genuine military coordination needs (the state must identify available
 *   manpower) with powerful state extraction capacity (surveillance, movement
 *   control, mandatory participation). The constraint exhibits high
 *   suppression (0.72) through both technical barriers (biometric
 *   requirements, constant credential verification) and legal mechanisms
 *   (penalties for non-compliance, restrictions on employment and travel
 *   without valid certificates). Theater ratio (0.58) reflects the gap
 *   between the system's stated function (efficient mobilization
 *   coordination) and its actual operation (enforcement is visible but
 *   incomplete; elite evasion persists; administrative procedures are often
 *   performative). Extractiveness has risen from 0.52 at system launch to
 *   0.68 by 2026, driven by scope creep into labor market controls and the
 *   degradation of promised sunset mechanisms. The constraint manifests as
 *   pure extraction (snare) from the perspective of trapped conscriptable
 *   citizens, mixed coordination-extraction (tangled rope) from employers'
 *   perspectives, pure coordination (rope) from the central mobilization
 *   authority's perspective, and false natural law (mountain) from analytical
 *   observers who naturalize surveillance as inherent to state defense. The
 *   mandatrophy (χ > 0.70 requires resolution) is resolved through explicit
 *   acknowledgment that the system, while delivering genuine coordination
 *   benefits to state actors, imposes severe costs on citizens with no exit
 *   options and exhibits signs of degradation toward permanent surveillance
 *   infrastructure (piton trajectory).
 *
 * KEY AGENTS:
 *   - Conscriptable Male Citizens: Primary victims (powerless/trapped) — subject to mandatory participation, movement restrictions, mandatory credential verification; no legal exit short of emigration
 *   - Central Mobilization Authority: Primary beneficiary (institutional/arbitrage) — benefits from real-time population tracking, efficient manpower identification, deployment coordination; full control over system rules
 *   - Private Sector Employers: Secondary beneficiary (moderate/constrained) — gain workforce stability predictability and reduced mobilization shocks, but constrained by mandatory verification and hiring controls
 *   - International Military Aid Consortium: Temporary beneficiary (organized/arbitrage) — designed system for military coordination purposes; retains withdrawal option if system diverges from military necessity
 *   - Civil Society Rights Organizations: Organized opposition (organized/constrained) — advocate for legal protections; constrained by emergency justifications and enforcement theater; primary function degraded
 *   - Regional Labor Market: Diffuse victim (powerless/trapped) — unable to exit gatekeeping constraint; loses fluidity and cross-border mobility; no compensation mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ua_mobilization_protector_cert, 0.68).
domain_priors:suppression_score(ua_mobilization_protector_cert, 0.72).
domain_priors:theater_ratio(ua_mobilization_protector_cert, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, extractiveness, 0.68).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ua_mobilization_protector_cert, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ua_mobilization_protector_cert, snare).
narrative_ontology:human_readable(ua_mobilization_protector_cert, "Ukrainian 'Protector' Certificate Mobilization System").
narrative_ontology:topic_domain(ua_mobilization_protector_cert, "political/technological").

domain_priors:requires_active_enforcement(ua_mobilization_protector_cert).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, central_mobilization_authority).
narrative_ontology:constraint_beneficiary(ua_mobilization_protector_cert, state_security_apparatus).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, conscriptable_male_population).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, labor_market_fluidity).
narrative_ontology:constraint_victim(ua_mobilization_protector_cert, civil_liberties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTABLE CITIZEN (SNARE) — Male citizens subject to mobilization have no exit option from the Protector certificate system. Movement, employment, and travel are contingent on certificate status. Cannot avoid the constraint; extraction (mobilization obligation, surveillance cost) is maximal. High suppression via legal and practical barriers to geographic exit.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVATE SECTOR EMPLOYER (TANGLED ROPE) — Benefits from workforce stability predictability (the state certifies who is available) and reduced unplanned mobilization shocks. But constrained by mandatory credential verification, unable to hire undocumented workers, and subject to state enforcement audits. Mixed coordination (manages labor supply) and extraction (state control of hiring decisions).
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL MOBILIZATION AUTHORITY (ROPE) — Primary beneficiary. The certificate system solves a genuine coordination problem: identifying available manpower, verifying status in real-time, and reducing administrative overhead. The authority experiences the constraint as pure coordination tool with net benefit. High exit optionality through control over system rules.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIONAL LABOR MARKET (SNARE) — Unable to exit the digital gatekeeping constraint. Labor fluidity and cross-border mobility are reduced by certificate requirements. Workers cannot seek employment outside the tracking system. The labor market's flexibility is extracted; compensation (wage growth, job quality) does not rise to offset lost dynamism.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 5: CIVIL SOCIETY RIGHTS COALITION (PITON) — Organized resistance frames the certificate system as rights-violating surveillance infrastructure. But the coalition's primary function — advocacy for legal protections — has degraded: actual monitoring practices often exceed legal safeguards; enforcement is theatrical (publicized arrests for fraud while elite evasion proceeds). The constraint persists through institutional inertia and emergency justification, not because it solves the advocacy problem.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL MILITARY AID CONSORTIUM (SCAFFOLD) — NATO and allied military advisors designed the Protector system as a temporary coordination mechanism for the acute phase of conflict (2024-2026). The system has an implicit sunset: as conflict intensity declines and regular conscription resumes, the digital infrastructure is meant to transition to standard military records. International actors have arbitrage optionality — they can withdraw support and technical infrastructure if Ukraine's political use of the system diverges from military necessity.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational vantage, state mobilization capacity requires population tracking — the argument goes that any wartime state needs to know who is available for conscription. This perspective risks naturalizing the Protector system as an inevitable requirement of sovereignty and defense. However, the structural data contradicts the mountain classification: high suppression (0.72) and active enforcement requirement indicate contingent institutional design, not natural law. This is a false summit — a naturalized contingency.
constraint_indexing:constraint_classification(ua_mobilization_protector_cert, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ua_mobilization_protector_cert_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ua_mobilization_protector_cert, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ua_mobilization_protector_cert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ua_mobilization_protector_cert, TR),
    TR >= 0.70.

:- end_tests(ua_mobilization_protector_cert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The system extracts substantial value from the conscriptable population in the form of surveillance costs, movement restrictions, and forced labor availability. The 2-year trajectory from 0.52 to 0.68 reflects both system maturation (initial 0.52 was optimistic) and scope creep into labor market controls. The value is not maximal (0.85+) because some structural features remain partial: elite evasion occurs; enforcement is incomplete; international sunset clause is nominally present. Suppression (0.72): High. Technical barriers (biometric authentication, digital credential requirements) and legal barriers (employment penalties, travel restrictions, criminal liability) create substantial suppression. Exit cost for conscriptable males is extremely high — emigration (or elite-level corruption) are the primary exit routes. Theater ratio (0.58): Moderate-high. The system maintains visible enforcement (publicized arrests for fraud, credential revocation ceremonies) while actual prevention effectiveness is unclear due to endemic fraud and elite evasion. The theater appears to be increasing as the system matures — more elaborate enforcement procedures with less proportional impact. Claimed type (snare): Justified by high extractiveness, high suppression, and beneficiary-victim asymmetry. The system is designed to solve a real coordination problem (military mobilization) but its implementation has characteristics of extraction: asymmetric burden distribution, limited exit options, and institutional inertia toward permanence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The central authority sees a well-functioning coordination mechanism (rope) — the certificate system genuinely solves the problem of identifying available manpower and reducing deployment lags. Employers see a mixed coordination-extraction system (tangled rope) — they benefit from workforce predictability but are constrained by state control. Conscriptable citizens see pure extraction (snare) — no benefit to offset the surveillance, movement control, and forced availability. International actors see a temporary coordination scaffold (scaffold) — designed for the acute conflict phase with implicit sunset. Civil society sees a degraded rights violation system (piton) — the legal framework for protecting conscientious objectors exists but is increasingly theatrical; enforcement proceeds with minimal regard for the stated protections. The analytical observer risks seeing naturalized necessity (mountain) — that states require population tracking for defense — which is contradicted by the structural data showing high suppression and active enforcement, indicating contingent institutional design rather than immutable law. This perspectival gap is the primary diagnostic marker that the constraint is not a mountain but rather a snare with false natural law narratives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions within the mobilization flow. Conscriptable citizens occupy the position of full targets: they are victims with no exit optionality (d → 0.95, f(d) → 1.42, high experienced χ). The central authority occupies the position of full beneficiary: they receive the coordination value with arbitrage exit optionality (d → 0.05, f(d) → -0.12, negative experienced χ). Employers occupy intermediate positions: they are partial beneficiaries (workforce predictability) but also constrained (hiring controls), so d ≈ 0.45-0.55 depending on sector. International actors have arbitrage exit, placing them at low d despite organization level. Civil society organizations face trapped constraints (cannot exit advocacy function) with powerless institutional status (can advocate but cannot block system), yielding d ≈ 0.65. The regional labor market is abstract and powerless with no exit mechanism (d → 0.95). The directionality pattern shows a sharp asymmetry: institutional beneficiaries at low d, conscriptable population at very high d, creating the snare signature.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy at χ ≈ 0.73-0.78 (depending on perspective and scope modifiers). The mandatrophy is resolved through multi-level recognition: (1) The system delivers genuine coordination benefits to state actors and employers — this is not false extraction dressed as coordination, but real coordination mixed with severe extraction from other perspectives. (2) The asymmetry is extreme and structural: conscriptable citizens bear >90% of the cost with near-zero coordination benefit, while the authority reaps >90% of the benefit with near-zero cost. This asymmetry exceeds the coordination threshold and confirms the snare classification. (3) The scope has expanded beyond military coordination into labor market control, indicating mission creep and institutional inertia toward permanence. (4) The theater ratio (0.58) is elevated, suggesting that enforcement is becoming increasingly performative relative to actual prevention. (5) International actors retain arbitrage exit (conditional support), which distinguishes this snare from a total trap but does not resolve the mandatrophy — it only clarifies that the snare is constrainable rather than absolute. The resolution concludes: This is a genuine snare with coordination components (not pure extraction), but the extraction vastly exceeds the coordination value and is borne by trapped agents with no exit optionality. The system is resolvable only by (a) genuine sunset enforcement, (b) equitable burden-sharing (elite inclusion in conscription, not just evasion), or (c) genuine conscientious objector pathways. Absence of all three mechanisms confirms mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    certificate_scope_creep,
    'Does the Protector certificate system remain limited to military mobilization, or does it expand to labor control, movement restrictions, and social benefit allocation?',
    'Historical tracking of system scope; analysis of secondary legislation and enforcement guidelines; comparison of stated legal scope vs actual administrative use',
    'If scope remains limited: certificate system is scaffold (temporary, military-specific). If scope expands: system becomes snare (permanent surveillance infrastructure). Scope creep would increase extractiveness from 0.68 to 0.85+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certificate_scope_creep, empirical, 'Degree of mission creep beyond military mobilization').

omega_variable(
    international_sunset_enforcement,
    'Will the international military aid consortium actually enforce the sunset clause by withdrawing technical/logistical support when the acute conflict phase ends?',
    'Monitoring of international statements on system duration; analysis of NATO technical support agreements; observation of decommissioning timelines in post-conflict scenarios',
    'If sunset enforced: scaffold classification confirmed, high plausibility of system termination. If sunset not enforced: system transitions to permanent piton (degraded surveillance theater). Sunset failure would reveal that international actors have arbitrage exit in principle only.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_sunset_enforcement, conceptual, 'Whether international partners will enforce the system''s sunset').

omega_variable(
    elite_evasion_rates,
    'What percentage of elite/politically connected males successfully evade Protector certificate obligations compared to non-elite populations?',
    'Statistical analysis of mobilization rates by socioeconomic quintile and political affiliation; investigation of exemption grant patterns; analysis of deployment outcomes',
    'If evasion rates are equal: system functions as neutral coordination tool (snare classification confirmed equally across groups). If elite evasion >> non-elite: extraction is asymmetric and regressive, confirming snare + rights violation framing. Evasion differential > 3x would increase mandatrophy urgency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(elite_evasion_rates, empirical, 'Differential evasion rates by social class').

omega_variable(
    certificate_fraud_prevalence,
    'What is the actual rate of Protector certificate fraud (fake status updates, bribed officials, stolen credentials) relative to the theater of enforcement?',
    'Audit of certificate issuance procedures; investigation of documented fraud cases; comparison of enforcement arrests vs estimated fraud volume; analysis of insider threat reports',
    'If fraud is widespread: certificate system is piton (theatrical enforcement masking actual chaos). If fraud is rare: system is effective snare (genuine tracking + suppression). Theater ratio (0.58) suggests moderate fraud; empirical resolution would clarify whether theater exceeds actual functionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certificate_fraud_prevalence, empirical, 'Rate of certificate fraud relative to enforcement theater').

omega_variable(
    conscientious_objector_pathway,
    'Does Ukraine''s legal framework provide a real, accessible alternative to military service for conscientious objectors, or is the alternative (civilian service) substantially more burdensome?',
    'Comparative analysis of military vs civilian service requirements; documentation of conscientious objector case outcomes; assessment of actual accessibility of alternative pathways',
    'If alternative is genuinely accessible and comparable: exit_options for some conscriptable population would upgrade from ''trapped'' to ''constrained'', changing chi calculation. If alternative is illusory: trapped classification confirmed, snare strengthened. This omega affects mandatrophy severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conscientious_objector_pathway, empirical, 'Real accessibility of conscientious objector pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ua_mobilization_protector_cert, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ua_prot_tr_t0, ua_mobilization_protector_cert, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ua_prot_tr_t1, ua_mobilization_protector_cert, theater_ratio, 1, 0.53).
narrative_ontology:measurement(ua_prot_tr_t2, ua_mobilization_protector_cert, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(ua_prot_be_t0, ua_mobilization_protector_cert, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ua_prot_be_t1, ua_mobilization_protector_cert, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(ua_prot_be_t2, ua_mobilization_protector_cert, base_extractiveness, 2, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ua_mobilization_protector_cert, enforcement_mechanism).
narrative_ontology:affects_constraint(ua_mobilization_protector_cert, ukraine_labor_market_mobilization_drag).
narrative_ontology:affects_constraint(ua_mobilization_protector_cert, diaspora_return_disincentive).

% DUAL FORMULATION NOTE:
% The Protector system is the primary technological constraint; it links upstream to the geopolitical conflict necessitating mobilization and downstream to labor market and diaspora effects. Related constraints include the labor market drag from conscriptable population unavailability and the diaspora return disincentive created by certificate requirement for re-entry. This story focuses on the direct certificate system; downstream stories model its economic and social consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ua_mobilization_protector_cert, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
