% ============================================================================
% CONSTRAINT STORY: sensory_substitution_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sensory_substitution_dependency, []).

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
 *   constraint_id: sensory_substitution_dependency
 *   human_readable: Sensory Substitution Technology Dependency
 *   domain: assistive_technology/disability/accessibility
 *
 * SUMMARY:
 *   Sensory substitution technology — cochlear implants, retinal prostheses,
 *   haptic-feedback systems, brain-computer interfaces for sensory
 *   restoration — creates a structural trap for users. Once a person adapts
 *   to a device that restores access to a lost sense, they cannot
 *   meaningfully exit the relationship without abandoning the restored
 *   capability. This creates a binding that differs from other technology
 *   dependencies because the alternative (sensory impairment) is not
 *   equivalent to the technology-dependent state. The constraint exhibits
 *   high suppression (0.67) because users lack material alternatives:
 *   proprietary devices dominate the market, regulations restrict open-source
 *   alternatives, repair is controlled by manufacturers, firmware updates are
 *   mandatory, consumables are vendor-specific, and switching between
 *   incompatible ecosystems is prohibitively costly. The extractiveness has
 *   increased over the measurement interval (0.35 → 0.58) as manufacturers
 *   have layered lock-in mechanisms (cloud connectivity requirements,
 *   AI-driven optimization locked to proprietary neural networks, data
 *   harvesting from implant usage) onto the underlying sensory restoration
 *   function. The constraint is not a mountain (immutable law of biology) but
 *   a snare (contingent institutional arrangement disguised as natural
 *   necessity).
 *
 * KEY AGENTS:
 *   - Sensory-Impaired Users: Primary victims (powerless/trapped) — cannot exit without losing restored sensory access; bear full extraction through proprietary consumables, mandatory updates, incompatibility, and vendor control
 *   - Technology Manufacturers (cochlear implant companies, retinal prosthesis makers, haptic interface producers): Primary beneficiaries (institutional/arbitrage) — capture value through device sales, proprietary consumables, ecosystem lock-in, and data harvesting; legitimate R&D cost recovery but extractive mechanisms exceed justifiable returns
 *   - Open-Source Assistive Technology Community: Secondary actors (moderate/constrained) — develop interoperable alternatives but face patent barriers, regulatory exclusion, and reimbursement discrimination
 *   - Right-to-Repair Advocacy: Organized agents (organized/constrained) — push for standardization, interoperability, and user control; see the constraint as a temporary institutional failure with a sunset trajectory
 *   - Medical Device Regulators: Institutional actors (institutional/arbitrage) — maintain certification theater that appears to protect safety but does not prevent lock-in or enforces interoperability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent lock-in mechanisms as immutable biological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sensory_substitution_dependency, 0.58).
domain_priors:suppression_score(sensory_substitution_dependency, 0.67).
domain_priors:theater_ratio(sensory_substitution_dependency, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sensory_substitution_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(sensory_substitution_dependency, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(sensory_substitution_dependency, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sensory_substitution_dependency, snare).
narrative_ontology:human_readable(sensory_substitution_dependency, "Sensory Substitution Technology Dependency").
narrative_ontology:topic_domain(sensory_substitution_dependency, "assistive_technology/disability/accessibility").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sensory_substitution_dependency, technology_manufacturers).
narrative_ontology:constraint_beneficiary(sensory_substitution_dependency, research_institutions).
narrative_ontology:constraint_victim(sensory_substitution_dependency, sensory_impaired_users).
narrative_ontology:constraint_victim(sensory_substitution_dependency, accessibility_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SENSORY-IMPAIRED USER (SNARE) — Trapped in technological dependency. Once adapted to a sensory substitution device (cochlear implant, retinal prosthesis, haptic interface), users cannot exit without abandoning access to their environment. The technology becomes constitutive of their adapted perceptual capability. Device manufacturer controls maintenance, software, upgrades, and compatibility. User bears full extraction: expensive proprietary consumables, mandatory firmware updates, vendor lock-in, incompatibility with competing systems. No meaningful alternatives once integrated into daily life.
constraint_indexing:constraint_classification(sensory_substitution_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNOLOGY MANUFACTURER (ROPE) — Experiences the constraint as pure coordination: providing life-altering sensory access solves a genuine collective action problem. Users cannot coordinate alternative access without the device; the manufacturer bridges that gap. Captures value through licensing, proprietary consumables, and ecosystem lock-in, but these are legitimately justified by R&D cost and ongoing support obligations. Beneficiary position with arbitrage exit — can shift to different markets, licensing models, or technologies if returns decline.
constraint_indexing:constraint_classification(sensory_substitution_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: OPEN-SOURCE ASSISTIVE TECH COMMUNITY (TANGLED ROPE) — Develops non-proprietary alternatives (open-hardware cochlear implants, open-source signal processing, DIY haptic interfaces) and enjoys genuine coordination benefits: shared protocols enable interoperability, distributed development reduces cost. But faces extractive barriers: proprietary manufacturers issue patent challenges, medical device regulations lock out alternatives, reimbursement systems only cover approved proprietary devices. Mixed coordination and extraction — genuine benefit function blocked by asymmetric enforcement.
constraint_indexing:constraint_classification(sensory_substitution_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHT-TO-REPAIR MOVEMENT (SCAFFOLD) — Organized advocacy for device repairability, software ownership, and interoperability standards (USB-C charging, open firmware). Sees the constraint as a temporary institutional failure with a sunset: legal mandates (EU Right to Repair, Digital Markets Act) are building pathways to user agency. Theater ratio is moderate — some genuine progress on standardization, but much advocacy effort remains performative. Sunset clause: 10-15 years for interoperability standards to mature and lock-in mechanisms to weaken.
constraint_indexing:constraint_classification(sensory_substitution_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MEDICAL DEVICE REGULATORY FRAMEWORK (PITON) — FDA approval, CE marking, and similar regulatory gates were designed to ensure safety and efficacy. But the framework has become largely performative: manufacturers navigate certifications routinely; regulations do not prevent extractive lock-in mechanisms; regulatory bodies lack resources to enforce interoperability or repairability. The theatrical certification process persists because alternatives haven't fully replaced it, not because it prevents harm. Theater ratio high — visible regulatory ritual obscures the functional vacuum where user protections should be.
constraint_indexing:constraint_classification(sensory_substitution_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some technological dependency is inherent: biological sensory loss cannot be replaced without external technology. Sensory substitution requires a device; the device requires maintenance and updates. This perspective sees the lock-in as immutable law of nature — you cannot have sensory restoration without accepting technological dependence. However, this naturalizes what is contingent: the lock-in mechanisms (proprietary consumables, incompatible protocols, repair restrictions) are not inherent to sensory substitution technology; they are institutional choices. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(sensory_substitution_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sensory_substitution_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sensory_substitution_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sensory_substitution_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sensory_substitution_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sensory_substitution_dependency, TR),
    TR >= 0.70.

:- end_tests(sensory_substitution_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and increasing. Manufacturers legitimately recover R&D costs through device sales and ongoing support. But extractiveness has grown beyond justifiable return because manufacturers have added lock-in mechanisms: proprietary signal processing, cloud-connected optimization, data harvesting from implant usage, incompatible consumables, repair restrictions, and mandatory firmware updates. The measurement trajectory (0.35 → 0.58) reflects this layering — early devices (0.35) focused on sensory restoration with moderate vendor control; modern devices (0.58) bundle restoration with aggressive lock-in. Suppression (0.67): High. Users cannot meaningfully exit because the alternative (sensory impairment) is not equivalent to device dependence. Material barriers include: monopolistic device markets, regulatory gates that exclude open-source alternatives, reimbursement systems locked to proprietary devices, repair controls, and switching costs between incompatible ecosystems. Theater ratio (0.52): Moderate. Medical device certification processes (FDA, CE mark) appear to protect safety but do not prevent lock-in or enforce interoperability. Regulatory theater obscures the functional vacuum where user protections should exist. As manufacturers sophisticate lock-in mechanisms, the proportion of genuine coordination (sensory restoration) versus performative ritual (regulatory approval) is declining.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why indexical classification is essential. The manufacturer sees Rope — they are solving the genuine coordination problem of restoring lost sensory access. Users see Snare — they are locked into extraction with no exit. The open-source community sees Tangled Rope — real coordination benefits (interoperability) blocked by asymmetric enforcement. The right-to-repair movement sees Scaffold — a temporary institutional failure being resolved by legal mandates and standardization efforts. Medical regulators see their own work as essential (implicit Rope) but the system appears as Piton — performative certification with degraded functional content. The civilizational analyst risks seeing Mountain (inherent biological necessity) but the structural data reveals this as naturalization of contingent institutional choices. The perspectival gap is wide because the manufacturer's legitimate function (sensory restoration) is genuine, but the extraction mechanisms (lock-in, proprietary consumables, incompatibility) are contingent policy choices, not inherent technical necessities.
 *
 * DIRECTIONALITY LOGIC:
 *   The manufacturer as institutional beneficiary with arbitrage exit derives low d (d ≈ 0.15-0.25) — they capture extraction but can exit by shifting to different markets if returns decline. Their experienced extractiveness is moderate (negative chi toward them, meaning they extract net value). Users as powerless victims with trapped exit derive high d (d ≈ 0.90-0.95) — they cannot exit and bear maximum extraction. The open-source community as moderate actors with constrained exit derives moderate-high d (d ≈ 0.65-0.75) — they can develop alternatives but face structural barriers (patents, regulations, reimbursement discrimination). The right-to-repair advocates as organized actors with constrained exit derive moderate d (d ≈ 0.50-0.60) — they have agency and organizing capacity to change the rules (and are succeeding: EU Right to Repair directives, Digital Markets Act). These derivations explain why different agents classify the same constraint differently: directionality determines experienced extractiveness, and experienced extractiveness determines classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing legitimate coordination (sensory restoration) from extractive mechanisms (lock-in). The manufacturer's Rope classification is justified by the genuine coordination function — users need the device and manufacturers provide it. The user's Snare classification is justified by the extractive mechanisms — mandatory updates, proprietary consumables, incompatible ecosystems, repair restrictions. The tangled_rope classification (open-source community perspective) correctly identifies the hybrid: genuine coordination function (open-source sensory restoration) plus asymmetric extraction (patent barriers, regulatory exclusion). The scaffold classification (right-to-repair) is justified by the sunset clause: legal mandates and standardization efforts are actively weakening lock-in mechanisms. The piton classification (regulators) correctly identifies the performative nature of certification — the ritual appears to protect users but functionally does not. The false summit (natural law view) is correctly identified as naturalization: sensory restoration does NOT require manufacturer lock-in; open-source projects have demonstrated equivalent functionality with lower extraction. The mandate is NOT that sensory impairment is inherent or that technological dependence is inherent — it is that LOCK-IN MECHANISMS are contingent and extractive, not inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_dependency_vs_lock_in,
    'How much of the measured suppression is inherent to sensory substitution technology, and how much is contingent lock-in mechanism?',
    'Comparative analysis of open-source vs proprietary devices; measurement of switching costs with and without vendor-imposed barriers; historical cases where devices transitioned from proprietary to interoperable standards',
    'If high proportion is inherent: constraint approaches mountain (immutable dependency). If high proportion is lock-in: constraint is snare (contingent extraction). This determines whether manufacturer power is legitimate (Rope) or extractive (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_dependency_vs_lock_in, empirical, 'Proportion of suppression from inherent technology vs contingent lock-in').

omega_variable(
    reimbursement_capture_mechanism,
    'Are insurance/Medicare reimbursement systems locked into proprietary devices due to genuine medical evidence, or due to lobbying and established relationships?',
    'Analysis of reimbursement coverage decisions; comparison of coverage for equivalent open-source vs proprietary devices; investigation of manufacturer influence on reimbursement policy bodies',
    'If evidence-based: reimbursement barriers are legitimate (Rope coordination of safety assurance). If capture-based: barriers are extractive (Snare enforcement). This affects whether the constraint appears as coordination (Rope) or pure extraction (Snare) from the user perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reimbursement_capture_mechanism, empirical, 'Whether reimbursement capture is evidence-based or lobbying-driven').

omega_variable(
    identity_fusion_in_users,
    'To what extent do users experience their sensory substitution device as identity-constitutive (identity_locked) versus structurally dependent but identity-separate (trapped)?',
    'Qualitative study of user self-concept; measurement of perceived agency in device choices; longitudinal tracking of identity statements before/after device integration',
    'If high identity fusion: users are identity_locked; exit would require identity transformation, not just device replacement. If structural dependency only: users are trapped; exit is blocked by material barriers. Classification shifts from Snare (trapped) to Snare-with-identity-lock (trapped + identity_locked exit options in same perspective, which is impossible — must decompose into separate stories of hardware dependency vs identity integration).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_users, empirical, 'Degree of identity fusion vs structural dependency in user experience').

omega_variable(
    open_source_viability_threshold,
    'What level of adoption would make open-source sensory substitution devices viable alternatives to proprietary systems?',
    'Network effects analysis; measurement of quality-cost tradeoffs at different adoption levels; identification of critical mass for supply chain viability (components, repair technicians, software developers)',
    'If threshold is reachable (< 15% market share): Scaffold sunset is realistic, and the constraint may degrade to Rope over time. If threshold is unreachable (> 40% market share): open-source remains niche, and Snare classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_viability_threshold, empirical, 'Market adoption threshold for open-source viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sensory_substitution_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sensub_tr_t0, sensory_substitution_dependency, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sensub_tr_t5, sensory_substitution_dependency, theater_ratio, 5, 0.45).
narrative_ontology:measurement(sensub_tr_t10, sensory_substitution_dependency, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(sensub_be_t0, sensory_substitution_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sensub_be_t5, sensory_substitution_dependency, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sensub_be_t10, sensory_substitution_dependency, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sensory_substitution_dependency, resource_allocation).
narrative_ontology:affects_constraint(sensory_substitution_dependency, medical_device_data_harvesting).
narrative_ontology:affects_constraint(sensory_substitution_dependency, accessibility_patent_encumbrance).
narrative_ontology:affects_constraint(sensory_substitution_dependency, right_to_repair_regulation).

% DUAL FORMULATION NOTE:
% Sensory substitution dependency decomposes into three structurally distinct constraints: (1) sensory_restoration_coordination (ε≈0.15, Rope) — genuine coordination of technology provision; (2) manufacturer_lock_in_extraction (ε≈0.65, Snare) — proprietary consumables, incompatibility, repair restrictions; (3) regulatory_theater_certification (ε≈0.35, Piton) — medical device approval as performative ritual. This story integrates all three as a single constraint family, but they have distinct ε values and mechanisms. The story's ε=0.58 reflects the net effect of restoration + lock-in + theater across the user experience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sensory_substitution_dependency, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
