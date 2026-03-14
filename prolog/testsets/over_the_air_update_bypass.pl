% ============================================================================
% CONSTRAINT STORY: over_the_air_update_bypass
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_over_the_air_update_bypass, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: over_the_air_update_bypass
 *   human_readable: Over-The-Air Update Bypass in Connected Devices
 *   domain: technology/security/device_control
 *
 * SUMMARY:
 *   The over-the-air update bypass constraint governs the relationship
 *   between device owners and manufacturers over firmware control. Connected
 *   devices (smartphones, IoT, automotive, medical implants) have shifted
 *   from user-controlled software stacks to manufacturer-controlled,
 *   remotely-pushable firmware updates with no user abort, delay, or
 *   inspection mechanism. This constraint demonstrates classic Snare
 *   structure: device owners cannot exit without losing service, cannot
 *   inspect what is being pushed, face suppression (remote lockdown, warranty
 *   revocation, service suspension) if they attempt to regain control, and
 *   bear asymmetric costs (security vulnerabilities, forced obsolescence,
 *   loss of privacy). The constraint simultaneously shows Tangled Rope from
 *   manufacturer perspectives — genuine coordination function exists
 *   (security patching prevents botnet recruitment and liability cascades)
 *   layered onto extraction (vendor lock-in, suppression of repair,
 *   obsolescence acceleration). The extractiveness has increased over 15
 *   years (2010-2025) as device complexity has increased and manufacturers
 *   have tightened firmware control, closing escape hatches like bootloader
 *   unlock that existed in earlier device generations. Theater ratio reflects
 *   increasing performative content: regulatory frameworks (FCC, EU) create
 *   transparency requirements, but enforcement is weak and manufacturers
 *   routinely hide update content behind vague descriptions.
 *
 * KEY AGENTS:
 *   - Device Owner: Primary victim (powerless/trapped) — structurally unable to refuse, inspect, or delay updates; bears full cost of forced firmware changes
 *   - Security Researcher: Secondary victim (moderate/constrained) — blocked by legal suppression (DMCA), technical opacity, and career risk from losing platform access
 *   - Device Manufacturer: Primary beneficiary (institutional/arbitrage) — uses OTA authority for control and extraction while claiming security necessity
 *   - Network Service Provider: Secondary beneficiary (powerful/mobile) — uses OTA mechanism to enforce service bundling and monitor user behavior
 *   - Open-Source Device Coalition: Organized alternative (organized/mobile) — experiences the constraint as Rope (coordination without extraction); niche market position
 *   - Regulatory Framework: Performative gate (institutional/arbitrage) — creates theater of oversight without functional verification capacity
 *   - Analytical Observer: Systemic view (analytical/analytical) — identifies constraint as pure extraction mechanism sustained by network effects and regulatory capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(over_the_air_update_bypass, 0.68).
domain_priors:suppression_score(over_the_air_update_bypass, 0.75).
domain_priors:theater_ratio(over_the_air_update_bypass, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(over_the_air_update_bypass, extractiveness, 0.68).
narrative_ontology:constraint_metric(over_the_air_update_bypass, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(over_the_air_update_bypass, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(over_the_air_update_bypass, snare).
narrative_ontology:human_readable(over_the_air_update_bypass, "Over-The-Air Update Bypass in Connected Devices").
narrative_ontology:topic_domain(over_the_air_update_bypass, "technology/security/device_control").

domain_priors:requires_active_enforcement(over_the_air_update_bypass).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(over_the_air_update_bypass, device_manufacturers).
narrative_ontology:constraint_beneficiary(over_the_air_update_bypass, network_service_providers).
narrative_ontology:constraint_victim(over_the_air_update_bypass, device_users).
narrative_ontology:constraint_victim(over_the_air_update_bypass, security_researchers).
narrative_ontology:constraint_victim(over_the_air_update_bypass, software_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVICE OWNER (SNARE) — User owns the hardware but cannot refuse updates, delay them, or opt out without losing device functionality. Suppression is structural: attempting to block updates triggers remote lockdown, service suspension, or warranty revocation. No exit option exists within the device ecosystem. Maximum experienced extraction — user bears security risk of forced updates while manufacturer captures control over firmware stack.
constraint_indexing:constraint_classification(over_the_air_update_bypass, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECURITY RESEARCHER (SNARE) — Faces high barriers to independent analysis: encrypted firmware, proprietary chipsets, legal threats under DMCA Section 1201 if reverse-engineering attempted, and manufacturer refusal to publish update change logs. Constrained exit — can exit by abandoning the device, but at career cost (loses access to dominant platform for analysis and publication). Asymmetric extraction: security vulnerabilities discoverable through update bypass are captured by manufacturers and suppressed from public disclosure.
constraint_indexing:constraint_classification(over_the_air_update_bypass, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVICE MANUFACTURER (TANGLED ROPE) — Genuine coordination function: mandatory updates do solve the shared problem of device security and prevent massive liability cascades from unpatched vulnerabilities. BUT the update mechanism is also a primary extraction channel: manufacturers use update authority to enforce software licensing, prevent repair, lock users into ecosystems, and suppress competitive analysis. Extraction and coordination coexist. Manufacturer can arbitrage out (exit to different device architecture) but chooses not to, because extraction is the point.
constraint_indexing:constraint_classification(over_the_air_update_bypass, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE DEVICE COALITION (ROPE) — Organizations building Linux-based and open-firmware alternatives (Lineage OS, Coreboot, open embedded systems) experience the OTA bypass constraint as a pure coordination problem to solve, not as extraction. These agents can exit the proprietary ecosystem (they do); they benefit from transparent update mechanisms; they face minimal suppression within their own coalition. The constraint exists in their world as a Rope — coordination of distributed firmware updates without extraction. However, they occupy regional/niche scope relative to mainstream devices.
constraint_indexing:constraint_classification(over_the_air_update_bypass, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NETWORK SERVICE PROVIDER (TANGLED ROPE) — Genuine coordination: pushing security updates protects network infrastructure from botnet recruitment and reduces provider liability for user device compromise. But NSPs also use OTA authority to enforce data plans, block competing services, and monitor user behavior via update mechanisms. Powerful agents with mobile exit options (can migrate customers to different technologies) but choose not to because extraction is profitable. Mixed coordination and extraction with agency — classified as tangled rope, not snare.
constraint_indexing:constraint_classification(over_the_air_update_bypass, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY OVERSIGHT FRAMEWORK (PITON) — FCC, EU regulatory bodies, and right-to-repair legislation create a performative framework around update transparency. Regulations technically require disclosure of what is being updated and why, but enforcement is weak, definitions are vague, and manufacturers routinely comply with the letter while violating the spirit. The regulatory theater persists (compliance filing, transparency reports) but its functional verification capacity has atrophied — inspectors lack technical depth to audit update mechanisms. Theater ratio (0.55) reflects this partial degradation of oversight function.
constraint_indexing:constraint_classification(over_the_air_update_bypass, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a systemic perspective, the OTA update bypass constraint extracts value from device owners while suppressing their ability to understand or modify their own hardware. The constraint persists because it benefits device manufacturers and service providers while costs are diffuse (security fragmentation, loss of user agency, elimination of right-to-repair, accelerated device obsolescence). No natural law justifies mandatory OTA authority — it is a pure institutional extraction mechanism sustained by network effects and regulatory capture.
constraint_indexing:constraint_classification(over_the_air_update_bypass, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(over_the_air_update_bypass_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(over_the_air_update_bypass, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(over_the_air_update_bypass, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(over_the_air_update_bypass, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(over_the_air_update_bypass, TR),
    TR >= 0.70.

:- end_tests(over_the_air_update_bypass_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. Manufacturers extract control, lock-in, forced upgrade costs, and suppression of independent analysis. The extraction is not total (some coordination function genuinely exists) but substantial and asymmetric — user cannot negotiate or opt out. The measurement trajectory shows extractiveness rising from 0.42 to 0.72 over 15 years, driven by architectural decisions (closed bootloaders, encrypted firmware, DRM enforcement) that progressively eliminated user escape hatches. Suppression (0.75): High. Structural barriers (device architecture) + legal barriers (DMCA, TOS terms) + economic barriers (loss of warranty, service suspension, forced replacement). Users cannot easily inspect, understand, or modify OTA content. Attempted bypass triggers remote lockdown. Theater ratio (0.55): Moderate-high and increasing. Regulatory frameworks (FCC device security rules, EU transparency requirements, right-to-repair legislation) create performative compliance (manufacturers file transparency reports, publish vague security advisories). But functional verification capacity has eroded — regulators lack technical depth to audit firmware changes, inspect update mechanisms, or validate claimed security benefits. The theater ratio increase from 0.35 to 0.62 reflects manufacturers investing more in compliance theater while suppression mechanisms have simultaneously strengthened.
 *
 * PERSPECTIVAL GAP:
 *   Device owners see pure extraction (Snare) — they cannot understand what is being installed, cannot refuse it, and face maximum suppression. Manufacturers see mixed coordination and extraction (Tangled Rope) — they frame OTA as security necessity while deploying it for control and lock-in. Security researchers see extraction with legal suppression (Snare) — they want to audit what manufacturers are pushing, but legal frameworks (DMCA) and technical barriers (encrypted firmware, proprietary chipsets) prevent independent analysis. Open-source coalitions see pure coordination (Rope) — they solve the OTA problem (distributing updates across heterogeneous devices) without extraction. Regulatory frameworks see a problem they can manage (Piton theater) — they create transparency rules, but enforcement mechanisms are weak and manufacturers have captured the standards bodies. Analytically, the constraint is a Snare: it extracts value and agency from powerless agents while suppressing transparency and alternatives. The perspectival gap reveals that manufacturers have successfully reframed extraction (control, lock-in) as coordination (security) — the Tangled Rope classification from their perspective is parasitic on this reframing. Were the reframing stripped away, the constraint would appear as pure Snare from all perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Device owners experience d ≈ 0.95 (full victim): trapped exit options, no structural benefits from OTA authority, maximum suppression. Manufacturers experience d ≈ 0.05 (full beneficiary): arbitrage exit options, direct benefits from OTA control, capability to suppress alternatives. Security researchers experience d ≈ 0.85: constrained exit (career risk of losing platform access), victim status (blocked from analysis), but some mobility (can study open devices). Open-source coalition experiences d ≈ 0.30: mobile exit options, some benefits (transparent updates), reduced suppression within their subset. Each agent's χ (effective extraction) is computed from ε × f(d) × σ(S), where S = global scope (σ=1.2). Device owners: χ ≈ 0.68 × 1.42 × 1.2 ≈ 1.16 (capped at 1.0). Manufacturers: χ ≈ 0.68 × (-0.12) × 1.2 ≈ -0.10 (negative extraction: they are net extractors, not targets). The directionality gap is maximum — device owners and manufacturers occupy opposite poles of the d spectrum relative to this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint exhibits genuine mixed coordination and extraction (Tangled Rope from manufacturer perspective) but the coordination function is NOT the driver of the constraint's existence or design. Manufacturers could solve the security coordination problem (distributing patches, maintaining baseline protection) through transparent update mechanisms with user oversight, rollback capacity, and audit capability. That they choose not to — closing bootloaders, encrypting firmware, suppressing third-party analysis — reveals that extraction (control, lock-in, obsolescence acceleration) is the primary motivation. The Tangled Rope is real but not explanatory. The Snare classification from powerless agent perspective is more structurally accurate: the constraint persists because it extracts from those who cannot exit. Manufacturers would abandon OTA bypass immediately if they faced credible competition on update transparency. The extractiveness level (0.68) is justified: not pure extraction (some coordination exists), but substantial asymmetric extraction (user bears all costs, manufacturers capture all benefits).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_necessity_vs_control,
    'What portion of mandatory OTA authority is genuinely necessary for security (addressing zero-days, maintaining baseline protection) versus what is control mechanism (enforcing vendor lock-in, preventing repair, suppressing analysis)?',
    'Historical security impact analysis: measurement of (critical zero-days addressed via emergency OTA) vs (non-security firmware changes: licensing updates, feature restrictions, obsolescence signals); comparison with security outcomes on open-source devices with user-controlled updates',
    'If security necessity dominates: constraint shifts toward Tangled Rope across all perspectives. If control mechanism dominates: classification remains Snare. If roughly equal: Tangled Rope is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_vs_control, empirical, 'Security necessity vs control mechanism in mandatory OTA').

omega_variable(
    user_agent_recovery_path,
    'Is there a feasible technical pathway for device owners to regain agency over firmware (bootloader unlock, OEM key revocation, signed custom ROM support, slow-roll opt-out) without total device replacement?',
    'Technical audit of device architecture; analysis of which device classes support user firmware modification; measurement of percentage of global device installed base that permits OTA bypass. Comparison with historical device controls (2010-2015) when bootloader unlock was common.',
    'If pathway exists for >50% of devices: exit_options could shift from ''trapped'' to ''constrained'' for user perspective, lowering d and chi. If pathway exists but legally blocked (DMCA): exit remains trapped (legal suppression ≥ structural). If pathway was eliminated by architecture choice: trapped status confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_agent_recovery_path, empirical, 'Feasibility of user firmware agency recovery').

omega_variable(
    open_firmware_competitive_viability,
    'Can open-firmware devices (custom ROM, open bootloader, transparent update mechanisms) achieve sufficient market penetration and user experience parity to reduce network effects binding powerless agents to proprietary OTA constraints?',
    'Market share tracking for Lineage OS, Ubuntu Touch, PostmarketOS, Coreboot-enabled systems; user satisfaction and security update responsiveness comparison; identification of which device classes support realistic open-firmware transitions',
    'If yes: Rope classification for mainstream (not niche) becomes possible — escape from extraction becomes structurally real, not just theoretically possible. If no: Snare classification is permanent for trapped users.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_firmware_competitive_viability, empirical, 'Open-firmware device market viability as exit mechanism').

omega_variable(
    manufacturer_collective_action,
    'Do manufacturers maintain OTA bypass suppression through genuine collective action agreement, or through unilateral architectural lock-in decisions that happen to align?',
    'Antitrust discovery analysis; identification of communications, standards bodies (3GPP, OMA), or industry associations that coordinate OTA policy; measurement of whether any manufacturer has ever publicly committed to user-controlled update rollback and been forced to abandon it',
    'If collective action: Snare could be reshaped via antitrust intervention (trust-busting architecture choice). If unilateral alignment: Snare is more durable (no conspiracy to prosecute), but easier to break with individual defector (one manufacturer supporting bootloader unlock changes incentive landscape).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturer_collective_action, empirical, 'Collective action vs unilateral alignment in OTA suppression').

omega_variable(
    regulatory_enforcement_capacity,
    'Do regulatory bodies (FCC, EU DMA, right-to-repair coalitions) have sufficient technical depth and enforcement authority to mandate OTA transparency, audit mechanisms, or user control without being captured by manufacturer expertise asymmetry?',
    'Analysis of EU Digital Markets Act enforcement against Apple (right-to-repair); FCC device security rules and enforcement outcomes; measurement of regulatory audit frequency and technical depth; identification of regulatory turnover and capture vulnerabilities',
    'If enforcement capacity strengthens: Piton could shift toward Scaffold (temporary regulatory control with sunset as oversight capacity matures) or even Tangled Rope (coordination with extraction). If captured: Piton theater persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_capacity, empirical, 'Regulatory enforcement capacity for OTA transparency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(over_the_air_update_bypass, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(otabypass_tr_t0, over_the_air_update_bypass, theater_ratio, 0, 0.35).
narrative_ontology:measurement(otabypass_tr_t5, over_the_air_update_bypass, theater_ratio, 5, 0.45).
narrative_ontology:measurement(otabypass_tr_t10, over_the_air_update_bypass, theater_ratio, 10, 0.55).
narrative_ontology:measurement(otabypass_tr_t15, over_the_air_update_bypass, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(otabypass_be_t0, over_the_air_update_bypass, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(otabypass_be_t5, over_the_air_update_bypass, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(otabypass_be_t10, over_the_air_update_bypass, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(otabypass_be_t15, over_the_air_update_bypass, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(over_the_air_update_bypass, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(over_the_air_update_bypass, 0.12).
narrative_ontology:affects_constraint(over_the_air_update_bypass, device_right_to_repair).
narrative_ontology:affects_constraint(over_the_air_update_bypass, firmware_transparency_regulation).
narrative_ontology:affects_constraint(over_the_air_update_bypass, vendor_lock_in_smartphone_markets).

% DUAL FORMULATION NOTE:
% The OTA bypass constraint is downstream of device architecture design (closed bootloaders, encrypted firmware) but represents a distinct structural constraint. The upstream architecture choices are system design decisions; the OTA bypass is the enforcement mechanism that makes those choices stick. Regulatory attempts to mandate transparency operate on the OTA constraint (requiring disclosure, audit trails) rather than on the architecture (bootloader unlock, custom ROM support). Decomposition: device_architecture_lock_in (ε ≈ 0.45) feeds into over_the_air_update_bypass (ε ≈ 0.68). Device owners trapped by architecture choice; manufacturers enforce trap via OTA mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(over_the_air_update_bypass, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
