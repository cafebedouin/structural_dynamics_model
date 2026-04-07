% ============================================================================
% CONSTRAINT STORY: data_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_asymmetry, []).

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
 *   constraint_id: data_asymmetry
 *   human_readable: Data Asymmetry: Information Advantage and Structural Extraction
 *   domain: information_economics/governance
 *
 * SUMMARY:
 *   Data asymmetry — the structural gap between those who collect, control,
 *   and analyze data and those whose data is collected — has become a
 *   foundational constraint in the digital economy. Unlike earlier
 *   information economics where asymmetry was geographically or temporally
 *   bound, digital data asymmetry is systematic, continuous, and
 *   architecturally embedded. This constraint exhibits the full range of DR
 *   classifications: data subjects trapped in systems (snare), excluded
 *   populations locked into underrepresentation (snare), regulated
 *   enterprises balancing coordination and extraction (tangled rope), data
 *   infrastructure reaping coordination benefits (rope), coalitions building
 *   technical alternatives (scaffold), consent rituals performing legal cover
 *   (piton), and naturalizing frameworks suggesting asymmetry is inherent
 *   (false mountain). The constraint's extractiveness has increased from 0.35
 *   to 0.58 over the interval, driven by deepening algorithmic
 *   decision-making in credit, employment, healthcare, and social services.
 *   Theater ratio has slightly decreased (0.55 to 0.48) as technical
 *   alternatives reduce the performative compliance burden, but extraction
 *   has simultaneously increased as data collection extends into more
 *   intimate domains. The asymmetry is not primarily about the existence of
 *   data flows — coordination genuinely benefits from data — but about the
 *   *directionality* of those flows and who controls interpretation.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary victim (powerless/trapped) — individuals whose behavioral, financial, health, and relational data is continuously collected; trapped by dependence on digital services
 *   - Excluded Populations: Primary victim (powerless/trapped) — communities underrepresented in training data; locked into algorithmic discrimination through absence
 *   - Data Collectors: Primary beneficiary (institutional/arbitrage) — platforms, ISPs, device manufacturers, data brokers; extract value through data monetization and service optimization
 *   - Algorithmic Gatekeepers: Primary beneficiary (institutional/arbitrage) — ML researchers, data scientists, algorithm designers; control interpretation and application of collected data
 *   - Regulated Enterprises: Secondary beneficiary (moderate/constrained) — companies subject to data governance regulations; benefit from data within compliance boundaries while exploiting regulatory gaps
 *   - Data Governance Coalition: Organized agent (organized/constrained) — privacy advocates, data trusts, decentralized protocol developers, cooperative platforms; building alternative architectures
 *   - Legal/Consent Infrastructure: Institutional actor (institutional/analytical) — maintains performative consent rituals; benefits from liability protection without enabling genuine choice
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architectural choices (centralization, opacity, property rights) as inherent limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_asymmetry, 0.58).
domain_priors:suppression_score(data_asymmetry, 0.62).
domain_priors:theater_ratio(data_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(data_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(data_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_asymmetry, tangled_rope).
narrative_ontology:human_readable(data_asymmetry, "Data Asymmetry: Information Advantage and Structural Extraction").
narrative_ontology:topic_domain(data_asymmetry, "information_economics/governance").

domain_priors:requires_active_enforcement(data_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_asymmetry, data_collectors).
narrative_ontology:constraint_beneficiary(data_asymmetry, algorithmic_gatekeepers).
narrative_ontology:constraint_victim(data_asymmetry, data_subjects).
narrative_ontology:constraint_victim(data_asymmetry, excluded_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Individual cannot exit digital systems without forfeiting access to essential services. Extraction through behavioral data harvesting, predictive profiling, and opaque decision-making produces high experienced chi. The subject bears costs (privacy loss, price discrimination, surveillance) while benefiting minimally from coordination mechanisms.
constraint_indexing:constraint_classification(data_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXCLUDED POPULATIONS (SNARE) — Communities with limited data representation (geographic, demographic, behavioral) are locked out of systems built on aggregate training data. The asymmetry becomes epistemic: their absence from datasets ensures algorithms perform worse for their needs, and the performance gaps then justify continued exclusion. Maximal extraction with no coordination benefit.
constraint_indexing:constraint_classification(data_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATED ENTERPRISE (TANGLED ROPE) — Organizations subject to data governance rules (GDPR, CCPA) face compliance costs but benefit from data collection within regulatory boundaries. The constraint coordinates legitimate data use while enabling selective extraction through technicalities (consent dark patterns, purpose creep, data brokers). Mixed mechanism: coordination of privacy protection + asymmetric extraction through regulatory arbitrage.
constraint_indexing:constraint_classification(data_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DATA COLLECTION INFRASTRUCTURE (ROPE) — Platforms, devices, and data brokers experience data asymmetry as a coordination mechanism. They collect data to improve services, recommend content, and optimize resource allocation. The infrastructure sees genuine coordination benefits (personalization, efficiency) alongside extraction. Net beneficiary position with low exit costs — can rotate between data markets and regulatory domains.
constraint_indexing:constraint_classification(data_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA GOVERNANCE COALITION (SCAFFOLD) — Organized actors (privacy advocates, data trusts, decentralized systems, cooperative platforms) are building alternative data architectures with sunset logic: data minimization, federated learning, differential privacy, and data cooperatives reduce asymmetric extraction over time. The coalition perceives the bottleneck as temporary — technological and regulatory changes enabling data reciprocity. Theater ratio low because technical alternatives are functional, not performative.
constraint_indexing:constraint_classification(data_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSENT RITUAL (PITON) — The legal fiction of informed consent for data use is substantially performative. Users cannot meaningfully comprehend terms-of-service documents, have no realistic exit option to withheld consent, and the consent process exists primarily to provide legal cover rather than to enable genuine choice. The ritual persists through institutional inertia — companies maintain performative consent procedures because legal precedent requires them, not because consent mechanisms actually work. Theater ratio high; extraction remains uncontrolled.
constraint_indexing:constraint_classification(data_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry is inherent to systems with differential access to knowledge. Data asymmetry appears as a natural limit of human cognition and organizational capacity: those who control data flows will always have informational advantage. However, structural data reveals this as a false summit — the asymmetry is contingent on technical architecture (centralized data collection), legal frameworks (property rights over data), and institutional choices (opacity), not physical law. The naturalization obscures that reciprocal data architectures are technically feasible.
constraint_indexing:constraint_classification(data_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_asymmetry, TR),
    TR >= 0.70.

:- end_tests(data_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Data collection generates legitimate coordination benefits — personalization, efficiency, fraud detection, resource optimization. But extraction far exceeds coordination cost: behavioral surplus is captured without meaningful return; price discrimination and exclusion extract further; algorithmic decisions are made without transparency or recourse. The 0.58 reflects that extraction is substantial but not maximal — some genuine service improvement occurs. The trajectory from 0.35 to 0.58 shows accumulation: data collection expands into more intimate domains (health, location, biometric, psychological), deepening extraction while coordination benefits plateau. Suppression (0.62): Moderate-high. Barriers to exit include technical dependence (digital services are essential infrastructure), legal barriers (terms-of-service), cognitive barriers (complexity of data flows), and power asymmetry (individuals cannot negotiate). But suppression is not total — some population segments maintain low-digital footprints; alternative platforms exist at smaller scale; technical tools (VPNs, ad blockers) provide partial relief. Theater ratio (0.48): Moderate-low. Consent and privacy disclosures are substantially performative (users cannot reasonably comprehend terms-of-service; withdrawal of consent often blocks service). But the ratio is not high because some genuine technical alternatives exist (encrypted messaging, federated systems, data minimization) that replace theater with function. The slight decline suggests data governance coalitions are building real alternatives rather than regulatory theater.
 *
 * PERSPECTIVAL GAP:
 *   Data subjects experience snare; infrastructure experiences rope; governance sees temporary problem with exit path. The gap is maximal because data asymmetry has no symmetric form — if data subjects had equivalent access to institutional data, they would use it for contestation and recourse, destabilizing the extraction mechanism. The constraint persists precisely because reciprocity is architecturally prevented, not because coordination is impossible.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (data collectors, algorithmic gatekeepers) have arbitrage exit — they can pivot between regulatory domains, business models, or markets. This low-d position produces negative chi: they experience the constraint as enabling, not extractive. Victims (data subjects, excluded populations) have trapped or identity-locked exit — they cannot realistically opt out without forfeiting essential services, and their internalized relationship to algorithmic profiles may prevent them from recognizing exit as conceptually possible. This high-d position produces high chi: they experience maximum extraction. The constraint is tangled rope (not pure snare) because coordination benefits are real — algorithmic personalization does improve user experience, and fraud detection prevents harms. But those benefits are captured asymmetrically: infrastructure reaps most surplus while subjects get the externalized costs (surveillance, discrimination, manipulation). The tangled rope classification depends on beneficiaries genuinely benefiting from coordination (which they do) while victims bear asymmetric costs (which they do). If data collection had no coordination function — if it was pure extraction with no service improvement — the constraint would be snare for all perspectives, not tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Data asymmetry is a canonical tangled rope: genuine coordination mechanism (service improvement through data) coexisting with systematic extraction (behavioral surplus capture, discrimination, opacity). The mandatrophy is resolved by decomposing the benefits and costs separately. Coordination benefits: personalization (genuine), fraud detection (genuine), resource optimization (genuine), infrastructure maintenance (genuine). Extraction mechanisms: price discrimination, behavioral manipulation, exclusion of underrepresented groups, algorithmic decisions without recourse, terms-of-service scope creep, data broker monetization. The constraint is not misclassified as pure extraction (snare) because subjects genuinely benefit from some coordination. The constraint is not misclassified as pure coordination (rope) because systematic extraction is the dominant mechanism from victims' perspective. Tangled rope captures exactly this hybrid: the existence of coordination benefit does not prevent or excuse asymmetric extraction. The governance coalition's scaffold perspective (sunset through technical alternatives) is structurally coherent — federated learning, differential privacy, and data trusts reduce asymmetry without eliminating coordination. If those alternatives scale, the constraint transitions from tangled rope toward rope as extraction declines relative to coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_fiction_boundary,
    'Is the consent ritual (terms-of-service, cookie notices) a coordination mechanism with broken feedback loops, or pure extraction theater with no coordinating function?',
    'User comprehension studies; analysis of behavioral changes post-consent requests; comparison of compliance between enforced vs optional consent mechanisms',
    'If coordination with broken feedback: tangled_rope classification holds. If pure theater: snare classification extends to regulatory enterprises; extraction increases with suppression of alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_fiction_boundary, empirical, 'Whether consent mechanisms coordinate data use or are performative cover').

omega_variable(
    algorithmic_recourse_feasibility,
    'Can data subjects realistically contest algorithmic decisions informed by asymmetric data, or do disclosure requirements remain theoretical?',
    'Case study analysis of GDPR right-to-explanation requests; measurement of algorithmic recourse success rates; time and cost burden on subjects attempting contestation',
    'If feasible: exit options upgrade from trapped to constrained; classification shifts from snare toward tangled_rope. If theoretical: suppression holds at 0.62; subjects remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_recourse_feasibility, empirical, 'Feasibility of algorithmic contestation and recourse mechanisms').

omega_variable(
    data_minimization_scalability,
    'Do federated learning, differential privacy, and data minimization techniques enable equivalent service quality and personalization with asymmetry reduction, or do they require unacceptable performance tradeoffs?',
    'Technical benchmarking of federated vs centralized systems; user satisfaction studies on personalization quality with minimized data; adoption rates of privacy-preserving architectures',
    'If scalable: scaffold sunset is real; organizational incentives shift toward data minimization over 10-20 year horizon. If not scalable: asymmetry persists because coordination genuinely requires centralized data; classification stabilizes as tangled_rope rather than declining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_minimization_scalability, empirical, 'Technical feasibility of data-minimization approaches at scale').

omega_variable(
    power_asymmetry_amplification,
    'Does data asymmetry amplify or dampen existing power differentials between institutional actors and individuals?',
    'Network analysis of data flow direction; measurement of decision velocity differentials (how fast institutions act on individuals vs vice versa); institutional accountability response times',
    'If amplification: extraction mechanism is autocatalytic — data asymmetry feeds back into power asymmetry, raising chi over time. If dampening: some corrective feedback exists; suppression may decline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(power_asymmetry_amplification, empirical, 'Whether data asymmetry amplifies or corrects power imbalances').

omega_variable(
    identity_lock_data_profiling,
    'Do data subjects internalize algorithmic profiling narratives as self-truth (identity fusion with the profile), or do they perceive profiling as external observation?',
    'Qualitative interviews on algorithmic identity; analysis of self-concept shifts post-exposure to personal data profiles; behavioral changes following algorithmic recommendation revelation',
    'If internalized: exit_options for data subjects should be reclassified from trapped to identity_locked; perspectival classification shifts from snare toward rope (perceivable as changeable). If external: trapped classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_data_profiling, empirical, 'Whether data subjects experience profiling as identity-constitutive or external').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(data_asym_tr_t0, data_asymmetry, theater_ratio, 0, 0.55).
narrative_ontology:measurement(data_asym_tr_t5, data_asymmetry, theater_ratio, 5, 0.52).
narrative_ontology:measurement(data_asym_tr_t10, data_asymmetry, theater_ratio, 10, 0.48).
narrative_ontology:measurement(data_asym_tr_t15, data_asymmetry, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(data_asym_be_t0, data_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(data_asym_be_t5, data_asymmetry, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(data_asym_be_t10, data_asymmetry, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(data_asym_be_t15, data_asymmetry, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(data_asymmetry, 0.18).
narrative_ontology:affects_constraint(data_asymmetry, algorithmic_opacity).
narrative_ontology:affects_constraint(data_asymmetry, behavioral_manipulation).
narrative_ontology:affects_constraint(data_asymmetry, regulatory_arbitrage_digital).

% DUAL FORMULATION NOTE:
% Data asymmetry is upstream of specific extractive mechanisms: algorithmic opacity (how decisions are made), behavioral manipulation (how asymmetry is leveraged), and regulatory arbitrage (how firms navigate governance). The base constraint has ε=0.58 reflecting genuine mixed mechanism; downstream constraints show how asymmetry enables specific extractions. All three are linked: removing data asymmetry would degrade the other constraints' extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
