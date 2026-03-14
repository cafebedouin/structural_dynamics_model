% ============================================================================
% CONSTRAINT STORY: synthetic_text_detection_arms_race
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_synthetic_text_detection_arms_race, []).

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
 *   constraint_id: synthetic_text_detection_arms_race
 *   human_readable: Synthetic Text Detection Arms Race
 *   domain: information/technology/epistemic
 *
 * SUMMARY:
 *   The synthetic text detection arms race creates a structural tension
 *   between the capability of generative language models and the
 *   institutional demand for credentialing systems that distinguish
 *   human-authored work from synthetic work. This constraint exhibits
 *   classical tangled_rope dynamics: it genuinely coordinates academic
 *   integrity verification (beneficiaries like educators and institutions
 *   solve a real problem: maintaining assessment validity), while
 *   simultaneously extracting from access-constrained populations who cannot
 *   afford proprietary detection tools and face compounding credential
 *   barriers. The constraint's extractiveness (0.58) reflects that the
 *   primary benefit flow runs toward detection service vendors and
 *   institutional gatekeepers, while the costs (surveillance, privacy
 *   erosion, access barriers, credential inflation) fall on students and
 *   researchers without institutional affiliation or resources. The
 *   theater_ratio (0.65) indicates that a significant portion of detection
 *   adoption is performative — institutions adopt tools to demonstrate
 *   compliance with emerging integrity norms and accreditation expectations,
 *   rather than because detection accuracy solves their core assessment
 *   problems. The arms race cycle shows accelerating extractiveness: as
 *   generative models improve, detection demands increase, vendor lock-in
 *   deepens, and alternative credentialing pathways that could provide exits
 *   become less attractive to mainstream institutions.
 *
 * KEY AGENTS:
 *   - Detection Service Providers: Primary beneficiary (institutional/arbitrage) — capture vendor premium through proprietary detection APIs and institutional subscriptions; experience constraint as pure coordination mechanism for academic integrity
 *   - Educators and Institutional Gatekeepers: Secondary beneficiary (moderate/constrained) — gain enforcement mechanism for academic integrity but bear costs of false positives, tool obsolescence, and vendor lock-in; benefit from coordination function outweighs extraction costs
 *   - Students in Access-Constrained Regions: Primary victim (powerless/trapped) — cannot afford detection tools; face compounding credential barriers as institutions adopt detection as gating mechanism; no exit options
 *   - Low-Resource Researchers: Primary victim (powerless/trapped) — cannot afford detection tools; institutional repositories increasingly require detection certification; career advancement pathways blocked
 *   - Open Knowledge Commons: Victim (powerless/trapped) — open-access publishing pressured to adopt detection; knowledge circulation barriers increase; abstract collective good bears cost of vendor lock-in
 *   - Open-Source Detection Coalition: Organized actor (organized/constrained) — building alternative detection tools; perceive sunset mechanism through commodification; constrained by funding and institutional adoption resistance
 *   - Traditional Academic Credentialing Systems: Institutional actor (institutional/arbitrage) — maintain own structure through performative adoption of detection; actually weakened by vendor dependence despite appearing to strengthen integrity verification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional design choices as immutable features of text verification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(synthetic_text_detection_arms_race, 0.58).
domain_priors:suppression_score(synthetic_text_detection_arms_race, 0.48).
domain_priors:theater_ratio(synthetic_text_detection_arms_race, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(synthetic_text_detection_arms_race, extractiveness, 0.58).
narrative_ontology:constraint_metric(synthetic_text_detection_arms_race, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(synthetic_text_detection_arms_race, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(synthetic_text_detection_arms_race, tangled_rope).
narrative_ontology:human_readable(synthetic_text_detection_arms_race, "Synthetic Text Detection Arms Race").
narrative_ontology:topic_domain(synthetic_text_detection_arms_race, "information/technology/epistemic").

domain_priors:requires_active_enforcement(synthetic_text_detection_arms_race).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(synthetic_text_detection_arms_race, detection_service_providers).
narrative_ontology:constraint_beneficiary(synthetic_text_detection_arms_race, content_platforms_with_gating).
narrative_ontology:constraint_beneficiary(synthetic_text_detection_arms_race, institutional_credentialing_systems).
narrative_ontology:constraint_victim(synthetic_text_detection_arms_race, open_knowledge_commons).
narrative_ontology:constraint_victim(synthetic_text_detection_arms_race, low_resource_researchers).
narrative_ontology:constraint_victim(synthetic_text_detection_arms_race, students_in_access_constrained_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The open knowledge ecosystem cannot exit the detection arms race and bears full cost of escalating gatekeeping. Text classification tools proliferate; detection paywalls multiply; researchers in low-resource contexts face compounding barriers. No organizing principle, no exit option. Experiences maximum extraction.
constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STUDENTS IN ACCESS-CONSTRAINED REGIONS (SNARE) — Cannot afford premium detection tools or attend institutions with site licenses. Trapped between generative tools (free, powerful) and detection tools (paywalled, proprietary). Original academic pathways increasingly unavailable. No alternatives for credential acquisition.
constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: EDUCATORS AND INSTITUTIONAL GATEKEEPERS (TANGLED ROPE) — Benefit from detection tools as enforcement mechanism for academic integrity norms (genuine coordination function: maintaining assessment validity). But also bear costs: false positives damage student relationships, detection tools require budget allocation, rapid tool obsolescence forces continuous re-adoption. Constrained by institutional need to demonstrate integrity verification.
constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DETECTION SERVICE PROVIDERS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: they provide signal for integrity verification, enabling institutions to gate credentials. Revenue model scales with adoption and perceived necessity. Net beneficiary — extraction runs toward them.
constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE DETECTION COALITION (SCAFFOLD) — Organized agents (academia, open-source communities, nonprofit fact-checkers) building detection tools outside the proprietary ecosystem. See the arms race as a temporary market dysfunction with a sunset: sufficiently accurate, open, community-maintained detection would undercut extraction by eliminating the scarcity premium. Constrained by resource limitations but perceive an exit path through collective action.
constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL ACADEMIC CREDENTIALING (PITON) — The primacy of in-person, proctored examination and supervised thesis work is being maintained through performative adoption of detection tools as proxy for integrity verification. Detection paywalls actually weaken academic institutions' independence (they become dependent on vendor services), yet adoption is mandated through accreditation norms. The mechanism (detection tools) persists through institutional inertia despite undermining the function (independent assessment).
constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some verification lag between generative capability and detection capability is inherent to the adversarial game: generators always outpace detectors in an arms race, and perfect detection is mathematically impossible given sufficient computational resource disparity. This perspective naturalizes the arms race as an immutable feature of computational limits. However, the structural data contradicts the mountain classification — the constraint is contingent on design choices (public detection APIs, adversarial incentive structures, gating mechanisms) and institutional arrangements (accreditation mandates), not on physical or logical limits.
constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(synthetic_text_detection_arms_race_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(synthetic_text_detection_arms_race, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(synthetic_text_detection_arms_race, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(synthetic_text_detection_arms_race, TR),
    TR >= 0.70.

:- end_tests(synthetic_text_detection_arms_race_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting asymmetric benefit distribution. Detection vendors capture subscription revenue and market concentration gains. Institutions gain enforcement capability without bearing development costs. Access-constrained populations and open knowledge commons bear gatekeeping costs without benefiting from detection improvements. The trajectory shows acceleration (0.28 → 0.58 over 6 periods) as vendor concentration increases and institutional adoption mandates spread through accreditation systems. Suppression (0.48): Moderate. Multiple layers of suppression exist: cost barriers (detection tools are paywalled), institutional mandates (accreditation bodies increasingly expect detection), information barriers (algorithm opacity makes independent verification difficult), and structural barriers (alternative credentialing systems are still nascent). Suppression is not total because some populations have institutional access (universities with site licenses), but the barriers are sufficiently high to trap the most vulnerable populations. Theater ratio (0.65): High and increasing. Institutional adoption of detection tools often serves performative functions: demonstrating compliance with emerging norms, signaling institutional rigor, deflecting liability concerns. The actual detection accuracy and utility for core assessment functions are secondary to the institutional ritual of having a detection system. This is evident in institutional behavior: detection tool adoption frequently precedes clear pedagogical integration, suggesting the mechanism (having a tool) matters more than the outcome (actually improving assessment validity). The increasing trajectory (0.42 → 0.65) suggests that as the arms race intensifies and tool switching accelerates, institutions are adopting tools more for signaling than for genuine integrity improvement.
 *
 * PERSPECTIVAL GAP:
 *   The vendor perspective (Rope) sees pure coordination: providing signal that institutions value for gatekeeping. The educator perspective (Tangled Rope) sees mixed function: coordinating assessment validity but also bearing institutional costs of vendor lock-in and false positives. The access-constrained student perspective (Snare) sees pure extraction: credential barriers they cannot overcome. The open-source coalition perspective (Scaffold) sees a temporary dysfunction with exit: building open tools that would commodify detection and eliminate the vendor premium. The traditional credentialing perspective (Piton) sees a degraded ritual: detection adoption mandates that actually weaken institutional independence while appearing to strengthen integrity. The analytical observer perspective risks a false summit (Mountain) by naturalizing the arms race as inherent to text verification rather than contingent on design choices (public APIs for generative models, adversarial incentive structures, gating mechanisms). The gap reveals that the constraint is not about detection technology itself but about the institutional arrangements that create vendor lock-in and gatekeeping leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from their structural position relative to the extraction flow. Detection vendors (beneficiary + arbitrage exit) experience low d (~0.10-0.15) → negative effective extraction (χ). Educators (beneficiary but constrained exit due to institutional mandates) experience moderate d (~0.35-0.40) → moderate positive χ. Students and researchers (victims + trapped exit) experience high d (~0.90-0.95) → maximum χ. Open-source coalition (organized exit + victim status through knowledge commons effects) experience moderate-high d (~0.60-0.70) → high χ despite organizational capacity. The scaffold classification for the open-source coalition derives from genuine exit optionality (building alternative detection tools) and perceived sunset mechanism (commodification of open detection would undercut vendor premium), despite current trappedness. The piton classification for traditional credentialing derives from the theater gate (high performative adoption) rather than from low extractiveness — credentialing systems maintain themselves through ritual compliance with detection adoption mandates rather than through genuine functional improvement.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating genuine mixed function. The coordination element is real: institutions face a legitimate problem (verifying assessment validity in the presence of powerful generative tools) and detection systems provide signal. But the extraction element is equally real: beneficiary groups (vendors, gatekeepers) capture disproportionate value while victim groups (access-constrained populations) bear disproportionate costs. The constraint cannot be reduced to pure extraction (Snare) because educators genuinely benefit from integrity verification capacity. It cannot be reduced to pure coordination (Rope) because access barriers create asymmetric benefits. The tangled_rope classification holds exactly because both elements are structurally necessary — extraction enables coordination (vendor revenue funds detection development and adoption incentives), and coordination enables extraction (institutions willing to gate credentials require powerful detection signals). The constraint persists because dismantling either element (removing detection entirely, or removing vendor premium) would undermine the other. Open-source coalition's scaffold perspective offers a genuine alternative: if open detection tools achieve parity, the vendor premium disappears and extraction pressure decreases, while coordination function (institutions still need to gate credentials) remains. This is the sunset mechanism — not elimination of the coordination need, but elimination of the extraction premium through commodification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_falsifiability_limit,
    'Is the fundamental unprovability of text origin a logical limit or a contingent feature of current detection design?',
    'Formal analysis of information-theoretic bounds on detection; comparison of stylometric, probabilistic, and watermarking approaches; empirical limits of multi-modal verification (metadata, provenance chains, structural markers)',
    'If logical limit: mountain classification gains weight; arms race is unavoidable. If contingent: tangled_rope classification confirmed; arms race is institutional design choice subject to sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_falsifiability_limit, empirical, 'Whether text origin is fundamentally undetectable or contingently difficult').

omega_variable(
    open_detection_sufficiency,
    'Can open-source detection tools achieve institutional acceptance parity with proprietary vendors, or does the proprietary premium reflect genuine performance gaps or liability asymmetries?',
    'Comparative false positive/negative analysis across open and proprietary tools; institutional adoption patterns after tool parity; barrier analysis (liability concerns, support requirements, certification gaps)',
    'If open tools suffice: scaffold sunset is structural; open-source coalition will reduce extraction through commodification. If proprietary premium is justified: arms race persists; extraction continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_detection_sufficiency, empirical, 'Whether open-source detection achieves institutional parity').

omega_variable(
    credential_bypass_emergence,
    'Will alternative credentialing systems (project portfolios, direct competency demonstration, blockchain/cryptographic proof) emerge fast enough to make synthetic text detection gatekeeping obsolete?',
    'Growth trajectories of alternative credentialing; institutional adoption rates; cost comparison with detection-based credentialing; employer signal extraction patterns',
    'If alternatives emerge (5-10 years): scaffold sunset becomes real; institutional gatekeeping pressure decreases; extraction pressure on access-constrained populations decreases. If alternatives stall: detection arms race becomes permanent institutional feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credential_bypass_emergence, empirical, 'Whether alternative credentialing can render detection gatekeeping obsolete').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Does measured suppression (0.48) reflect material barriers (cost, algorithm opacity, institutional mandate) or internalized acceptance of detection legitimacy, or both?',
    'Post-mandate behavior analysis: if detection tools were freely available, would access-constrained populations increase synthetic text use? Survey data on perceived legitimacy vs actual tool barriers. Cross-border policy comparison: regions without detection mandates show different patterns.',
    'If structural: suppression reflects real barriers (cost, access, complexity). If internalized: agents carry suppression internally; reducing barriers alone does not eliminate constraint. If both: constraint is more stable than structural metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism in detection gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(synthetic_text_detection_arms_race, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(syndet_tr_t0, synthetic_text_detection_arms_race, theater_ratio, 0, 0.42).
narrative_ontology:measurement(syndet_tr_t2, synthetic_text_detection_arms_race, theater_ratio, 2, 0.51).
narrative_ontology:measurement(syndet_tr_t4, synthetic_text_detection_arms_race, theater_ratio, 4, 0.62).
narrative_ontology:measurement(syndet_tr_t6, synthetic_text_detection_arms_race, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(syndet_be_t0, synthetic_text_detection_arms_race, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(syndet_be_t2, synthetic_text_detection_arms_race, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(syndet_be_t4, synthetic_text_detection_arms_race, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(syndet_be_t6, synthetic_text_detection_arms_race, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(synthetic_text_detection_arms_race, enforcement_mechanism).
narrative_ontology:affects_constraint(synthetic_text_detection_arms_race, academic_credentialing_inflation).
narrative_ontology:affects_constraint(synthetic_text_detection_arms_race, generative_model_capability_acceleration).
narrative_ontology:affects_constraint(synthetic_text_detection_arms_race, institutional_surveillance_mandate_expansion).

% DUAL FORMULATION NOTE:
% The synthetic text detection arms race decomposes into three structurally distinct constraints: (1) detection_technology_arms_race (ε≈0.35, Rope) — the technical competition between generative and detection models; (2) synthetic_text_detection_arms_race (ε≈0.58, Tangled Rope) — the institutional gatekeeping and access barrier dynamics studied here; (3) academic_credentialing_extraction (ε≈0.62, Snare) — the broader credential inflation dynamic that detection tools participate in. This story focuses on the institutional constraint (2), which is downstream of generative capability acceleration and upstream of broader credentialing system degradation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(synthetic_text_detection_arms_race, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
