% ============================================================================
% CONSTRAINT STORY: cancer_treatment_combination_protocols
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cancer_treatment_combination_protocols, []).

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
 *   constraint_id: cancer_treatment_combination_protocols
 *   human_readable: Cancer Treatment Combination Protocols
 *   domain: healthcare/oncology/medical_regulation
 *
 * SUMMARY:
 *   Standardized cancer treatment combination protocols represent a
 *   constraint that exhibits simultaneous coordination and extraction across
 *   different patient and institutional positions. These protocols
 *   (chemotherapy combination regimens, targeted therapy combinations,
 *   immunotherapy protocols) were initially developed to prevent harmful
 *   drug-drug interactions and optimize sequential treatment efficacy.
 *   However, the constraint has evolved to embed significant asymmetric
 *   extraction: pharmaceutical manufacturers maintain protected market
 *   positions through approved combinations, oncology specialists maintain
 *   practice authority through protocol compliance, and regulatory apparatus
 *   maintains institutional relevance through approval gatekeeping.
 *   Simultaneously, the coordination function remains genuine — protocols do
 *   prevent some harmful interactions and enable comparative research. The
 *   extractiveness has increased over the 30-year measurement interval (0.35
 *   → 0.61) primarily through intensification of pharmacoeconomic barriers,
 *   while theater ratio has increased (0.52 → 0.72) as regulatory approval
 *   processes have become increasingly formalized and documentation-heavy.
 *   The constraint is neither pure coordination (rope) nor pure extraction
 *   (snare) but a hybrid where both mechanisms operate structurally.
 *
 * KEY AGENTS:
 *   - Terminal patients without protocol match: Powerless/trapped victims (maximum extraction) — face binary choice between standard protocol with poor prognosis or access barriers to alternatives
 *   - Informed patients seeking experimental combinations: Moderate/constrained victims and partial beneficiaries — benefit from some safety coordination but face cost barriers and medical/legal constraints on options
 *   - Pharmaceutical manufacturers with approved combinations: Institutional/arbitrage beneficiaries — capture market protection, pricing power, and reduced liability through protocol standardization
 *   - Oncology specialists: Organized/constrained actors — benefit from protocol standardization (liability protection, practice authority) but constrained by clinical judgment limitations and licensing compliance requirements
 *   - FDA and regulatory apparatus: Institutional/arbitrage beneficiaries — maintain institutional relevance and gatekeeping authority; increasingly performative in verification function
 *   - Cancer research community: Powerful/mobile actors with mixed benefits and constraints — benefit from standardization (reproducible data) but constrained by funding concentration and publication bias
 *   - Analytical observer: Views constraint as hybrid coordination-extraction with increasing theater component indicating potential piton degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_treatment_combination_protocols, 0.58).
domain_priors:suppression_score(cancer_treatment_combination_protocols, 0.62).
domain_priors:theater_ratio(cancer_treatment_combination_protocols, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_treatment_combination_protocols, extractiveness, 0.58).
narrative_ontology:constraint_metric(cancer_treatment_combination_protocols, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cancer_treatment_combination_protocols, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_treatment_combination_protocols, tangled_rope).
narrative_ontology:human_readable(cancer_treatment_combination_protocols, "Cancer Treatment Combination Protocols").
narrative_ontology:topic_domain(cancer_treatment_combination_protocols, "healthcare/oncology/medical_regulation").

domain_priors:requires_active_enforcement(cancer_treatment_combination_protocols).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_treatment_combination_protocols, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(cancer_treatment_combination_protocols, established_treatment_protocols).
narrative_ontology:constraint_beneficiary(cancer_treatment_combination_protocols, oncology_specialists_with_practice_licenses).
narrative_ontology:constraint_victim(cancer_treatment_combination_protocols, cancer_patients_seeking_alternatives).
narrative_ontology:constraint_victim(cancer_treatment_combination_protocols, treatment_efficacy_research).
narrative_ontology:constraint_victim(cancer_treatment_combination_protocols, personalized_medicine_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TERMINAL PATIENT WITHOUT PROTOCOL MATCH (SNARE) — Patient diagnosed with cancer type or stage not covered by approved combination protocols faces stark choice: follow standard protocol with known poor outcomes or face legal/medical barriers to alternative combinations. No exit. Maximum extraction — patient's desperation is the extraction mechanism. Suppression is maximal: medical licensing boards, insurance coverage decisions, and FDA status create multiple barriers to exit.
constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFORMED PATIENT SEEKING EXPERIMENTAL COMBINATION (TANGLED ROPE) — Patient with access to oncologist and resources faces constrained exit. Some genuine coordination exists (combination protocols do prevent harmful drug interactions and reduce wasteful trial-and-error). But extraction is embedded: off-protocol combinations require private pay, international travel, or access to clinical trials with their own gatekeeping. Moderate extraction with constrained mobility.
constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER WITH APPROVED COMBINATION (ROPE) — Sees combination protocols as coordination mechanism that protects market position and ensures predictable demand for their approved drugs. Benefits from standard protocols through insurance coverage, treatment guidelines, and reduced liability. Arbitrage exit available (can lobby for broader approval, can license competitors, can invest in alternative formulations). Net beneficiary — experiences constraint as enabling coordination of market demand.
constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ONCOLOGY SPECIALIST UNDER PROTOCOL CONSTRAINTS (TANGLED ROPE) — Experienced oncologist with established practice sees protocols as both coordination (preventing harmful interactions, ensuring liability protection) and extraction (limiting clinical judgment, creating liability risk for deviations, requiring compliance documentation). Professional license depends on following approved protocols; deviating requires justifying to board, risking sanctions. Constrained mobility — can deviate but at career cost. Organized status reflects professional guild membership and collective licensing authority.
constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FDA APPROVAL APPARATUS (PITON) — The regulatory machinery that certifies combination protocols has become substantially performative. Theater includes lengthy approval timelines, byzantine trial design requirements, and documentation burden that grows independent of actual safety/efficacy gains. The apparatus persists through institutional inertia — it is the recognized authority structure for protocol legitimacy — but its functional verification capacity has degraded as combination complexity exceeds human review capacity. Theater ratio 0.68 reflects that much FDA evaluation activity is ritual compliance rather than substantive efficacy assessment.
constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CANCER RESEARCH COMMUNITY (TANGLED ROPE) — Academic and independent researchers benefit from standardized protocols (reproducible data, comparable outcomes, baseline for novel combinations) while being constrained by them (funding concentrated on approved protocols, publication bias against off-protocol research, difficulty recruiting patients for non-standard arms). Powerful institutional position but mobile exit (can publish internationally, can conduct trials in other jurisdictions). Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Constraint exhibits genuine coordination (preventing harmful drug interactions, ensuring reproducible outcomes, enabling comparative research) layered with asymmetric extraction (pharmaceutical revenue protection, specialist credential protection, regulatory capture). Classification requires both beneficiary/victim analysis and enforcement mechanisms to be present — all three are structurally evident. Theater component (0.68) indicates performative elements but not sufficient for piton (which requires theater >= 0.70 and degraded primary function). Primary function remains coordination, not pure performance.
constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_treatment_combination_protocols_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_treatment_combination_protocols, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cancer_treatment_combination_protocols, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cancer_treatment_combination_protocols, TR),
    TR >= 0.70.

:- end_tests(cancer_treatment_combination_protocols_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly through four mechanisms: (1) pharmaceutical market protection enabling premium pricing for approved combinations, (2) oncology specialist gatekeeping of off-protocol combinations, (3) insurance coverage decisions enforcing protocol adherence, (4) psychological extraction from trapped patients' desperation. However, the extraction is not maximal (0.70+) because genuine coordination function exists — drug-drug interaction prevention, outcome reproducibility, adverse event monitoring — these are real services with real value. The measurement trajectory (0.35 → 0.61) shows extraction intensifying as pharmacoeconomic barriers strengthen. Suppression (0.62): High. Multiple mechanisms suppress alternatives: legal barriers (prescribing restrictions), economic barriers (off-protocol treatment costs), institutional barriers (licensing sanctions for deviation), information barriers (publication bias against off-protocol research), and psychological barriers (learned helplessness from trapped patients). Theater ratio (0.68): Moderate-high. FDA approval processes, guideline committees, and outcome documentation have become increasingly ritualized. Much of the activity involves compliance with documentation requirements, committee review procedures, and approval timelines that grow independent of actual safety/efficacy gains. Reviewers cannot directly verify combination efficacy in every cancer subtype; instead, they verify adherence to trial design protocols, statistical thresholds, and precedent. The theater is not yet sufficient for piton classification (would require >= 0.70 and degraded primary function) because the coordination function remains functional, but trajectory suggests piton risk if theater continues rising.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (standardized combination protocols) is experienced as snare (trapped patient), rope (pharma manufacturer), tangled rope (specialist, researcher), and piton (regulatory apparatus) depending on structural position. The gap reflects genuine structural differences in extraction mechanisms and exit options, not mere disagreement. The trapped patient literally cannot exit and bears maximum extraction cost. The pharma manufacturer can arbitrage (lobby for broader approval, license to competitors, invest in alternatives) and benefits from coordination. The specialist can deviate but at professional cost (constrained not trapped). The regulatory apparatus maintains institutional position through performative approval processes. These are not different opinions about the same constraint — they are different structural relationships to the extraction and coordination mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d, where 0.0 = full beneficiary, 1.0 = full target) are derived from structural position: Pharmaceutical manufacturers with approved combinations occupy beneficiary + arbitrage position (low d ≈ 0.10, institutional canonical), producing negative effective extraction (they benefit from the constraint). Trapped terminal patients occupy victim + trapped position (high d ≈ 0.95, powerless canonical), producing maximal effective extraction (they bear full cost). Specialists occupy mixed position: beneficiary (through credential protection + liability management) + constrained exit (can deviate but at career cost), producing intermediate d ≈ 0.35, moderate-institutional hybrid. Research community occupies victim + mobile position (high d but mobile exit reduces experienced extraction through publishing options), d ≈ 0.65. The engine's sigmoid f(d) transforms these d values into experienced chi scalars that vary by time horizon and scope. Powerless/trapped position produces f(d) ≈ 1.42, institutional/arbitrage produces f(d) ≈ -0.12. This structural diversity in d values across perspectives is what generates the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID COORDINATION-EXTRACTION: This constraint resolves potential mandatrophy by demonstrating genuine structural evidence of both coordination and extraction. Coordination function is real: preventing harmful drug-drug interactions, enabling comparative outcome research, standardizing adverse event monitoring, reducing wasteful trial-and-error. Extraction function is also real: pharmaceutical revenue protection, specialist credential gatekeeping, regulatory apparatus institutional protection, and most importantly, systematic suppression of alternatives available to trapped patients. The classification as tangled rope (not rope or snare) reflects that both mechanisms are structurally essential — removing the coordination would risk patient harm from interactions, but removing the extraction would require enabling access to non-approved combinations. The measurement trajectory (theater rising from 0.52 to 0.72 while extractiveness rises from 0.35 to 0.61) shows that the constraint is drifting toward piton: the theater component is intensifying while the coordination function's necessity is being questioned by genomic medicine advances. The mandate is neither purely that protocols are good (which would justify treating as rope) nor purely that they extract (which would justify snare) but that institutional structures have embedded extraction into what began as genuine coordination, and that embedding is becoming increasingly difficult to justify as scientific alternatives mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_innovation_tradeoff,
    'Do standardized combination protocols genuinely improve patient outcomes on average, or do they primarily protect established market positions while suppressing higher-efficacy alternatives?',
    'Longitudinal outcome comparison: patients on approved standard protocols vs patients on non-standard combinations (matched for cancer type/stage/demographics). Analysis of time lag between novel combinations showing efficacy and FDA approval for standard protocols.',
    'If standard protocols improve outcomes: constraint is primarily coordination (Rope emphasis). If outcomes are equivalent or worse: constraint is primarily extraction (Snare emphasis). If mixed: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_innovation_tradeoff, empirical, 'Whether standardized protocols improve outcomes or suppress innovation').

omega_variable(
    personalized_medicine_obsolescence,
    'As genomic profiling and AI-driven drug selection mature, do fixed combination protocols become structurally obsolete, making the suppression mechanism a vestigial protection of outdated institutional arrangements?',
    'Tracking adoption of personalized treatment selection vs protocol-driven selection; comparison of outcomes between protocol-selected and genomically-selected combinations; timeline of institutional resistance to protocol replacement.',
    'If genomic selection demonstrably superior: constraint is degrading toward piton status. If institutional barriers prevent adoption despite superiority: constraint has become pure extraction mechanism protecting obsolete institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(personalized_medicine_obsolescence, empirical, 'Whether genomic profiling and AI rendering protocols obsolete').

omega_variable(
    international_variation_in_protocol_rigor,
    'Do countries with less rigid protocol enforcement show meaningfully different patient outcomes (better or worse) than those with strict FDA-equivalent governance?',
    'Comparative outcome analysis between jurisdictions with different protocol enforcement strictness; analysis of protocol-deviation frequency and associated outcome variance in high-variance vs low-variance enforcement regimes.',
    'If less-rigid enforcement produces better outcomes: protocols are extractive constraints. If worse outcomes: protocols provide genuine safety function despite theater. If equivalent: theater dominates safety contribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_variation_in_protocol_rigor, empirical, 'International variation in protocol enforcement and outcomes').

omega_variable(
    commercial_bias_in_combination_selection,
    'To what extent do approved combination protocols reflect pharmaceutical company commercial interests (which combinations are patented/profitable) vs pure efficacy optimization?',
    'Analysis of approved protocol composition: are combinations preferentially selected from same-company drug portfolios? Are superior single-agent or competing-company combinations excluded? Comparison of approval timeline for profitable vs non-profitable combinations.',
    'High commercial bias indicates primary extraction mechanism. Low bias indicates genuine coordination. Evidence of exclusion of superior alternatives indicates constraint is suppressing innovation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_bias_in_combination_selection, empirical, 'Commercial bias in protocol combination selection').

omega_variable(
    informed_consent_paradox,
    'Can patients genuinely consent to standard protocol when alternatives are legally/medically inaccessible, or does the constraint make consent theater?',
    'Analysis of informed consent disclosures: are alternative combinations mentioned and their unavailability explained? Patient surveys on perceived choice and understanding of alternatives. Legal analysis of whether informed consent can be genuine when alternatives are systematically unavailable.',
    'If alternatives are disclosed and accessible: consent is genuine (coordination). If alternatives are hidden or presented as ''not options'': consent is theater (extraction). If legally/medically inaccessible but disclosed: consent is informed but not free (suppression mechanism evident).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informed_consent_paradox, conceptual, 'Whether informed consent can be genuine when alternatives are inaccessible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_treatment_combination_protocols, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canc_combo_tr_t0, cancer_treatment_combination_protocols, theater_ratio, 0, 0.52).
narrative_ontology:measurement(canc_combo_tr_t10, cancer_treatment_combination_protocols, theater_ratio, 10, 0.62).
narrative_ontology:measurement(canc_combo_tr_t20, cancer_treatment_combination_protocols, theater_ratio, 20, 0.68).
narrative_ontology:measurement(canc_combo_tr_t30, cancer_treatment_combination_protocols, theater_ratio, 30, 0.72).

% Extraction over time
narrative_ontology:measurement(canc_combo_be_t0, cancer_treatment_combination_protocols, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(canc_combo_be_t10, cancer_treatment_combination_protocols, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(canc_combo_be_t20, cancer_treatment_combination_protocols, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(canc_combo_be_t30, cancer_treatment_combination_protocols, base_extractiveness, 30, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_treatment_combination_protocols, resource_allocation).
narrative_ontology:boltzmann_floor_override(cancer_treatment_combination_protocols, 0.18).
narrative_ontology:affects_constraint(cancer_treatment_combination_protocols, pharmaceutical_market_pricing_power).
narrative_ontology:affects_constraint(cancer_treatment_combination_protocols, cancer_patient_informed_consent_framework).
narrative_ontology:affects_constraint(cancer_treatment_combination_protocols, personalized_medicine_genomic_profiling).
narrative_ontology:affects_constraint(cancer_treatment_combination_protocols, medical_licensing_and_specialist_gatekeeping).

% DUAL FORMULATION NOTE:
% Cancer treatment combination protocols coordinate multiple functions simultaneously: drug interaction prevention (information_standard type), resource allocation (hospital/insurance coverage decisions), and identity coordination (specialist licensing and professional authority). This story treats the constraint holistically. Decomposed analysis would separate: (1) pharmacological safety constraints (coordination-dominant, lower extractiveness), (2) pharmaceutical market protection (extraction-dominant), (3) regulatory gatekeeping (increasingly performative/piton-like). Current story emphasizes integrated tangled rope classification; decomposition would show coordination floor at 0.30-0.40 (safety), extraction component at 0.55-0.70 (market/regulatory), theater component at 0.65-0.75 (regulatory ritual).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cancer_treatment_combination_protocols, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
