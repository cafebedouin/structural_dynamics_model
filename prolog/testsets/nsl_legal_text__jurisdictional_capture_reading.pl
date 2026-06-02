% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: NSL as Vehicle for Jurisdictional Capture and Common Law Erosion
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The National Security Law (NSL), enacted by mainland China in 2020 and
 *   imposed on Hong Kong under the framework of the Sino-British Joint
 *   Declaration, creates a structural vehicle for transplanting mainland
 *   legal system authority into Hong Kong's common law jurisdiction. This
 *   constraint, read through the jurisdictional_capture_reading, models NSL
 *   as a mechanism that achieves institutional control through textual
 *   formalization while preserving the facade of Hong Kong's legal autonomy.
 *   The kernel (the NSL legal text itself) is interpreted by different actors
 *   as serving different functions: mainland authorities present it as
 *   restoring sovereignty and preventing subversion
 *   (sovereignty_restoration_reading); democratic critics frame it as
 *   temporarily enclosing democratic space pending reversal
 *   (democratic_enclosure_reading); this reading instantiates the claim that
 *   NSL functions as permanent institutional capture—the systematic
 *   transplantation of mainland security apparatus authority into the Hong
 *   Kong judiciary's interpretive domain, eroding the institutional
 *   independence that common law systems require. The extractiveness
 *   trajectory (0.42 → 0.58 over four years) reflects the accumulation of
 *   precedent, institutional adaptation, and normalization of dual
 *   interpretive authority. Theater ratio growth (0.55 → 0.64) documents the
 *   increasing performative invocation of common law procedure as cover for
 *   outcomes predetermined by security-state considerations. Suppression
 *   requirement escalation (0.58 → 0.68) tracks the intensification of
 *   enforcement machinery required to maintain judicial compliance as
 *   resistance within the profession strengthens.
 *
 * KEY AGENTS:
 *   - Hong Kong Judiciary: Primary institutional victim (powerless/trapped) — stripped of interpretive autonomy, forced to operate under dual authority (common law precedent + mainland security override). Career survival depends on calibrating outcomes to security interests.
 *   - Hong Kong Legal Profession (Barristers, Solicitors): Primary professional victim (powerless/trapped) — cannot exit without career dissolution. Foundational norms (adversarial testing, client loyalty, institutional independence) subordinated to state security objectives.
 *   - Common Law Institutional Autonomy: Abstract victim (powerless/trapped) — the epistemic and procedural system itself becomes a target. Precedent-binding, statutory interpretation methods, and adversarial discovery are constrained when security override is available.
 *   - Mainland Security Apparatus (National Security Commission, Mainland Courts): Primary beneficiary (institutional/arbitrage) — gains interpretive authority over HK legal system without formal institutional merger. Can apply or relax pressure as needed; has full arbitrage options.
 *   - Chinese Central Authority: Secondary beneficiary (institutional/arbitrage) — NSL solves the political control problem while maintaining HK's international legal standing and economic function. Long-term strategy of institutional integration without formal sovereignty merger.
 *   - International Legal Community (ICC, UN bodies, Democratic States): Secondary organized agent (organized/mobile) — perceives NSL as violating international commitments and threatens countervailing pressure (sanctions, ICC investigation), but enforcement capacity is limited by mainland veto power.
 *   - International Treaty System (Sino-British Joint Declaration): Institutional actor (institutional/arbitrage) — maintains performative compliance with treaty language ('two systems') while text is systematically undermined. Theater-dependent for legitimacy but lacks enforcement mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.58).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.68).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL as Vehicle for Jurisdictional Capture and Common Law Erosion").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '482f5582-148a-4ff6-a236-4217b5933e07').
narrative_ontology:cs_kernel_codification('482f5582-148a-4ff6-a236-4217b5933e07', formalized).
narrative_ontology:cs_authority_grounding('482f5582-148a-4ff6-a236-4217b5933e07', extraction).
narrative_ontology:cs_interpretation_layer_present('482f5582-148a-4ff6-a236-4217b5933e07').
narrative_ontology:cs_reading_relation('482f5582-148a-4ff6-a236-4217b5933e07', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('482f5582-148a-4ff6-a236-4217b5933e07', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('482f5582-148a-4ff6-a236-4217b5933e07', foundational, nsl_instantiates_permanent_jurisdictional_transfer).
narrative_ontology:cs_axiom_status(nsl_instantiates_permanent_jurisdictional_transfer, holdable).
narrative_ontology:cs_axiom_grounding('482f5582-148a-4ff6-a236-4217b5933e07', nsl_instantiates_permanent_jurisdictional_transfer, empirically_contingent).
narrative_ontology:cs_axiom('482f5582-148a-4ff6-a236-4217b5933e07', foundational, common_law_institutional_autonomy_is_structural_target).
narrative_ontology:cs_axiom_status(common_law_institutional_autonomy_is_structural_target, holdable).
narrative_ontology:cs_axiom_grounding('482f5582-148a-4ff6-a236-4217b5933e07', common_law_institutional_autonomy_is_structural_target, deontological).
narrative_ontology:cs_reference_frame('482f5582-148a-4ff6-a236-4217b5933e07', hk_common_law_institutional_autonomy).
narrative_ontology:cs_drift_state('482f5582-148a-4ff6-a236-4217b5933e07', contemporary_post_nsl_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('482f5582-148a-4ff6-a236-4217b5933e07', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, chinese_central_authority).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, common_law_institutional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HK COMMON LAW PRACTITIONERS (SNARE) — Judges and barristers cannot exit the jurisdiction without abandoning professional identity and career. NSL creates dual interpretive authority (HK courts + mainland security apparatus), trapping practitioners in a system where their foundational institutional norms (rule of law, judicial independence, adversarial testing) are subordinated to security-state objectives. Maximal extraction: career survival depends on calibrating legal reasoning to political outcomes. Escape is structurally unavailable without identity dissolution.
constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HK CIVIL SOCIETY AND OPPOSITION (TANGLED ROPE) — Organized agents with some capacity for collective action and exit through migration/remittance networks, but facing severe resource constraints and legitimacy erosion. NSL both coordinates political control (provides predictable enforcement mechanism) and extracts (crushes alternative organizing capacity). Genuine institutional coordination function exists alongside asymmetric extraction. Constrained exit: emigration imposes costs (family separation, credential loss, economic downgrade) but remains possible for some.
constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAINLAND SECURITY APPARATUS (ROPE) — Benefits from NSL as a coordination mechanism that solves the central control problem without explicit institutional merger. NSL creates legal cover for security operations while preserving HK's institutional facade (judges, courts, legal procedure). Net beneficiary with maximal arbitrage: can withdraw NSL pressure when sufficient compliance is achieved, or intensify it as needed. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL COMMUNITY (SCAFFOLD) — Organized agents (ICC, UN human rights bodies, democratic governments imposing sanctions) see NSL as temporary coercive arrangement that will eventually be subject to international legal review or geopolitical reversal. Escalating sanctions and ICC jurisdiction create countervailing pressure. Low effective extraction from this perspective because international agents retain exit options (sanctions, diplomatic isolation) and perceive a sunset: NSL's legitimacy erodes as HK's integration into global rule-of-law systems weakens, creating pressure for eventual reform or geopolitical realignment.
constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL TREATY SYSTEM (PITON) — The Sino-British Joint Declaration's One Country, Two Systems commitment is substantially performative. NSL violates the text's core premise (HK's common law system and institutional autonomy) while maintaining ritual compliance (NSL is framed as 'safeguarding' HK's system). Theater ratio high: continued invocation of the treaty's framework despite systematic violation. The treaty persists as institutional form—referenced in international forums—but its functional verification mechanism (International Court of Justice, UN review bodies) is blocked by mainland veto power. Piton classification derives from the theater gate: the treaty survives through inertia and face-saving rhetoric, not because it constrains mainland conduct.
constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOPOLITICAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, the erosion of colonial legal institutions under pressure from the metropolitan power is presented as an immutable feature of decolonization and sovereignty reintegration. This perspective sees the dismantling of common law autonomy as inherent to any transfer of political control from a liberal democratic state to an authoritarian one. However, the structural data reveals this as a false summit: the 'inevitable' erosion is specifically a choice of the mainland apparatus to dismantle institutional independence rather than preserve it. Other geopolitical reintegrations (e.g., German reunification, Hong Kong's economic integration with southern China before 2020) did not require systematic legal system transplantation. The mountain framing naturalizes what is a contingent political strategy.
constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsl_legal_text__jurisdictional_capture_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, TR),
    TR >= 0.70.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. NSL achieves systematic institutional capture—HK judges lose interpretive autonomy, legal profession loses independence, common law precedent becomes conditional on security interests—but extraction is not maximal because (a) HK courts retain formal decision-making authority and can shape implementation at margins; (b) some practitioners can emigrate, creating remittance effects and professional knowledge drain rather than absolute immobility; (c) international pressure constrains the pace of institutional dismantling. If NSL were simply a security directive without legal text, extractiveness would approach 0.75 (pure Snare). The textual format and institutional preservation allow some Tangled Rope coordination function to persist. Suppression (0.68): High. Multiple suppressive mechanisms operate simultaneously: (1) structural—judges cannot overrule mainland security apparatus without facing removal; legal criteria for 'national security' are undefined and subject to mainland interpretation; (2) career-based—barristers who challenge security jurisdiction face harassment, disbarment pressure, or prosecution under NSL itself; (3) normative—the foundational institutional norms of common law are framed as obstacles to security, creating cognitive dissonance for practitioners. Escape is not structurally impossible but costs include professional identity loss, family separation, and credential devaluation. Theater ratio (0.64): Moderate-high. NSL proceedings maintain surface-level common law procedure—witness examination, cross-examination, legal argument, written judgment—while outcome determinants lie outside the legal process (security apparatus pre-determination of outcome). The theater has increased as institutions become more sophisticated in producing compliant judgments while preserving the appearance of judicial reasoning. Mainland authorities have learned that procedural theater is cheaper than transparent override and sustains international legitimacy longer.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The mainland security apparatus sees NSL as Rope—an elegant coordination mechanism that solves HK control without formal institutional destruction. HK judges see Snare—inescapable subordination to dual authority. The international community sees Scaffold—temporary coercive arrangement subject to diplomatic reversal or ICC review. Civil society sees Tangled Rope—genuine coordination of security with genuine extraction. The treaty system sees Piton—performative invocation of 'two systems' as theater covering systematic dismantling. The analytical observer risks seeing Mountain—geopolitical reintegration as inevitable—but the structural data reveals choice, not inevitability: other reintegrations (German reunification, Macau's administration) did not require systematic legal system transplantation. The gap between Rope (beneficiary perspective) and Snare (victim perspective) is approximately three classification levels, corresponding to the structural difference between beneficiary arbitrage and victim trap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position relative to NSL's extraction flow. Mainland security apparatus: beneficiary + arbitrage exit = low d (≈0.15) → negative f(d) → negative χ. They experience NSL as net benefit with full withdrawal/intensification options. HK judiciary: victim + trapped exit = high d (≈0.92) → f(d) ≈ 1.30 → high χ. Cannot exit without career dissolution; bears full institutional subordination cost. HK legal profession: victim + trapped exit = d ≈ 0.88 → high χ. Constrained exit (emigration available but costs high) would shift this to d ≈ 0.75. International community: observer/organized with mobile exit = d ≈ 0.65 → moderate χ. Retention of sanctions, ICC referral, and diplomatic isolation options limits experienced extraction. The directional overrides are minimal—the structural derivation from beneficiary/victim + exit options captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by displaying genuine Tangled Rope structure: NSL provides a coordination function (government security integration, control mechanism that avoids explicit institutional merger) while simultaneously extracting (institutional autonomy, procedural independence, common law institutional memory). The Snare perspective (HK judiciary) is not incorrect—from their position, escape is structurally impossible and extraction is severe. But Snare is a perspectival reading of a Tangled Rope structure, not a misclassification. The Rope perspective (mainland beneficiary) is also structurally accurate—NSL does coordinate security integration. The perspectival gap is the answer: NSL is a tangled structure that genuinely coordinates security integration while asymmetrically extracting institutional autonomy. No single type is 'the truth'—the constraint IS tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mainland_intent_interpretability,
    'Does NSL reflect a strategic choice by mainland authorities to transplant legal system control, or an inevitable feature of security integration in a divided sovereignty context?',
    'Comparative historical analysis: examine mainland behavior in other transferred jurisdictions (Macau, Tibet, Xinjiang) and mainland statements about HK''s legal system pre- and post-NSL. Evidence for intent: explicit directives to HK judges, security apparatus coordination with legal decisions, reversals of HK court rulings by mainland bodies. Evidence against intent: mainland deference to HK legal process in security cases, continued recruitment from international legal scholars.',
    'If intent confirmed: NSL is a deliberate extraction mechanism (Snare/Tangled Rope classifications robust). If intent unconfirmed: erosion may be incidental to security integration, and Rope perspective gains plausibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mainland_intent_interpretability, empirical, 'Whether NSL reflects deliberate legal system transplantation strategy').

omega_variable(
    common_law_institutional_memory_persistence,
    'Can HK common law institutional norms (judicial independence, adversarial testing, precedent-binding, statutory interpretation traditions) persist under NSL conditions, or are they fundamentally incompatible with security-state override authority?',
    'Case law analysis: track HK court decisions in NSL cases over 5-10 years. Measure deviation from precedent, departure from common law interpretive methods, frequency of rulings that align with mainland security interests. Survey practicing barristers and judges about perceived constraints on legal reasoning. Compare ratio of guilty verdicts in NSL cases to similar non-NSL cases in same courts.',
    'If norms persist: institutional capture is incomplete, and Tangled Rope classification is accurate (genuine coordination component remains). If norms collapse: classification should shift toward pure Snare (all coordination function eroded). If partial persistence: Piton classification becomes plausible (performative invocation of common law while actual practice follows mainland directives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_law_institutional_memory_persistence, empirical, 'Whether common law institutional norms survive NSL regime').

omega_variable(
    international_enforcement_countervailing_pressure,
    'Do international sanctions, ICC referrals, and democratic-state diplomatic isolation create sufficient countervailing pressure to prevent complete jurisdictional capture, or are they symbolic gestures that lack enforcement mechanism?',
    'Economic analysis: measure Hong Kong''s capital flight, financial center status decline, and trade disruption attributed to NSL. Monitor mainland retaliation against democratic states'' NSL-related sanctions. Track ICC prosecutor''s investigative capacity and political protection from Security Council veto. Assess whether international legal pressure constrains NSL enforcement or merely creates public record of violations.',
    'If countervailing pressure substantial: Scaffold perspective is structurally valid (international actors retain exit options and perceived sunset logic). If pressure symbolic: international perspective converges with Piton (theater without enforcement). Classification stability depends on whether international actors can actually constrain mainland behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_countervailing_pressure, empirical, 'Effectiveness of international legal and diplomatic countervailing pressure').

omega_variable(
    kernel_reading_under_determination,
    'This constraint is ONE reading of the nsl_legal_text kernel. The sibling readings (sovereignty_restoration, democratic_enclosure) attribute different mechanisms and beneficiaries to the same textual artifact. What observable distinguishes this jurisdictional_capture reading from the alternatives?',
    'Discourse analysis of mainland official statements about NSL: track language of ''system restoration,'' ''unified interpretation authority,'' and ''security supremacy over local procedure.'' Examine mainland legal scholars'' publications on how NSL structures interpretive authority. Analyze HK judiciary''s reasoning in test cases: do they invoke NSL as temporary emergency measure (democratic_enclosure reading) or as permanent institutional transplantation (jurisdictional_capture reading)? What does the drafting history show about intent?',
    'If mainland framing emphasizes restoration of jurisdiction: sovereignty_restoration reading becomes primary. If mainland framing emphasizes mainland interpretation supremacy: jurisdictional_capture reading is validated. If mainland avoids explicit theorization: the reading contest remains contested and all three readings coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'What observable distinguishes this reading from sibling readings of the NSL kernel').

omega_variable(
    common_law_vs_civil_law_compatibility,
    'Are common law and civil law systems fundamentally incompatible when both operate in the same jurisdiction under dual authority (HK courts + mainland oversight), or can a hybrid system stabilize?',
    'Jurisprudential analysis of HK cases post-NSL: examine how HK judges handle mainland-imposed interpretive guidance. Do they adapt common law methods (precedent, statutory construction) to civil law reasoning (legislative intent, security interest)? Can a stable hybrid emerge, or does dual authority force choices that collapse one system?',
    'If incompatible and compression occurs: institutional capture is accelerating, and Snare classification strengthens (no stable Tangled Rope equilibrium). If hybrid stabilizes: Tangled Rope may persist indefinitely. If compression favors common law persistence: Piton (performative civil law overlay on common law skeleton) becomes likely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_vs_civil_law_compatibility, conceptual, 'Structural compatibility of dual common law and civil law authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_jc_tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(nsl_jc_tr_t2, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2, 0.59).
narrative_ontology:measurement(nsl_jc_tr_t4, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 4, 0.64).

% Extraction over time
narrative_ontology:measurement(nsl_jc_be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nsl_jc_be_t2, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(nsl_jc_be_t4, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 4, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nsl_jc_su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(nsl_jc_su_t2, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2, 0.63).
narrative_ontology:measurement(nsl_jc_su_t4, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, hk_judicial_independence_institutional_pressure).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, common_law_precedent_mainland_override).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, rule_of_law_institutional_capture).

% DUAL FORMULATION NOTE:
% The NSL kernel generates three reading constraints with different extractiveness values: sovereignty_restoration_reading emphasizes mainland political benefit (lower extractiveness on common law targets, higher on criminal defendants); democratic_enclosure_reading emphasizes temporary crisis dynamics (lower suppression, higher perceived sunset); jurisdictional_capture_reading emphasizes permanent institutional transplantation (moderate-high extractiveness, high suppression, high theater ratio). Each reading is a distinct constraint story. The three are linked via network.affects_constraints to document the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
