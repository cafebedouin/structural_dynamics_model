% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Vaccine Mandate Balance (Public Health Primary Reading)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of a contested kernel: the
 *   public_health_primary reading of vaccine_mandate_balance. The kernel
 *   itself is a foundational disagreement about when state power can override
 *   individual medical autonomy. Three irreducible readings compete:
 *   bodily_autonomy_primary (individual consent is inviolable),
 *   proportionality_reading (mandates permissible only when strict thresholds
 *   are met), and public_health_primary (collective protection supersedes
 *   individual consent when voluntary compliance fails and vulnerable
 *   populations face lethal risk). This story generates ONLY the
 *   public_health_primary reading as a clean, ε-invariant constraint. The
 *   other readings are separate constraint stories with their own
 *   base_properties, perspectives, and empirical support. The structural
 *   delta for this reading: immunocompromised populations enter the
 *   beneficiary set (protected by herd immunity), unvaccinated-coerced
 *   populations enter the victim set (subordinated by necessity), and mandate
 *   enforcement mechanisms contribute high extractiveness. The reading's
 *   viability depends on three structural claims: (1) voluntary compliance
 *   genuinely fails to achieve herd immunity at any reasonable incentive
 *   level, (2) the vulnerable population faces documented lethal exposure
 *   without protection, and (3) mandate suppression is proportional to the
 *   protective gain. If any of these fails empirically, the reading collapses
 *   from public_health_primary into either proportionality_reading (if
 *   suppression is excessive) or bodily_autonomy_primary (if voluntary
 *   compliance succeeds).
 *
 * KEY AGENTS:
 *   - Immunocompromised Population: Primary beneficiary (institutional/arbitrage) — protected by herd immunity achieved through mandate; faces lethal exposure if vaccination rate falls below threshold; primary justification for the reading
 *   - Coerced Unvaccinated Population: Primary victim (powerless/trapped) — faced with employment loss, education exclusion, social stigma; no exit options; consent subordinated by the reading to collective necessity
 *   - Public Health Authority: Secondary beneficiary (institutional/arbitrage) — coordinates herd immunity achievement; experiences mandate as solving collective action problem; has institutional capacity to enforce
 *   - Unvaccinated-by-Conscience: Secondary victim (moderate/constrained) — beliefs conflict with mandate requirement; faces high suppression costs; distinguished from powerless victim by potential exemption pathways
 *   - Harm-Reduction Governance (Ethics Boards, Exemption Committees): Organized actor (organized/constrained) — implements proportionality gates; enforces sunset conditions; determines whether constraint functions as scaffold or degrades to snare
 *   - Regulatory Analyst (Identity-Locked): Observer (analytical/identity_locked) — embedded in public health institutions; recognizes extraction-adjacent incentives (career advancement through crisis); cannot exit without abandoning institutional identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.58).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Vaccine Mandate Balance (Public Health Primary Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'e53ae592-ebb4-4e4b-8ad4-d365537a5927').
narrative_ontology:cs_kernel_codification('e53ae592-ebb4-4e4b-8ad4-d365537a5927', formalized).
narrative_ontology:cs_authority_grounding('e53ae592-ebb4-4e4b-8ad4-d365537a5927', extraction).
narrative_ontology:cs_interpretation_layer_present('e53ae592-ebb4-4e4b-8ad4-d365537a5927').
narrative_ontology:cs_reading_relation('e53ae592-ebb4-4e4b-8ad4-d365537a5927', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('e53ae592-ebb4-4e4b-8ad4-d365537a5927', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('e53ae592-ebb4-4e4b-8ad4-d365537a5927', foundational, necessity_overrides_autonomy).
narrative_ontology:cs_axiom_status(necessity_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('e53ae592-ebb4-4e4b-8ad4-d365537a5927', necessity_overrides_autonomy, deontological).
narrative_ontology:cs_axiom('e53ae592-ebb4-4e4b-8ad4-d365537a5927', foundational, vulnerable_protection_justifies_mandate).
narrative_ontology:cs_axiom_status(vulnerable_protection_justifies_mandate, holdable).
narrative_ontology:cs_axiom_grounding('e53ae592-ebb4-4e4b-8ad4-d365537a5927', vulnerable_protection_justifies_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('e53ae592-ebb4-4e4b-8ad4-d365537a5927', collective_health_primacy_framework).
narrative_ontology:cs_drift_state('e53ae592-ebb4-4e4b-8ad4-d365537a5927', endemic_phase_persistent_vaccine_hesitancy, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e53ae592-ebb4-4e4b-8ad4-d365537a5927', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, infants_ineligible_for_vaccination).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunosuppressed_recipients).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED UNVACCINATED (SNARE) — Faces maximal suppression: employment loss, education exclusion, social stigma, legal liability. No meaningful exit options within the mandate regime. The reading subordinates their consent to collective necessity, rendering them targets of extraction (mandated compliance) with minimal coordination benefit accruing to them individually. They perceive pure coercion.
constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC HEALTH AUTHORITY (ROPE) — Benefits from mandate as a coordination mechanism: achieves herd immunity threshold efficiently, reduces hospitalization burden, protects vulnerable populations. Experiences the mandate as solving a genuine collective action problem (voluntary uptake insufficient). The authority has institutional arbitrage capacity — can exit individual constraints while maintaining the system. Net beneficiary from the coordination achieved.
constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: IMMUNOCOMPROMISED POPULATION (TANGLED ROPE) — Faces genuine lethal exposure risk in the absence of herd immunity protection. Benefits from mandatory vaccination of others (herd immunity effect reduces their infection risk). But also constrained by enforcement mechanisms that may limit their own treatment options or create social pressure. Experiences both genuine protection (coordination benefit) and asymmetric extraction (their vulnerability is the justification for restricting others' autonomy, yet their own needs may be subordinated to aggregate benefit calculations). Mid-range experienced extraction.
constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: UNVACCINATED-BY-CONSCIENCE (TANGLED ROPE) — Experiences both coordination and extraction. The public health reading treats conscience as subordinate to necessity; the mandate's suppression is high (employment/education loss). But some coordination exists if mandates are time-limited or include medical exemption procedures (sunset logic). If the mandate lifts when herd immunity is achieved, the constraint has a genuine endpoint. Constrained exit means the cost is high but theoretically temporary.
constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HARM-REDUCTION GOVERNANCE (SCAFFOLD) — Organized agents (public health committees, ethics boards, exemption processes) attempting to create a temporary, proportional mandate structure with clear sunset conditions: mandate persists while R-effective > herd immunity threshold, lifts when vulnerability drops. Theater is low (genuine protective function, not performative) if exemption processes are robust and enforcement targets transmission risk, not ideology. The reading's legitimacy depends on this scaffold — if turned into permanent coercion without proportionality checks, it collapses into snare.
constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the public health reading risks naturalizing the mandate as an immutable epidemiological necessity: 'When voluntary compliance fails to achieve herd immunity, collective protection necessarily supersedes individual consent.' This perspective treats the subordination of autonomy as a law of nature rather than a contingent institutional choice. However, this reading is a false summit candidate: the structural data reveals multiple alternatives (graduated escalation, target-group mandates, incentive structures, information campaigns) were not exhausted. The 'necessity' naturalizes what was actually a policy choice.
constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REGULATORY ANALYST (TANGLED ROPE / IDENTITY-LOCKED) — An analyst embedded in public health institutions may recognize that the public health framing rationalizes what is actually extraction-adjacent institutional behavior: career advancement through crisis management, budget expansion during emergencies, elimination of inconvenient procedural checks. But the analyst's professional identity is fused with institutional legitimacy — calling out the extraction mechanism would require abandoning their institutional position and epistemic authority. This perspective instantiates the oracle gap: the analyst can see the structure from cross-position analysis but cannot exit from within their identity frame.
constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, tangled_rope,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vaccine_mandate_balance__public_health_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting genuine enforcement costs borne by unvaccinated populations but also real protective benefit to vulnerable populations. This is not pure extraction (which would score ≥0.66) because the reading's beneficiary set includes immunocompromised agents who genuinely cannot protect themselves. The extractiveness rises from 0.35 to 0.58 over the interval, reflecting enforcement intensification as voluntary uptake stalls and mandate mechanisms shift from encouragement to coercion. At t9, extractiveness plateaus at 0.58, indicating the suppression equilibrium is reached. Suppression (0.72): High, reflecting multiple enforcement mechanisms (employment loss, education exclusion, social consequences, legal liability). Suppression rises from 0.40 to 0.72 over the first 6 time units as enforcement shifts from voluntary encouragement to mandatory compliance, then decreases slightly to 0.62 at t9 as legal challenges and political pressure create modest exemption pathways. This trajectory indicates enforcement ratcheting followed by limited relaxation. Theater ratio (0.35): Low, indicating the protective function is genuine and not primarily performative. The public health reading's strength depends on this: if theater_ratio were high (≥0.70), the constraint would be piton (degraded ritual), not tangled_rope. The measured theater indicates that mandate enforcement is directed at actual transmission prevention, not at maintaining institutional appearance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across its observation site. The coerced unvaccinated see pure snare (high suppression, no benefit, no exit). The public health authority sees rope (solving coordination problem, achieving herd immunity). The immunocompromised see tangled rope (genuine protection but constrained by enforcement mechanisms that may limit their own autonomy). The conscience-unvaccinated see tangled rope (some coordination, high suppression, potential for exemptions). The harm-reduction governance framework sees scaffold if exempt processes are real and sunset is enforced. The analytical observer risks seeing mountain (naturalizing necessity) but structural data reveals false summit (alternatives were not exhausted, necessity was policy choice). The regulatory analyst sees tangled rope but cannot act on the recognition that institutional incentives, not purely epidemiological necessity, drive mandate intensity. The perspectival gaps measure the reading's internal coherence: if all observers saw the same type, the reading would be uniform (like a genuine natural law). The maximum gaps here indicate that the reading's legitimacy is contested — different structural positions generate incompatible classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The public_health_primary reading generates directionality through structural necessity: the unvaccinated-coerced are positioned as targets of the mandate (d≈0.88 derived from victim status + trapped exit), experiencing high effective extraction. The immunocompromised are positioned as beneficiaries of the protective effect (d≈0.15 derived from beneficiary status + constrained exit), experiencing low or negative effective extraction. The public health authority has institutional arbitrage (d≈0.05), benefiting from coordination. This directionality structure distinguishes the public_health_primary reading from the bodily_autonomy_primary reading, which would position the coerced as victims (same d) but the immunocompromised as irrelevant to the analysis (not in the beneficiary set, because that reading subordinates collective benefit to autonomy). The proportionality_reading would have identical directionality but different threshold gates on suppression and extractiveness — it would accept the same structural relationships but reject them as unjustifiable if suppression exceeds strict proportionality bounds or if voluntary alternatives could achieve the herd immunity threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITTER STRUCTURE RESOLUTION: This reading's mandatrophy resolves through the empirical questions encoded in omegas 1–3: Does voluntary compliance genuinely fail at scale? Is the coercion-response gradient necessary (maximum coercion required) or extractive overhead? Does the vulnerable population analysis hold? If empirical resolution answers affirmatively on all three, the reading stands as coherent tangled_rope with real justification for suppression (proportional to protective gain). If empirical resolution answers negatively on any, the reading collapses: if voluntary compliance succeeds, shift to proportionality_reading (lower mandate thresholds); if coercion is excessive relative to uptake gain, the reading becomes snare (extractive overhead); if vulnerable population analysis fails, the reading loses its beneficiary set (and thus its justification) and becomes pure snare. The mandatrophy is not internal inconsistency (the reading is internally consistent) but empirical contingency: the reading's viability depends entirely on whether its factual premises hold. This is a feature, not a bug — it routes the reading's legitimacy through reality rather than through abstract principle, which is exactly how the public_health_primary reading legitimates state power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herd_immunity_threshold_determination,
    'What is the epidemiologically defensible herd immunity threshold for the specific pathogen and variant? And who determines this threshold — technical experts, elected officials, or negotiated consensus?',
    'Comparison of R-effective empirical estimates across independent epidemiological modeling groups; documentation of threshold-setting process (expert vs political); post-hoc analysis of whether declared threshold matched actual immunity achieved',
    'If threshold is scientifically justified and transparently derived: scaffold''s sunset is credible. If threshold is inflated or politically determined: mandate persists despite herd immunity achievement, collapsing from scaffold into tangled_rope or snare. If threshold is abandoned entirely: scaffold loses its temporal structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(herd_immunity_threshold_determination, empirical, 'Herd immunity threshold determination and authority').

omega_variable(
    voluntary_compliance_counterfactual,
    'Under what conditions would voluntary compliance achieve herd immunity without mandate? What is the counterfactual vaccination rate under graduated incentives, targeted campaigns, or information provision at comparable cost?',
    'Comparative analysis of vaccination uptake across jurisdictions with different mandate intensity; controlled studies of incentive structures vs mandates; modeling of demand elasticity for vaccine uptake',
    'If voluntary compliance genuinely fails at any reasonable incentive level: mandate is the least-extractive option (proportional). If counterfactual shows voluntary compliance achievable at modest cost: mandate is unjustified extraction. If uncertain: uncertainty itself justifies the scaffold structure (temporary mandate with escalation gates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_counterfactual, empirical, 'Whether voluntary compliance could achieve herd immunity').

omega_variable(
    coercion_severity_calibration,
    'What is the measured relationship between mandate intensity (employment loss vs education exclusion vs social stigma) and vaccination uptake? Is maximum coercion necessary to achieve the herd immunity threshold, or do lower-intensity measures suffice?',
    'Empirical data on vaccination uptake across jurisdiction-specific mandate structures; measurement of coercion-response gradient; cost-benefit analysis of incremental coercion vs uptake gain',
    'If uptake plateaus below threshold at low coercion: maximum coercion is extractive overhead, collapsing the mandate from necessary protection into snare. If uptake continues to scale with coercion: mandate suppression is proportional to its protective function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_severity_calibration, empirical, 'Relationship between mandate intensity and vaccination uptake').

omega_variable(
    vulnerable_population_protection_necessity,
    'In the absence of a general mandate, would targeted protection measures (resources for immunocompromised, prioritized vaccination for caregivers, medical exemptions for high-risk cases) achieve equivalent harm reduction at lower autonomy cost?',
    'Comparison of harm outcomes under general-mandate vs targeted-protection scenarios; epidemiological modeling of partial herd immunity combined with concentrated protective resources; cost analysis of targeted interventions',
    'If targeted measures achieve equivalent outcomes: general mandate is unjustified extraction, reading should shift to proportionality_reading. If general mandate is materially superior: public_health_primary reading is structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_protection_necessity, empirical, 'Whether targeted protection could replace general mandates').

omega_variable(
    exemption_process_gateway,
    'Are exemption processes (medical, religious, conscientious) genuine procedural gates or performative theater that rarely succeed in practice?',
    'Analysis of exemption approval rates across jurisdictions; documentation of exemption criteria and their application; comparison of stated criteria to actual administrative practice; qualitative accounts from exemption requesters',
    'If exemption is genuine: mandate functions as tangled_rope with proportional enforcement. If exemption is theater: mandate is snare (coercion disguised as procedure). Theater_ratio assessment depends on this finding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exemption_process_gateway, empirical, 'Genuineness of exemption procedures').

omega_variable(
    reading_committer_kernel_contest,
    'Is the public_health_primary reading instantiating a genuine alternative normative foundation (''necessity subordinates autonomy'') or is it merely a policy implementation choice within a shared commitment-system framework?',
    'Jurisprudential analysis of constitutional traditions: does the framework contain explicit provision for emergency subordination of individual rights (e.g., police powers doctrine), or is necessity-driven override a constructed extension? Comparison across constitutional traditions.',
    'If genuine alternative foundation: the three readings (bodily_autonomy_primary, proportionality_reading, public_health_primary) genuinely foreclose each other — they cannot coexist in a single framework. If merely policy implementation: all three can coexist as different thresholds within a shared commitment to proportionality. This determines the reading_relations classification (forecloses vs coexists_with).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_kernel_contest, conceptual, 'Whether public_health_primary is an alternative foundation or implementation choice').

omega_variable(
    sunset_clause_enforceability,
    'Once a mandate is enacted, do democratic and institutional mechanisms actually enforce its sunset when herd immunity is achieved? Or do mandates persist after their justification expires?',
    'Historical analysis of emergency public health measures: How many were formally repealed after their triggering condition was met vs how many persisted until political pressure forced removal? Post-hoc evaluation of announced sunset conditions vs actual removal.',
    'If mandates reliably sunset: scaffold structure is credible. If mandates persist despite threshold achievement: scaffold degrades into tangled_rope or snare (permanent extraction justified by temporary necessity). This affects the viability of the public_health_primary reading as a stable constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, empirical, 'Whether emergency health mandates actually sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmb_theater_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(vmb_theater_t6, vaccine_mandate_balance__public_health_primary, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(vmb_extract_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vmb_extract_t3, vaccine_mandate_balance__public_health_primary, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(vmb_extract_t6, vaccine_mandate_balance__public_health_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(vmb_extract_t9, vaccine_mandate_balance__public_health_primary, base_extractiveness, 9, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vmb_suppress_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vmb_suppress_t3, vaccine_mandate_balance__public_health_primary, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(vmb_suppress_t6, vaccine_mandate_balance__public_health_primary, suppression_requirement, 6, 0.72).
narrative_ontology:measurement(vmb_suppress_t9, vaccine_mandate_balance__public_health_primary, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel has three structurally distinct readings. Each reading generates a separate constraint story with different ε values, beneficiary/victim sets, and empirical dependencies. This story (public_health_primary) links to its sibling readings via network.affects_constraints. The public_health_primary reading influences both siblings: if empirical evidence confirms that voluntary compliance fails at scale, it pressures the proportionality_reading to accept higher mandate intensity; it forecloses the bodily_autonomy_primary reading if the vulnerable population analysis holds. Conversely, evidence that voluntary compliance succeeds influences this reading toward proportionality_reading structure. The constraint family is a presheaf of empirical-conditional readings: which reading is justified depends on which empirical conditions hold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, powerless, 0.88).
constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
