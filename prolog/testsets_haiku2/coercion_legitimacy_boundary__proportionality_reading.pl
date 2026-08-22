% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Disease-Proportional Coercion Legitimacy (Proportionality Reading)
 *   domain: public_health/constitutional/medical_ethics
 *
 * SUMMARY:
 *   Public health authorities claim the right to mandate vaccination or
 *   quarantine when a pathogen meets certain epidemiological thresholds: high
 *   basic reproduction number (R0), high case fatality rate (CFR), or rapid
 *   transmission dynamics. Measles (R0~12-18, CFR~0.2%) is presented as
 *   justifying mandates; seasonal influenza (R0~1.5, CFR~0.1%) is not. This
 *   constraint embodies the proportionality reading: coercion legitimacy
 *   scales with disease severity and transmission dynamics. Victims include
 *   vaccine hesitants, refusers, and exemption-seekers, who bear autonomy
 *   costs when a pathogen crosses the threshold. Beneficiaries include
 *   immunologically vulnerable populations (who depend on others' compliance
 *   for herd immunity) and public health authorities (who gain enforcement
 *   power). The constraint is contested by those who hold bodily autonomy as
 *   categorically inalienable—they reject the proportionality frame itself.
 *   The measurement series track extractiveness rising during outbreak
 *   scenarios and falling when diseases become endemic or understood as
 *   low-risk.
 *
 * KEY AGENTS:
 *   - Public health authorities (institutional, agenda-setter): set and enforce the R0/CFR threshold for coercion; declare which pathogens justify mandates.
 *   - Vaccine hesitants and refusers (moderate power, constrained exit): bear autonomy costs when pathogens cross the threshold; face employment loss, school exclusion, travel restriction.
 *   - Immunologically vulnerable populations (powerless, trapped exit): benefit from herd immunity maintained by coercion on others; have no alternative protection.
 *   - Bodily autonomy advocates (organized, excluded): argue that no pathogen severity justifies medical coercion; contest the proportionality frame itself.
 *   - Epidemiologists (analytical seat): produce the R0/CFR data that trigger coercion; define what 'severity' means.
 *   - State legislatures (institutional, dual setter/observer): enact laws granting coercion power; can reshape the threshold via legislation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.52).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.68).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Disease-Proportional Coercion Legitimacy (Proportionality Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health/constitutional/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '5fff2d95-f55c-42fd-80ba-2f5c8aee3a07').
narrative_ontology:cs_kernel_codification('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', formalized).
narrative_ontology:cs_authority_grounding('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', extraction).
narrative_ontology:cs_interpretation_layer_present('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07').
narrative_ontology:cs_reading_relation('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_axiom('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', foundational, coercion_legitimacy_scales_with_epidemiological_severity).
narrative_ontology:cs_axiom_status(coercion_legitimacy_scales_with_epidemiological_severity, holdable).
narrative_ontology:cs_axiom_grounding('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', coercion_legitimacy_scales_with_epidemiological_severity, empirically_contingent).
narrative_ontology:cs_axiom('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', foundational, herd_immunity_justifies_autonomy_constraint_for_high_r0_disease).
narrative_ontology:cs_axiom_status(herd_immunity_justifies_autonomy_constraint_for_high_r0_disease, holdable).
narrative_ontology:cs_axiom_grounding('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', herd_immunity_justifies_autonomy_constraint_for_high_r0_disease, instrumental).
narrative_ontology:cs_reference_frame('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', medical_autonomy_with_proportionality_exception).
narrative_ontology:cs_drift_state('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', contemporary_institutional_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5fff2d95-f55c-42fd-80ba-2f5c8aee3a07', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, immunologically_vulnerable_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, outbreak_threat_communities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, medical_exemption_seekers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, treatment_refusers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set disease-control policy, declare which pathogens justify mandatory vaccination or quarantine, determine the R0/mortality threshold that triggers coercion. Claim the authority to balance individual autonomy against collective harm. Implement and enforce the constraint through school attendance rules, employment requirements, travel restrictions. Assess disease severity and transmission parameters scientifically and translate those assessments into legal mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Cannot mount immune response to many pathogens (infants, immunocompromised, elderly). Depend entirely on herd immunity thresholds maintained by coercion of others. A high-severity, high-transmission disease poses direct existential threat; without vaccination mandates on others, they have zero exit options. The constraint protects them through the coerced compliance of surrounding populations.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, immunologically_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Communities in the path of a high-R0 outbreak (measles: R0~12-18; flu: R0~1-3). Face rapid disease spread if coercion is absent. When a severe pathogen circulates, the constraint prevents exponential infection growth. When a mild pathogen circulates, the benefit is marginal—most unvaccinated survive—and the coercion cost outweighs it. The reading frames the constraint's legitimacy as proportional to this epidemiological fact.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, outbreak_threat_communities, beneficiary,
    organized, biographical, constrained, regional).

% Refuse vaccination on grounds of medical liberty, bodily autonomy, risk assessment, or religious belief. Face employment loss, school exclusion, travel restrictions when a pathogen is declared high-severity. Under this reading, they bear the extraction cost when the disease meets the proportionality threshold (measles) but not when it does not (seasonal flu). The legitimacy of the coercion is contingent on external epidemiological facts, not their consent.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vaccine_hesitant_individuals, payer,
    moderate, biographical, constrained, national).

% Have documented medical contraindications to certain vaccines (severe allergies, prior adverse events). Seek exemption from mandates and protection through herd immunity on others' compliance. Under high-severity-disease scenarios, they are caught: the coercion falls on others (who must vaccinate to protect them), and they are forced to rely on the collective's immunity. Under low-severity scenarios, exemptions are often granted more liberally. Identity-locked because their medical history becomes administratively visible and defines their legal status in outbreak scenarios.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, medical_exemption_seekers, payer,
    moderate, biographical, identity_locked, national).

% Refuse quarantine, isolation, or antiviral treatment during active infection, claiming freedom of movement or medication choice. During high-severity outbreaks (smallpox, measles), are compelled into isolation to prevent transmission. During low-severity outbreaks (seasonal flu), isolation orders are inconsistently enforced or absent. The reading ties the legitimacy of the coercion to whether the disease parameters justify it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, treatment_refusers, payer,
    moderate, biographical, constrained, national).

% Provide the empirical basis for proportionality adjudication: R0 estimates, case fatality rates, hospitalization curves, population-level immunity thresholds. Their measurements are the primary input to the question 'does this disease justify coercion under the proportionality reading?' They do not decide policy, but the constraint's application depends on the data they produce.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, infectious_disease_epidemiologists, observer,
    analytical, generational, analytical, global).

% Argue that medical autonomy is categorically inalienable—no pathogen severity justifies compelled medical intervention. They contest the proportionality reading on foundational grounds (axiom_overriding). Would argue that measles does not justify mandates if bodily autonomy is the core principle. Excluded from the policy conversation because the proportionality reading has been institutionalized; their objection would reframe the entire kernel.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_advocates, excluded,
    powerful, generational, analytical, national).

% Enact laws that grant public health authorities coercion power and establish exemption criteria (religious, philosophical, medical). Some legislatures have delegated threshold-setting to health authorities; others have constrained it by statute. Serve as the final institutional gate for constraint enforcement. Can reshape or repeal the constraint via legislation, but rarely do so except in response to large outbreaks or political pressure.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, state_legislatures, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__proportionality_reading, state_legislatures, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains herd immunity thresholds for high-transmission, high-severity pathogens (measles, smallpox, pertussis). Solves the free-rider problem: individuals have weak incentive to vaccinate when disease prevalence is low, but collective protection requires threshold coverage. Coercion overcomes the incentive gap by enforcing participation when epidemiological parameters make collective protection a justified state interest.
% TRANSFER_FUNCTION: Moves bodily autonomy (the right to refuse medical intervention) from vaccine hesitants, refusers, and treatment refusers to immunologically vulnerable populations and outbreak-threatened communities. The transfer is framed as legitimate and proportional when epidemiological severity is high (measles: CFR~0.2%, R0~12-18), and contested or withdrawn when severity is low (flu: CFR~0.1%, R0~1.5).
% ABSENT_VOICES: Those who hold bodily medical autonomy as categorically inalienable—libertarian activists, some religious communities, and bodily-autonomy-primary jurists—are excluded from the proportionality framework itself. They would object that the entire proportionality premise is wrong: no pathogen severity justifies medical coercion. This is a structural exclusion from the reading's logic, not a negotiable policy disagreement.
% DISAPPEARANCE_RATIONALE: If coercion disappeared, high-R0 outbreaks would occur without herd immunity suppression (measles elimination would reverse); low-severity diseases would circulate without mandated vaccination (flu would persist endemically with low but nonzero hospitalizations). Vulnerable populations would face uncontrolled risk. The proportionality reading accepts this outcome for low-severity pathogens but not for high-severity ones—the verdict is contested because the reading itself is contested.
% FOUNDING_PROBLEM: Historical measles and smallpox epidemics demonstrated exponential spread when vaccination fell below herd immunity thresholds, causing mass mortality particularly in vulnerable populations (infants, immunocompromised). Vaccination itself creates free-rider incentives: if most are vaccinated, unvaccinated individuals gain protection without risk. Early vaccination campaigns faced participation gaps because individual incentives diverged from collective interests. The constraint was built to overcome this free-rider problem by mandating coverage when disease severity made collective protection a state interest.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists corroborate that measles (R0~12-18, CFR~0.2%) requires ~95% herd immunity to prevent outbreaks, and that voluntary vaccination alone does not achieve this threshold in all populations (founding problem is live for high-severity pathogens). Public health authorities argue the founding problem persists for any high-R0 disease. Bodily autonomy advocates dispute whether the problem justifies coercion—they argue voluntary vaccination plus education and outreach can achieve thresholds without mandate. Historical data on smallpox elimination (through coercion-backed campaigns) support the empirical link between coercion and threshold achievement. Contemporary data on measles elimination in countries with high-uptake voluntary programs show mixed results—some achieve elimination without coercion, others require it. The founding problem status is live for high-severity pathogens but contested for low-severity ones where voluntary programs often suffice.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, contested).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the constraint genuinely solves a coordination problem for high-severity pathogens, but its legitimacy depends on factual assessments (R0, CFR) that are contingent and contestable. When applied to low-severity pathogens, the extraction becomes visible as coercion without corresponding collective benefit. Suppression is substantial (0.68) because the constraint requires active enforcement: school exclusion, employment mandates, quarantine powers. Theater ratio is moderate (0.42) because the public health justification is real for high-severity diseases (epidemiological benefit is genuine) but becomes performative for low-severity diseases (the disease is endemic, risk is low, but coercion persists through institutional inertia). Accessibility collapse is high (0.71) because once a pathogen is designated 'high-severity,' alternatives collapse: you cannot opt out of herd immunity thresholds; vaccination or quarantine become the only path to participation in schools, employment, or travel. Resistance is high (0.74) because substantial populations contest the proportionality frame itself—bodily autonomy advocates, religious exemption-seekers, and those skeptical of government medical authority resist the constraint's legitimacy. The measurement series show extractiveness and suppression rising during outbreak scenarios (when the proportionality justification is strongest) and stabilizing once threats recede—a cyclical pattern where the constraint persists beyond epidemiological necessity.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and immunologically vulnerable populations see the constraint as legitimate coordination: a necessary harm-prevention mechanism. Vaccine hesitants and refusers see it as illegitimate extraction: coercion without consent, justified by shifting risk assessments. Bodily autonomy advocates reject the proportionality frame itself—they see no level of disease severity that justifies medical mandate. The engine should compute different types from these positions: from the vulnerable-population seat, it is genuine rope (coordination benefit outweighs autonomy cost); from the refuser seat, it is tangled_rope or snare (coordination function is decoupled from the extraction—herd immunity could be achieved through incentives or education, and coercion appears purely extractive). From the bodily-autonomy seat, it is categorical snare (autonomy extraction is the entire point, and disease severity is irrelevant). The proportionality reading claims tangled_rope, acknowledging both coordination (herd immunity for vulnerable populations) and asymmetric extraction (burden on refusers); the threshold mechanism is meant to limit extraction to cases where benefit is high.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (vulnerable populations, health authorities) have low directionality (d near 0) because the constraint delivers benefit without burden. Payers (refusers, hesitants) have high directionality (d near 1) because they bear autonomy costs. For high-severity pathogens (measles), the beneficiary case is strong—herd immunity prevents exponential spread and protects those who cannot vaccinate. For low-severity pathogens (flu), the beneficiary case is weak—herd immunity thresholds are lower, individual risk is low, and coercion produces little coordination benefit. The proportionality reading embeds this directionality asymmetry: it acknowledges that refusers bear real extraction costs, but claims those costs are justified by proportional collective benefit. Directionality overrides are not needed here because the structural derivation (beneficiary/victim + exit options) maps correctly: vulnerable populations have zero exit (trapped) and genuine benefit (low d); refusers have constrained exit and pure cost-bearing (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification by maintaining a live founding problem: measles elimination requires sustained coercion to maintain herd immunity thresholds. The founding problem is NOT obsolete for high-severity pathogens. However, there is mandatrophy risk for low-severity pathogens: seasonal flu coercion persists despite low CFR and endemic equilibrium—the founding problem has been solved (voluntary vaccination is available, hospital capacity exists) but the coercion machinery remains, now maintained by institutional inertia. The proportionality reading is meant to prevent this mandatrophy by tying legitimacy to epidemiological fact: if the founding problem dies (disease becomes endemic and low-risk), coercion should be withdrawn. Empirically, the theater_ratio measurement series track this risk: as outbreaks subside and understanding of disease risk grows, the share of enforcement activity devoted to theater (reassuring the public, maintaining institutional appearance) grows relative to functional harm-prevention. The mandatrophy_analysis should note that the proportionality reading is vulnerable to institutional capture: once the enforcement machinery is built, it persists regardless of epidemiological change, and the constraint drifts toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_parameterization_ambiguity,
    'What specific R0 and CFR values constitute the proportionality threshold? Is it R0>10, CFR>0.1%, hospitalization burden >X% of capacity, or some other metric?',
    'Examination of actual policy documents and threshold-setting by public health authorities across jurisdictions; comparison to case law that enforces the proportionality constraint.',
    'If thresholds are ambiguous or variable across jurisdictions, the proportionality reading loses coherence—it becomes a heuristic covering different underlying policies rather than a principled boundary. Different thresholds yield different victim sets and different legitimacy verdicts for the same pathogen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_parameterization_ambiguity, empirical, 'Parameter specification for the proportionality boundary').

omega_variable(
    institutional_drift_toward_low_severity_coercion,
    'Does the enforcement machinery built for high-severity pathogens persist and expand to low-severity pathogens over time?',
    'Historical analysis of coercion policies: did seasonal flu, chickenpox, or other endemic low-severity diseases face increasing mandates after the institutional framework was established for measles/smallpox? Does theater_ratio increase as disease severity falls?',
    'If institutional drift occurs, the proportionality reading devolves into mandatrophy: coercion persists after the founding problem is solved, and the constraint becomes pure extraction masked by epidemiological theater. The reading''s failure would support either bodily_autonomy_primary (reject all coercion) or regulatory reform (tighten the threshold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_drift_toward_low_severity_coercion, empirical, 'Institutional tendency to expand coercion scope beyond the proportionality boundary').

omega_variable(
    reading_incompatibility_with_bodily_autonomy,
    'Can the proportionality reading and the bodily_autonomy_primary reading coexist within a single institutional framework, or does adoption of proportionality foreclose bodily autonomy?',
    'Constitutional and jurisprudential analysis: do courts that adopt the proportionality reading explicitly reject the categorical bodily-autonomy claim, or do they leave room for both? Can an individual refuse mandated vaccination on autonomy grounds while conceding the proportionality principle?',
    'If the readings foreclose each other, the kernel contest is genuinely binary—institutional commitment to proportionality forecloses the bodily-autonomy frame and vice versa. If they coexist, they are alternative frameworks held by different parties rather than logically incompatible positions. This determines whether the relationship is forecloses vs. coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incompatibility_with_bodily_autonomy, conceptual, 'Logical relationship between proportionality and bodily-autonomy readings').

omega_variable(
    epidemiological_contingency_of_legitimacy,
    'Is the proportionality reading''s legitimacy claim truly contingent on epidemiological fact, or is epidemiological assessment itself embedded in the reading''s authority structure and thus subject to motivated reasoning?',
    'Case analysis: when epidemiological assessment changes (e.g., new evidence on CFR, discovery of unexpected transmission routes), does public health authority legitimacy and coercion enforcement change accordingly? Or is the epidemiological assessment treated as fixed by the authority''s declaration?',
    'If assessment is truly contingent, the proportionality reading is transparent and revisable: disease severity claims are empirical questions subject to evidence. If assessment is fixed by authority declaration, the reading becomes a rhetorical cover for political choices—the proportionality mask hides discretionary coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epidemiological_contingency_of_legitimacy, empirical, 'Whether proportionality legitimacy is genuinely contingent on epidemiological fact or fixed by institutional authority').

omega_variable(
    kernel_reading_versus_bodily_autonomy_foreclosure,
    'Does the adoption of the proportionality reading as the binding institutional norm logically foreclose the bodily_autonomy_primary reading, or do the readings remain held simultaneously by different factions?',
    'Institutional sociology: trace which jurisdictions have adopted which reading as binding law; examine whether adopting proportionality legally prevents individuals from making bodily-autonomy-grounded objections, or merely denies those objections force in the coercion decision.',
    'If proportionality forecloses bodily autonomy (within a single legal framework), the relation is forecloses; if both remain live positions held by different parties (even where proportionality is the law), the relation is coexists_with. This affects how the engine models the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_versus_bodily_autonomy_foreclosure, conceptual, 'Logical foreclosure vs. institutional coexistence of kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary__public_health_primary).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel coercion_legitimacy_boundary. Two sibling stories instantiate the bodily_autonomy_primary and public_health_primary readings. All three share a single referent (the state's power to mandate medical intervention) but author different ε values and victim/beneficiary structures, depending on which reading is adopted. The proportionality_reading (this story) claims moderate extraction (0.52) because the constraint genuinely solves coordination for high-severity pathogens but becomes illegitimate for low-severity ones. The bodily_autonomy_primary reading would author high extraction (ε~0.8+) by rejecting the proportionality frame entirely. The public_health_primary reading would author similar or lower extraction (ε~0.45) by accepting coercion for any pathogen with collective-harm potential, dissolving the severity boundary. The three stories are linked by network.affects_constraints to enable cross-kernel comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__proportionality_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
