% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949: Conditional Reciprocity Reading
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Geneva Conventions 1949 establish legal frameworks for the treatment
 *   of combatants and civilians in armed conflict. This constraint
 *   instantiates ONE READING of the contested kernel: the conditional
 *   reciprocity reading. Under this reading, the Conventions function as
 *   reciprocal restraints—states meet their obligation to protect combatants
 *   who comply with organizational criteria (Article 4: organized command,
 *   distinctive insignia, carrying arms openly, conducting operations in
 *   accordance with laws of war). Combatants who fail to meet these criteria
 *   do not receive full protections; instead, they may be classified as
 *   unlawful combatants or detained persons with reduced safeguards.
 *   Non-compliance by irregular forces justifies proportional degradation of
 *   protections for civilians in mixed areas (permitting higher collateral
 *   harm where irregular combatants embed in civilian infrastructure). This
 *   reading moderately constrains state violence—more than security
 *   maximization advocates want, less than humanitarian ceiling advocates
 *   demand. It creates an enforcement apparatus where state actors decide
 *   compliance status, international organizations monitor treatment, and
 *   courts adjudicate whether degradation was proportional.
 *
 * KEY AGENTS:
 *   - state_military_actors: set and enforce the reading through classification decisions, detention policies, interrogation protocols
 *   - organized_combatants_with_compliance: receive full protections if they meet Article 4 criteria
 *   - irregular_combatants: classified as non-compliant, detained with reduced protections, vulnerability determined by state assessment of compliance
 *   - detained_non_compliant_actors: bear the enforcement machinery, face uncertainty about classification and treatment
 *   - civilian_populations: retain nominal immunity but face narrowed protections via proportionality calculations
 *   - international_humanitarian_organizations: produce external documentary record of compliance and treatment
 *   - international_courts_and_accountability: interpret the reading, define sufficiency of non-compliance, constrain proportional degradation
 *   - humanitarian_ceiling_advocates: excluded from enforcement tier, contest the reciprocity reading in parallel discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.71).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949: Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "political/legal").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '92893c24-47cd-48df-87e0-df5ff550e3ce').
narrative_ontology:cs_kernel_codification('92893c24-47cd-48df-87e0-df5ff550e3ce', fixed_text).
narrative_ontology:cs_authority_grounding('92893c24-47cd-48df-87e0-df5ff550e3ce', lineage).
narrative_ontology:cs_interpretation_layer_present('92893c24-47cd-48df-87e0-df5ff550e3ce').
narrative_ontology:cs_reading_relation('92893c24-47cd-48df-87e0-df5ff550e3ce', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('92893c24-47cd-48df-87e0-df5ff550e3ce', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('92893c24-47cd-48df-87e0-df5ff550e3ce', foundational, reciprocity_as_legitimate_interpretive_principle).
narrative_ontology:cs_axiom_status(reciprocity_as_legitimate_interpretive_principle, holdable).
narrative_ontology:cs_axiom_grounding('92893c24-47cd-48df-87e0-df5ff550e3ce', reciprocity_as_legitimate_interpretive_principle, conventional).
narrative_ontology:cs_axiom('92893c24-47cd-48df-87e0-df5ff550e3ce', foundational, proportionality_as_constraint_on_reciprocal_degradation).
narrative_ontology:cs_axiom_status(proportionality_as_constraint_on_reciprocal_degradation, holdable).
narrative_ontology:cs_axiom_grounding('92893c24-47cd-48df-87e0-df5ff550e3ce', proportionality_as_constraint_on_reciprocal_degradation, deontological).
narrative_ontology:cs_reference_frame('92893c24-47cd-48df-87e0-df5ff550e3ce', conventions_as_reciprocal_restraint).
narrative_ontology:cs_drift_state('92893c24-47cd-48df-87e0-df5ff550e3ce', post_2001_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92893c24-47cd-48df-87e0-df5ff550e3ce', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_military_actors).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, organized_combatants_with_compliance).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_non_compliant_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, detained_non_compliant_actors).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, reciprocity_doctrine_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Conventions. Under the conditional reciprocity reading, they determine which adversary forces meet Article 4 criteria (organized command, distinctive insignia, carrying arms openly, conducting operations in accordance with laws of war). They argue non-compliance by irregular forces—operating in civilian dress, embedding in civilian areas, attacking without warning—justifies proportional degradation of protections. They command the interrogation, detention, and treatment apparatus and decide which detained persons receive full POW status, reduced protections, or unlawful combatant classification.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_military_actors, agenda_setter,
    institutional, generational, analytical, global).

% Armed forces of states and some non-state actors (insurgencies, liberation movements) organized with command structure, distinctive insignia, carrying arms openly, observing proportionality. Under the conditional reciprocity reading, they receive full protections: POW status if captured, medical care, humane treatment, communication with families. Their compliance with the criteria is the transaction that unlocks the protection tier.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, organized_combatants_with_compliance, beneficiary,
    institutional, generational, analytical, global).

% Non-uniformed fighters, insurgent networks, militia operating outside formal command structures, operating in civilian clothing or mixed dress, or failing to carry arms openly. Under the conditional reciprocity reading, they are classified as not meeting Article 4 criteria and thus do not receive POW status if captured. They may be detained as unlawful combatants with reduced protections: no prisoner-of-war privileges, eligibility for criminal prosecution, reduced access to legal representation, limited due process guarantees. The reading frames their non-compliance as justifying this treatment. Exit is impossible—the classification is backward-looking, applied after capture.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, biographical, trapped, global).

% Persons in detention whose compliance status is contested or determined to be non-compliant. They bear the enforcement machinery: interrogation under reduced-safeguard protocols, indefinite detention without clear charges, vulnerability to mistreatment justified as necessary for security. Some benefit from basic humanitarian protections (food, shelter, medical care) that exceed treatment of common criminals, but fall short of POW privileges. Their exit is determined by the detaining power's security assessment.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detained_non_compliant_actors, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, detained_non_compliant_actors, beneficiary).

% Populations in conflict zones. Under the conditional reciprocity reading, they retain immunity from attack as civilians, but that immunity is narrowed by proportionality calculations that weigh military advantage against civilian harm. Where irregular combatants embed in civilian areas, operate from civilian-use infrastructure, or use human shields, proportionality permits increased collateral harm than would otherwise be permitted. Civilians benefit from the general structure but pay through narrowed protections when irregular combatants operate from their vicinity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations, payer).

% Red Cross/Red Crescent, Amnesty International, Human Rights Watch, and other monitors and implementers. They investigate compliance with the Conventions and produce testimony about treatment of detainees, application of protections, and contested classifications. Their observations are used by both state actors (to justify classifications) and accountability mechanisms (to scrutinize them). They sit outside the enforcement machinery but produce the primary external documentary record.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_organizations, observer,
    organized, generational, constrained, global).

% International Criminal Court, International Court of Justice, ad-hoc tribunals (ICTY, ICTR), and national courts. They interpret the Conventions, adjudicate compliance, and prosecute violations. Under the conditional reciprocity reading, they recognize reciprocity as a legitimate interpretive principle: state actors can proportionally degrade protections when adversaries are non-compliant. Their role is to define what counts as sufficient non-compliance to justify degradation and to identify when proportional responses become disproportionate violations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_courts_and_accountability, observer,
    institutional, generational, analytical, global).

% International organizations, states, and movements that hold the humanitarian ceiling reading: that the Conventions establish absolute minimums regardless of adversary compliance. They argue the conditional reciprocity reading creates a permission structure for abuse—that classifying persons as non-compliant becomes a mechanism for denying protections without genuine legal foundation. They produce counter-testimony and alternative legal interpretations but operate outside the enforcement machinery that applies the conditional reciprocity reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_ceiling_advocates, excluded,
    organized, generational, constrained, global).

% States conducting asymmetric conflicts against irregular forces. They often invoke the conditional reciprocity reading to justify protections degradation, but sometimes push further toward the security maximization reading (Conventions as peacetime aspirations, suspended for operational necessity). They use the conditional reciprocity reading as a middle ground between the humanitarian ceiling reading (which constrains them most) and their preferred security maximization stance. Their position in this reading is ambiguous: they advocate for it when convenient, but often seek to exceed its bounds.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, security_maximization_states, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__conditional_reciprocity_reading, state_military_actors).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__conditional_reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes mutual expectations for how combatants are to be treated when captured or hors de combat, enabling military operations to proceed with predictable constraints on violence. Creates a tiered protection system where compliance with organizational and procedural criteria (distinctive insignia, open carrying of arms, organized command) maps to protection levels, providing incentives for some armed actors to organize formally and conduct operations in accordance with the laws of war.
% TRANSFER_FUNCTION: Transfers protections (POW status, humane treatment, medical care, communication rights, repatriation after conflict) from captured compliant combatants to the detaining state, which gains internment authority and labor capacity. Transfers non-protections (unlawful combatant status, reduced due-process guarantees, detention authority) from non-compliant captured fighters to the state, enabling interrogation and prosecution without POW-level constraints. Transfers narrowed civilian immunity protections (reduced absolute protection, increased proportionality calculations) to states conducting operations in civilian-mixed areas.
% ABSENT_VOICES: Captured irregular combatants have no voice in the classification system that determines their status—the criteria are authored by states and applied by states to post-hoc judge whether combatants met the organizational standards. Humanitarian organizations can testify about conditions but cannot override classifications. The humanitarian ceiling reading advocates (those arguing for absolute minimums regardless of reciprocity) are structurally excluded from the enforcement tier where the conditional reciprocity reading operates—their alternative interpretation is not represented in the detention decision apparatus.
% DISAPPEARANCE_RATIONALE: If the conditional reciprocity reading were replaced by the humanitarian ceiling reading (absolute protections regardless of compliance), state militaries would face different operational constraints—they could not proportionally degrade protections based on non-compliance and would have to extend full protections or near-full protections to all detained combatants regardless of organization status. If replaced by security maximization reading (Conventions as peacetime aspirations), protections would degrade further. The reading's specific boundary (reciprocity as legitimate degradation trigger, proportionality as the constraint on degradation) directly shapes detention authority, interrogation practices, and prosecution eligibility—removing it shifts the legal architecture of armed conflict.
% FOUNDING_PROBLEM: Early 20th-century armed conflicts and World Wars demonstrated that combatants operating irregularly (without uniforms, without organized command, without insignia) created ambiguity about who counted as a combatant, what protections they deserved, and how to distinguish combatants from civilians. The Geneva Conventions were developed to establish clear criteria for combatant status and to tie protection levels to compliance with organizational and procedural norms, creating incentives for armed actors to organize formally and conduct operations transparently. The conditional reciprocity reading formalizes reciprocity as a principle: states meet the burden of protecting those who meet the criteria; those who do not meet the criteria forgo the reciprocal protections.
% FOUNDING_PROBLEM_CORROBORATION: States applying the conditional reciprocity reading attest the founding problem remains live: irregular forces today operate deliberately outside the organizational criteria to avoid detection and accountability. They argue the reading is necessary to incentivize compliance. International humanitarian organizations, human rights monitors, and advocates for the humanitarian ceiling reading contest this narrative: they attest that the conditional reciprocity reading has become a permission structure for mistreatment and that the founding problem (distinguishing combatants from civilians) is better solved by absolute baselines than by reciprocal degradation. Academic and legal scholarship is divided; jurisprudence from international courts is mixed (some rulings support strong reciprocity logic, others support humanitarian-ceiling-adjacent constraints on degradation). There is no corroboration from outside both readings—the corroboration is the dispute itself.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the reading creates a permission structure for state actors to degrade protections by classifying combatants as non-compliant. The classification decision is made by the detaining power ex post (after capture), with limited external oversight at the moment of classification. The measurement trajectory shows rising extractiveness from 1949 (0.38) to 2024 (0.62), reflecting the growing use of irregular-warfare justifications to expand the unlawful-combatant category and justify interrogation techniques and detention practices that would be prohibited for POWs. Suppression is high (0.71) because the enforcement machinery—the classification decision, interrogation protocols, detention authority—operates with limited transparency and resistance. Theater ratio is moderate (0.48) and rising because the reading invokes legitimate legal concepts (reciprocity, proportionality) but increasingly as cover for practices that prioritize security and interrogation value over humanitarian constraint. Accessibility collapse is substantial (0.68) because once a combatant is classified as non-compliant, alternatives effectively close off—reclassification is rare, appeal mechanisms are limited, and the individual bearing the consequence has minimal voice in the determination. Resistance is high (0.73) and rising because humanitarian organizations, human rights advocates, and humanitarian ceiling advocates actively contest the reading's application and push back against expansive interpretations of non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence between agenda-setter and payer seats is substantial and deliberate in this reading. State military actors experience the conditional reciprocity reading as a legitimate framework for sorting combatants and fitting protections to compliance—they see reciprocity and proportionality as coherent principles that enable both protection and security. Irregular combatants and detained non-compliant actors experience the same structure as ex-post classification machinery that denies them protections without advance notice or appeal, generating profound legal uncertainty and vulnerability. International courts and human rights monitors experience the reading as an ongoing permission structure for mistreatment that requires constant policing and interpretation. This perspectival gap is not a flaw in the reading—it is the reading's structural feature. The reading is designed to permit differential treatment based on compliance, which necessarily produces divergent experiences across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   State military actors occupy the agenda-setter and beneficiary seats (d near 0.0-0.2: they control the classification apparatus, benefit from flexibility in interrogation and detention authority, face minimal personal cost for non-compliance determinations). Organized combatants meeting Article 4 criteria occupy a symmetric or beneficiary seat (d near 0.2-0.4: they receive protections and have clearer status, though they also face combat risk). Irregular combatants and detained non-compliant actors occupy the target seat (d near 0.8-1.0: they bear the enforcement apparatus, face classification at the discretion of detaining powers, have limited recourse to challenge determinations). Civilian populations occupy a mixed seat (d near 0.5-0.6: they benefit from nominal immunity but pay through narrowed protections via proportionality calculations). International courts and accountability bodies occupy an observer seat (d = analytical: they interpret the reading but do not directly experience its enforcement).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—distinguishing combatants from civilians in irregular warfare—is actively contested as to whether it remains live (founding_problem_status = contested). State actors argue it remains live: irregular forces deliberately operate outside formal criteria to avoid accountability and detection. Humanitarian ceiling advocates argue it has been substantially solved and the conditional reciprocity reading has become a permission structure for abuse rather than a response to a live coordination problem. The measurement trajectory supports the contested assessment: extractiveness and theater ratio have both risen substantially (extractiveness 0.38→0.62, theater ratio 0.25→0.48), suggesting the constraint's function has drifted from clarifying combatant status toward authorizing protections degradation. This drift pattern is visible in jurisprudence: early post-WWII tribunals applied the reading more narrowly; post-2001 applications have extended the non-compliance category to cover a broader range of irregular operations and distributed interrogation authority more widely. The rising theater ratio indicates that an increasing fraction of the constraint's activity is maintaining the permission structure (producing classifications, authorizing interrogation protocols) rather than achieving the original coordination function (clarity on combatant status). This is consistent with mandatrophy: the founding problem is contested, and the constraint persists partly by expanding its enforcement machinery rather than solving the original problem more effectively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_vs_absolute_baseline_boundary,
    'Is reciprocity a legitimate principle for interpreting humanitarian law, or does it necessarily degrade into a permission structure for abuse when applied by parties with asymmetric power?',
    'Empirical examination of applied jurisprudence: do international courts and accountability mechanisms enforce reciprocity-based degradation narrowly (proportionality strictly constrained, classifications rarely upheld upon appeal) or broadly (classifications frequently upheld, proportionality calculations favor security interests)? Theoretical resolution via foundational legal scholarship and comparative-tradition analysis of humanitarian law across jurisdictions.',
    'If reciprocity is found to operate consistently narrowly (courts constrain state flexibility significantly), the reading remains viable as moderate constraint. If reciprocity operates broadly (courts defer substantially to state classification and proportionality determinations), the reading has drifted into security-maximization territory and should be reclassified accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_absolute_baseline_boundary, empirical, 'Whether reciprocity principle functions as constraint or permission.').

omega_variable(
    article_4_compliance_determination_ambiguity,
    'What counts as sufficient compliance with Article 4 criteria (organized command, distinctive insignia, carrying arms openly)? Is the threshold administratively clear, or does it permit state actors to reclassify combatants after capture based on post-hoc judgment?',
    'Analysis of jurisprudence from ICTY, ICTR, ICC, and military courts regarding how Article 4 criteria are applied in practice. Examination of pattern of appeals and reversals of unlawful combatant classifications.',
    'If the criteria are applied consistently and narrowly (most armed actors meeting organizational standards receive Article 4 classification), the reading functions as moderate constraint on state authority. If the criteria are applied expansively (state actors retain wide latitude to classify organized forces as non-compliant post-hoc), the reading functions as a mechanism for selective protection denial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_4_compliance_determination_ambiguity, empirical, 'Actual scope of Article 4 criteria in state practice.').

omega_variable(
    proportionality_metric_ambiguity,
    'How is proportionality—the constraint on how far protections may be degraded in response to non-compliance or irregular operations—measured and enforced? Is it subject to objective calibration or discretionary state judgment?',
    'Jurisprudential analysis of how proportionality is assessed in court decisions. Examination of cases where courts found protections degradation to be disproportionate vs. cases where they upheld state determinations. Comparison across legal traditions (International Criminal Law, US military law, European approaches).',
    'If proportionality is subject to objective calibration (courts regularly strike down state claims of proportionality), the reading constrains state extraction meaningfully. If proportionality is highly discretionary (courts defer substantially to state security assessments), the reading functions as ex-post legitimation of state choices rather than meaningful constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_metric_ambiguity, empirical, 'Whether proportionality constraint on degradation is objective or discretionary.').

omega_variable(
    kernel_reading_foreclosure_analysis,
    'Does the conditional reciprocity reading foreclose the humanitarian ceiling reading, or can both readings coexist as live positions held by different state and non-state actors?',
    'Jurisprudential mapping: can a single court apply humanitarian ceiling reasoning while another applies conditional reciprocity? Can a state commit to humanitarian ceiling while accepting that other states may apply conditional reciprocity? Test coexistence hypothesis against the actual structure of international law (no world government to force uniform interpretation; parallel jurisdictions permitted).',
    'If the readings foreclose each other, the container kernel is broken (no stable legal framework). If they coexist, the kernel remains contested but structurally viable. Classification of the reading''s relationship to its siblings and the type of the constraint family depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_analysis, conceptual, 'Whether conditional reciprocity foreclose humanitarian ceiling or coexist as live positions.').

omega_variable(
    irregular_warfare_drift_in_scope,
    'Has the category of ''irregular combatants'' and ''non-compliant forces'' expanded substantially since 1949 to cover a broader range of armed actors (e.g., insurgencies, militias, cyber-warfare actors, drone operators)? If so, does this expansion represent a legitimate application of the conditional reciprocity principle, or a drift toward security maximization?',
    'Historical analysis of jurisprudence and state practice: compare the scope of non-compliant classifications in post-WWII tribunals (1946-1950), Cold War conflicts (1950-1990), post-Cold War interventions (1990-2001), and post-9/11 asymmetric conflicts (2001-2024). Measure the proportion of captured combatants classified as unlawful combatants in each period.',
    'If the expansion reflects genuine changes in warfare (more irregular operations) and is applied consistently to the same criteria, the reading remains stable. If the expansion is decoupled from criteria changes and reflects state preference for reduced-protection detention, the reading has drifted toward security maximization and should be reclassified toward snare or piton (performance maintenance without genuine coordination function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irregular_warfare_drift_in_scope, empirical, 'Scope expansion of irregular combatant category over the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.25).
narrative_ontology:measurement_basis(gene_tr_t1949, projected).
narrative_ontology:measurement(gene_tr_t1975, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement_basis(gene_tr_t1975, observed).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement_basis(gene_tr_t1990, observed).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.43).
narrative_ontology:measurement_basis(gene_tr_t2001, observed).
narrative_ontology:measurement(gene_tr_t2015, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(gene_tr_t2015, observed).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.48).
narrative_ontology:measurement_basis(gene_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.38).
narrative_ontology:measurement_basis(gene_be_t1949, projected).
narrative_ontology:measurement(gene_be_t1975, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement_basis(gene_be_t1975, observed).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement_basis(gene_be_t1990, observed).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement_basis(gene_be_t2001, observed).
narrative_ontology:measurement(gene_be_t2015, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(gene_be_t2015, observed).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(gene_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement_basis(gene_su_t1949, projected).
narrative_ontology:measurement(gene_su_t1975, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement_basis(gene_su_t1975, observed).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement_basis(gene_su_t1990, observed).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement_basis(gene_su_t2001, observed).
narrative_ontology:measurement(gene_su_t2015, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(gene_su_t2015, observed).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(gene_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1949, tn=2024
narrative_ontology:measurement(gene_grid_01, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(class), 1949, 0.38).
narrative_ontology:measurement(gene_grid_02, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(class), 2024, 0.68).
narrative_ontology:measurement(gene_grid_03, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(individual), 1949, 0.61).
narrative_ontology:measurement(gene_grid_04, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(individual), 2024, 0.72).
narrative_ontology:measurement(gene_grid_05, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(organizational), 1949, 0.55).
narrative_ontology:measurement(gene_grid_06, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(organizational), 2024, 0.74).
narrative_ontology:measurement(gene_grid_07, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(structural), 1949, 0.42).
narrative_ontology:measurement(gene_grid_08, geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse(structural), 2024, 0.72).
narrative_ontology:measurement(gene_grid_09, geneva_conventions_1949__conditional_reciprocity_reading, resistance(class), 1949, 0.45).
narrative_ontology:measurement(gene_grid_10, geneva_conventions_1949__conditional_reciprocity_reading, resistance(class), 2024, 0.78).
narrative_ontology:measurement(gene_grid_11, geneva_conventions_1949__conditional_reciprocity_reading, resistance(individual), 1949, 0.42).
narrative_ontology:measurement(gene_grid_12, geneva_conventions_1949__conditional_reciprocity_reading, resistance(individual), 2024, 0.72).
narrative_ontology:measurement(gene_grid_13, geneva_conventions_1949__conditional_reciprocity_reading, resistance(organizational), 1949, 0.62).
narrative_ontology:measurement(gene_grid_14, geneva_conventions_1949__conditional_reciprocity_reading, resistance(organizational), 2024, 0.74).
narrative_ontology:measurement(gene_grid_15, geneva_conventions_1949__conditional_reciprocity_reading, resistance(structural), 1949, 0.55).
narrative_ontology:measurement(gene_grid_16, geneva_conventions_1949__conditional_reciprocity_reading, resistance(structural), 2024, 0.68).
narrative_ontology:measurement(gene_grid_17, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(class), 1949, 0.38).
narrative_ontology:measurement(gene_grid_18, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(class), 2024, 0.58).
narrative_ontology:measurement(gene_grid_19, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(individual), 1949, 0.48).
narrative_ontology:measurement(gene_grid_20, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(individual), 2024, 0.68).
narrative_ontology:measurement(gene_grid_21, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(organizational), 1949, 0.52).
narrative_ontology:measurement(gene_grid_22, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(organizational), 2024, 0.65).
narrative_ontology:measurement(gene_grid_23, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(structural), 1949, 0.45).
narrative_ontology:measurement(gene_grid_24, geneva_conventions_1949__conditional_reciprocity_reading, stakes_inflation(structural), 2024, 0.61).
narrative_ontology:measurement(gene_grid_25, geneva_conventions_1949__conditional_reciprocity_reading, suppression(class), 1949, 0.58).
narrative_ontology:measurement(gene_grid_26, geneva_conventions_1949__conditional_reciprocity_reading, suppression(class), 2024, 0.73).
narrative_ontology:measurement(gene_grid_27, geneva_conventions_1949__conditional_reciprocity_reading, suppression(individual), 1949, 0.62).
narrative_ontology:measurement(gene_grid_28, geneva_conventions_1949__conditional_reciprocity_reading, suppression(individual), 2024, 0.75).
narrative_ontology:measurement(gene_grid_29, geneva_conventions_1949__conditional_reciprocity_reading, suppression(organizational), 1949, 0.52).
narrative_ontology:measurement(gene_grid_30, geneva_conventions_1949__conditional_reciprocity_reading, suppression(organizational), 2024, 0.72).
narrative_ontology:measurement(gene_grid_31, geneva_conventions_1949__conditional_reciprocity_reading, suppression(structural), 1949, 0.48).
narrative_ontology:measurement(gene_grid_32, geneva_conventions_1949__conditional_reciprocity_reading, suppression(structural), 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__conditional_reciprocity_reading, 0.18).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, international_criminal_law__combatant_status_dispute).

% DUAL FORMULATION NOTE:
% The Geneva Conventions 1949 constraint family consists of three readings of a single contested kernel: (1) conditional_reciprocity_reading (this file), which permits reciprocal degradation of protections based on non-compliance; (2) humanitarian_ceiling_reading, which asserts absolute minimums regardless of reciprocity; (3) security_maximization_reading, which treats the Conventions as peacetime aspirations suspended for operational necessity. Each reading instantiates a different ε value and produces different structural beneficiary/victim distributions. They are not the same constraint viewed from different angles—they are genuinely distinct interpretations of the kernel that produce different practical consequences for irregular combatants and detained persons. The conditional_reciprocity_reading affects the humanitarian_ceiling_reading by providing an alternative interpretive framework that competing state and non-state actors adopt; it also affects the security_maximization_reading by providing a legitimation structure that states invoke when pushing the humanitarian ceiling toward suspension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, powerless, 0.88).
constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
