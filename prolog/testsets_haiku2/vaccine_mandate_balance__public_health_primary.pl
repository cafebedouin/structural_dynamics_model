% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Vaccine Mandate Enforced for Herd Immunity (Public Health Primary Reading)
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primary reading of the
 *   vaccine_mandate_balance kernel. The core premise: collective protection
 *   of vulnerable populations supersedes individual consent when voluntary
 *   vaccination fails to achieve herd immunity and lethal exposure risk is
 *   present. This reading foregrounds the structural vulnerability of
 *   immunocompromised, infant, and medically contraindicated populations
 *   whose survival depends entirely on vaccinated populations around them.
 *   Under this reading, the unvaccinated-coerced are not victims — they are
 *   individuals who possess immunity capacity and refuse voluntary
 *   contribution to collective protection; the mandate is necessary
 *   enforcement, not extractive suppression. The beneficiary set enters as a
 *   structural necessity: immunocompromised persons become the constraint's
 *   raison d'être and are named as beneficiaries whose protection justifies
 *   the mandate. Immunocompromised individuals without mandates would face
 *   lethal exposure; they are victims of mandate-absence, not the mandate
 *   itself. The sibling bodily_autonomy_primary reading inverts this: it
 *   foregrounds consent-violators as the primary victims and vulnerable
 *   populations as a secondary consideration (manageable through alternative
 *   protective measures). The proportionality_reading sits between: mandates
 *   permissible but constrained by disease severity, vaccine safety, and
 *   robust exemptions. This story generates ONLY the public_health_primary
 *   reading as a clean, ε-invariant constraint with the structural delta
 *   specified: immunocompromised-exposed in the beneficiary set,
 *   unvaccinated-coerced in the victim set, high extraction from enforcement
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Immunocompromised populations (powerless, trapped): depend on herd immunity for survival
 *   - Infants unvaccinatable (powerless, trapped): too young to vaccinate, lethal risk exposure
 *   - Unvaccinated-coerced (moderate, identity-locked): refuse on grounds of bodily autonomy/ideology, face employment/exclusion penalties
 *   - Public health authority (institutional, agenda-setter): sets/enforces vaccination mandate, calculates herd immunity threshold
 *   - Employers/healthcare institutions (institutional, dual): enforce mandate, bear workforce/liability costs
 *   - Bodily autonomy advocates (excluded): would argue for proportionality and robust exemptions, excluded from mandate-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.68).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.71).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Vaccine Mandate Enforced for Herd Immunity (Public Health Primary Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, 'a4feccd6-fe28-47b9-9f5a-ceecd980f380').
narrative_ontology:cs_kernel_codification('a4feccd6-fe28-47b9-9f5a-ceecd980f380', distributed).
narrative_ontology:cs_authority_grounding('a4feccd6-fe28-47b9-9f5a-ceecd980f380', extraction).
narrative_ontology:cs_interpretation_layer_present('a4feccd6-fe28-47b9-9f5a-ceecd980f380').
narrative_ontology:cs_reading_relation('a4feccd6-fe28-47b9-9f5a-ceecd980f380', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('a4feccd6-fe28-47b9-9f5a-ceecd980f380', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('a4feccd6-fe28-47b9-9f5a-ceecd980f380', foundational, collective_necessity_overrides_consent).
narrative_ontology:cs_axiom_status(collective_necessity_overrides_consent, holdable).
narrative_ontology:cs_axiom_grounding('a4feccd6-fe28-47b9-9f5a-ceecd980f380', collective_necessity_overrides_consent, empirically_contingent).
narrative_ontology:cs_axiom('a4feccd6-fe28-47b9-9f5a-ceecd980f380', foundational, vulnerable_population_lethal_exposure_compels_duty).
narrative_ontology:cs_axiom_status(vulnerable_population_lethal_exposure_compels_duty, holdable).
narrative_ontology:cs_axiom_grounding('a4feccd6-fe28-47b9-9f5a-ceecd980f380', vulnerable_population_lethal_exposure_compels_duty, deontological).
narrative_ontology:cs_reference_frame('a4feccd6-fe28-47b9-9f5a-ceecd980f380', collective_public_health_necessity).
narrative_ontology:cs_drift_state('a4feccd6-fe28-47b9-9f5a-ceecd980f380', covid_19_mandate_resistance_escalation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a4feccd6-fe28-47b9-9f5a-ceecd980f380', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, infants_unvaccinatable).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vaccine_allergic_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, medical_exemption_denied).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, patients_in_medical_crisis).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, employers_healthcare_institutions).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_populations).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, collective_protection_duty).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__public_health_primary, necessity_doctrine_public_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons with severe immunosuppression (solid organ/stem cell transplant recipients, advanced HIV disease, prolonged neutropenia, severe combined immunodeficiency) who cannot mount adequate immune response to vaccination and depend entirely on vaccinated populations for protection. They face lethal risk from vaccine-preventable diseases if herd immunity drops. Vaccination of surrounding populations is their only viable protection mechanism; mandates exist structurally for their benefit.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, immediate, trapped, national).

% Infants too young to receive certain vaccines (pertussis vaccine not approved before 2 months; measles vaccine not approved before 12 months) face lethal risk from preventable diseases during their first year of life. They cannot consent or refuse vaccination. Their protection depends entirely on vaccinated populations around them; they are passive beneficiaries of herd immunity created by mandates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, infants_unvaccinatable, beneficiary,
    powerless, immediate, trapped, national).

% Individuals with documented anaphylaxis to vaccine components (eggs, gelatin, polysorbate 80, etc.) cannot safely receive specific vaccines despite being otherwise immunocompetent. They medically cannot participate in vaccination and depend on surrounding vaccinated populations to suppress transmission of diseases they cannot vaccinate against.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_allergic_individuals, beneficiary,
    powerless, immediate, trapped, national).

% Individuals who refuse vaccination on personal, religious, or ideological grounds and face coercive pressure (employment termination, school exclusion, healthcare access denial, legal penalty, social stigma) to comply. Their refusal often reflects identity commitments (religious belief, anti-state ideology, naturalism, bodily autonomy principle) that make exit appear identity-incompatible even where legal exit is nominally available. They experience the mandate as an imposition on bodily autonomy and consent, and bear the direct cost of noncompliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_coerced, payer,
    moderate, biographical, identity_locked, national).

% Individuals who seek medical exemptions claiming vaccine contraindications but are denied by public health authorities who judge the medical grounds insufficient or not credible. They are forced to accept vaccination they claim presents medical risk. Their exit is constrained: they can litigate or relocate, but cannot easily exit the jurisdiction without substantial cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, medical_exemption_denied, payer,
    moderate, biographical, constrained, national).

% State and federal health departments that set vaccination thresholds, determine medical necessity for exemptions, enforce compliance through institutional rules, and adjudicate mandate scope. They justify mandates by the herd immunity threshold for the specific disease and the severity of exposure risk to vulnerable populations. They bear the political cost of mandate opposition and the administrative burden of compliance monitoring and exemption review.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Private employers, hospitals, clinics, care facilities, and schools that implement mandatory vaccination policies for employees, staff, visitors, and students. They execute enforcement (termination, exclusion, credential suspension). They benefit from reduced transmission in their facilities and reduced nosocomial disease burden. They pay costs in workforce disruption, litigation, and lost enrollment. Their dual role reflects that they both administer enforcement and bear a share of the extraction costs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, employers_healthcare_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, employers_healthcare_institutions, payer).

% Persons in acute medical crisis (receiving chemotherapy, in ICU care, dialysis, neonatal intensive care) who are immunocompromised by their underlying illness and treatment. They depend entirely on vaccinated staff and visitors to prevent nosocomial infection during their most vulnerable window. Mandatory healthcare worker vaccination directly protects them from preventable disease acquisition during crisis care.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, patients_in_medical_crisis, beneficiary,
    powerless, immediate, trapped, local).

% Organizations and coalitions that oppose vaccine mandates on grounds of bodily autonomy, informed consent, religious freedom, or constitutional limits on state power. They are formally excluded from mandate-setting authority in most jurisdictions. They would argue that collective benefit cannot override individual consent and that proportionality thresholds and robust exemptions must be maintained. Their exclusion means the mandate reflects only the public health authority's risk calculus, not a deliberative consensus.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, bodily_autonomy_advocates, excluded,
    organized, generational, constrained, national).

% Persons who have vaccine confidence deficits due to misinformation, historical medical racism, cultural barriers, language access, or prior adverse events or religious beliefs. They would accept vaccination with better information access or cultural accommodation but resist mandates as top-down coercion that increases their resistance rather than overcoming it. They bear the coercive cost (employment risk, exclusion threat) without the mechanism producing compliance, reducing mandate effectiveness and increasing resentment.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_hesitant_populations, payer,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves and maintains herd immunity thresholds (typically 85-95% depending on disease) needed to interrupt disease transmission chains and protect populations biologically unable to vaccinate themselves. Solves the collective action problem that voluntary vaccination leaves unresolved (free-rider incentive, high-risk-tolerance individuals decline despite collective benefit). Coordinates protection of vulnerable populations against lethal exposure.
% TRANSFER_FUNCTION: Transfers bodily autonomy and medical decision-making authority from individuals to public health institutions in circumstances where individual vaccination choices aggregate to inadequate herd immunity. Transfers risk of mandate noncompliance (employment loss, school exclusion, exclusion from healthcare settings, legal penalty) from vulnerable populations (who depend on vaccination) to unvaccinated individuals (who bear enforcement cost).
% ABSENT_VOICES: Bodily autonomy advocates, vaccine hesitant populations with legitimate barriers (language, prior adverse events, cultural beliefs), and individuals whose medical exemption claims are denied are structurally excluded from mandate-setting authority. They would argue for proportionality thresholds, robust exemption processes with genuine expert review, and consent-respecting alternatives. Their exclusion means the mandate reflects only the public health authority's risk assessment and epidemiological model, not a deliberative consensus that includes consent-focused perspectives.
% DISAPPEARANCE_RATIONALE: If vaccine mandates disappeared and vaccination returned to purely voluntary participation, disease transmission would increase in the unvaccinated population. Herd immunity thresholds would drop below critical levels for highly transmissible diseases (measles >95% immunity, pertussis >90%). Vulnerable populations (immunocompromised, infants, medically contraindicated) would face elevated lethal risk from preventable diseases. Healthcare systems would experience increased disease burden. Outbreaks would cluster in low-vaccination communities and in proximity to vulnerable individuals. The epidemiological equilibrium would shift toward pre-mandate transmission rates, exposing dependent populations to present danger they are currently protected from.
% FOUNDING_PROBLEM: Vaccine-preventable diseases threaten lethal and permanently disabling outcomes for populations unable to vaccinate (immunocompromised, infants, medically contraindicated). Voluntary vaccination often fails to achieve herd immunity thresholds because individuals underweight collective benefit relative to personal inconvenience and perceived risk. Diseases have high transmission rates; vaccination hesitancy, religious refusal, and cost barriers reduce coverage below thresholds in many communities. Vulnerable populations face escalating lethal risk as vaccination coverage drops, creating a public health emergency.
% FOUNDING_PROBLEM_CORROBORATION: Public epidemiological data, serological surveys, and disease surveillance systems confirm that vaccine-preventable disease transmission increases when vaccination coverage drops. Outbreaks of measles, pertussis, and mumps in low-vaccination communities are documented by CDC, state health departments, and international health organizations (WHO, European Centre for Disease Prevention and Control). Immunocompromised patients and pediatric ICU networks report increased preventable disease cases in their facilities when community vaccination coverage drops. Independent academic epidemiologists at university medical centers and independent public health research institutions (not funded by vaccine manufacturers) confirm the foundational problem remains urgent and unresolved by voluntary mechanisms.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the mandate transfers bodily autonomy authority to the state and imposes coercive cost (employment loss, exclusion) on individuals who refuse. The extraction is justified under this reading by the necessity doctrine (protecting lethal vulnerability is a compelling interest that permits state override of consent), but extraction remains present as a structural fact. Early in the interval (t=0), extractiveness is lower (0.55) because mandate compliance is high and coercive enforcement is proportional; as time advances and resistance hardens, compliance costs rise and more enforcement machinery activates, driving extractiveness upward to 0.68. Suppression is 0.71 because the constraint depends on actively preventing alternative vaccination pathways (home schooling exemptions, employment carve-outs, medical exemption leniency) and suppressing organized opposition. Theater ratio is moderate (0.28): the security/disease-prevention function is genuine, but a share of enforcement activity is devoted to maintaining mandate coverage and delegitimizing resistance claims — some enforcement activity is performance of state authority, not epidemiologically necessary. Theater rises early (t=0–8) as authorities mount communication campaigns, then plateaus once mandatory mechanisms are in place. The measurement series share a single time grid so every metric is authored at every examined point. Resistance measurement is not included in the interval series, but accessibility_collapse and resistance are present in base_properties to capture that alternatives remain nominally available (people can theoretically relocate, litigate, refuse and accept penalties) but are heavily collapsed for most people.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence between the public health authority and the unvaccinated-coerced is structural and deep. From the authority's position, the mandate is justified collective protection — a necessity doctrine override of consent when voluntary compliance fails and lethal vulnerability persists. The authority computes a beneficiary structure (immunocompromised, infants) and a necessity threshold (herd immunity %). From the unvaccinated-coerced seat, the same structure operates as state overreach — forced medical intervention on individuals who possess immunity capacity and are exercising choice. The bodily_autonomy_primary reading would classify this as a snare (pure extraction enforced by coercion, alternatives suppressed, victims named). This reading classifies it as tangled_rope: genuine coordination function (herd immunity protection of vulnerable populations is real; voluntary vaccination fails to achieve it) AND asymmetric extraction (unvaccinated bear coercive costs while beneficiaries collect protection without paying the enforcement cost). The engine computes per-seat classification from this structural data: the beneficiary seat sees rope-like coordination (protected); the victim seat sees snare-like extraction (coerced); the authority seat computes the necessity override. The divergence is what the measurement apparatus exists to flag.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is assigned by beneficiary/victim + exit options. Immunocompromised populations are beneficiaries with trapped exit (they cannot leave the jurisdiction and remain protected elsewhere; they are entirely dependent on mandated vaccination). Their directionality is d ≈ 0.0 (full beneficiary: the constraint subsumes their life-and-death dependency). Unvaccinated-coerced are victims with identity-locked exit (they could nominally relocate but refusal is often fused with religious/ideological identity making it identity-locked; they experience substantial coercive cost). Their directionality is d ≈ 0.95 (near-full target: the constraint extracts bodily autonomy and imposes penalty). The public health authority has institutional power and analytical exit (can revise thresholds, change enforcement, model alternatives); their directionality relative to the mandate they set is d ≈ 0.2 (slight beneficiary: they administer the constraint and derive legitimacy from it, but do not capture rents or personal benefit). Employers enforcing mandates have institutional power and constrained exit (they face legal liability either way; they enforce because mandates shift liability to the state); their directionality is d ≈ 0.5 (symmetric: they benefit from reduced transmission in their facilities but pay workforce costs). These directionalities feed the engine's effective extraction calculation: high d for unvaccinated-coerced amplifies effective extraction in their direction; low d for immunocompromised dampens (inverts to subsidy). The constraint extracts ASYMMETRICALLY: it protects beneficiaries while imposing costs on targets, mediated by institutional enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT exhibit mandatrophy as defined (a constraint where the founding problem is dead but the arrangement persists by institutional inertia). The founding problem — lethal risk to vulnerable populations when vaccination is voluntary — remains LIVE under this reading. Immunocompromised individuals continue to face present danger when vaccination coverage drops; measles outbreaks continue to occur in low-vaccination communities; infants continue to be born and to require herd immunity protection. The founding problem_status is authoritatively set to 'live' because independent epidemiological data, not merely authority assertion, confirms ongoing disease transmission risk and vulnerable population exposure. However, mandatrophy in a sibling reading is possible: under the bodily_autonomy_primary reading, the founding problem might be judged DEAD (vaccines are safe, transmission risk is manageable through alternative protective measures, vulnerable populations can be protected through targeted vaccination instead of universal mandates). In that reading, a persistent mandate would exhibit mandatrophy: the justification is gone but the structure persists. This reading's classification as tangled_rope avoids mandatrophy by anchoring the founding problem status to live epidemiological danger, not to authority convenience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herd_immunity_threshold_accuracy,
    'Is the epidemiological herd immunity threshold for the target disease accurate, and are current vaccination coverage levels actually below that threshold in the relevant population?',
    'Empirical measurement: serology surveys confirming immunity prevalence; transmission modeling against observed disease incidence; prospective tracking of disease outbreaks in vaccinated vs. unvaccinated cohorts.',
    'If the threshold is overestimated or coverage is actually above the threshold, the mandate''s necessity justification collapses and the extraction classification shifts toward snare (coercion without proportional public health benefit). If the threshold is accurate and coverage is below it, the necessity doctrine holds and the tangled_rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_threshold_accuracy, empirical, 'Whether the herd immunity threshold justifying the mandate is epidemiologically accurate.').

omega_variable(
    alternative_protection_feasibility,
    'Could vulnerable populations (immunocompromised, infants, medically contraindicated) be effectively protected through targeted vaccination strategies, targeted prophylaxis, or environmental controls WITHOUT universal mandates?',
    'Natural experiments from jurisdictions using non-mandate strategies; cost-benefit analysis of targeted vs. universal approaches; modeling of disease exposure in immunocompromised populations under different protection regimes.',
    'If effective alternative protection exists, mandates appear more extractive relative to necessity and approach snare classification; if alternatives are ineffective or prohibitively costly, mandates appear more necessary and the tangled_rope classification strengthens. This omega locates the boundary between necessity and overreach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_protection_feasibility, conceptual, 'Whether universal mandates are the only effective means to protect vulnerable populations or whether targeted alternatives are available.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the measured suppression of vaccine refusal primarily structural (legal penalties, employment loss, institutional barriers) or internalized (identity fusion with refusal, self-imposed isolation from counter-evidence, belief in government ill intent)?',
    'Post-mandate suppression trajectory: compare compliance/resistance patterns in jurisdictions where mandates end; track post-mandate vaccination rates to assess whether suppression persists after structural mechanisms are removed; survey refusers on identity vs. information vs. barrier bases for refusal.',
    'If suppression is primarily structural, it is external coercion that ends when enforcement ends. If internalized, refusers carry the suppression with them even after exit from the mandate — the constraint''s effective suppressive force extends into their post-exit choices, suggesting deeper lock-in than the structural measure alone indicates. This affects the identity_locked classification and the exit_options assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Whether suppression of vaccine refusal is structural enforcement or internalized belief.').

omega_variable(
    necessity_doctrine_boundary_contestation,
    'Where is the legitimate boundary between collective necessity and individual consent override? Does the location of this boundary differ between this reading (public_health_primary) and the bodily_autonomy_primary reading in a way that reflects a factual disagreement or merely a value disagreement?',
    'Conceptual analysis: examine whether the readings would change their boundary if empirical facts changed (disease severity, vaccine adverse event rates, transmission probability) — if they would, the disagreement is partly empirical and resolvable by evidence; if they would not, the disagreement is purely axiological and unresolvable by evidence alone.',
    'If the boundary is partly empirical (readings would shift with evidence), then discovering the accurate facts about disease severity or vaccine safety could resolve the reading contest. If the boundary is purely axiological (readings hold firm regardless of facts), then consensus requires value resolution, not factual discovery. This omega flags whether the kernel contest is empirically tractable or value-theoretic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_doctrine_boundary_contestation, conceptual, 'Whether the necessity doctrine boundary between readings is empirically resolvable or axiologically contested.').

omega_variable(
    alternative_reading_natural_law_claim,
    'Does the bodily_autonomy_primary reading ground its core premise in natural law or human rights (inviolable, independent of consequences) or in a consequentialist claim that consent violations create worse outcomes than the diseases they prevent?',
    'Textual and genealogical analysis of autonomy reading justifications; assess whether proponents hold to the claim regardless of empirical evidence that mandates reduce disease burden or whether they conditionally accept mandates if consequences are severe enough.',
    'If bodily_autonomy_primary grounds in natural law, the two readings foreclose each other — one cannot simultaneously hold that consent is inviolable and that collective necessity overrides it. If bodily_autonomy_primary grounds in consequentialism (consent-violation causes worse outcomes), the readings coexist and can shift based on empirical outcomes. This affects whether the reading_relation is forecloses or coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_natural_law_claim, conceptual, 'Whether bodily autonomy is grounded in natural law or in a conditional consequentialist claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__public_health_primary, theater_ratio, 4, 0.21).
narrative_ontology:measurement_basis(vacc_tr_t4, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__public_health_primary, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__public_health_primary, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t16, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__public_health_primary, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__public_health_primary, base_extractiveness, 4, 0.6).
narrative_ontology:measurement_basis(vacc_be_t4, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__public_health_primary, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__public_health_primary, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(vacc_be_t16, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__public_health_primary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(vacc_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__public_health_primary, suppression_requirement, 4, 0.63).
narrative_ontology:measurement_basis(vacc_su_t4, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__public_health_primary, suppression_requirement, 8, 0.67).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__public_health_primary, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(vacc_su_t16, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__public_health_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(vacc_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_balance__public_health_primary, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_balance kernel is contested across three readings: public_health_primary (this constraint), bodily_autonomy_primary, and proportionality_reading. Each reading instantiates a different constraint with a different beneficiary/victim structure and a different ε. public_health_primary foregrounds vulnerable populations' lethal exposure risk and places immunocompromised persons as beneficiaries; bodily_autonomy_primary foregrounds consent violation and places unvaccinated-coerced as primary victims; proportionality_reading accepts mandates but constrains them through disease severity, vaccine safety, and exemption robustness gates. The readings are linked via network.affects_constraints to signal family relationship. Each reading must author its own ε (0.68 for public_health_primary; higher for bodily_autonomy_primary which sees the constraint as pure extraction; conditional for proportionality_reading which applies mandates only under strict conditions). This story should be read alongside the sibling reading stories, not as a complete account of the mandate phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
