% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Bodily Autonomy Primacy Reading of Vaccine Mandate Legitimacy
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested
 *   vaccine-mandate-legitimacy kernel: the position that bodily
 *   self-sovereignty is a near-absolute constitutional floor and that state
 *   coercion to vaccinate is categorically impermissible regardless of the
 *   public health outcome at stake. Under this reading, courts and
 *   legislatures that adopt the autonomy-primacy framing narrow mandate
 *   authority and expand exemption categories, which shifts exposure risk
 *   onto those who cannot vaccinate themselves (the immunocompromised,
 *   medically fragile elderly, and pre-vaccination-age infants) while liberty
 *   advocacy movements and vaccine-hesitant or exemption-seeking individuals
 *   retain unconstrained institutional participation. This is a genuinely
 *   distinct constraint from the sibling readings
 *   (public_health_primacy_reading, risk_stratification_reading) — its
 *   extractiveness, beneficiary set, and victim set do not overlap cleanly
 *   with either sibling, which is why each is authored as its own story per
 *   the ε-invariance principle rather than as one story parameterized by
 *   observable choice.
 *
 * KEY AGENTS:
 *   - liberty_advocacy_movements: agenda-setting beneficiary — organized/mobile — drives doctrinal expansion
 *   - vaccine_hesitant_individuals: beneficiary — moderate/mobile — retains participation without vaccination
 *   - immunocompromised_individuals: primary victim — powerless/trapped — bears elevated exposure with no exit
 *   - medically_vulnerable_elderly: secondary victim — powerless/trapped — depends on community coverage it cannot control
 *   - state_public_health_agencies: institutional payer — loses enforcement leverage
 *   - constitutional_courts: analytical observer — adjudicates and sets the controlling precedent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.42).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.31).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Bodily Autonomy Primacy Reading of Vaccine Mandate Legitimacy").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '2bc5e19f-20ce-45cd-ab49-0a8ee6c94138').
narrative_ontology:cs_kernel_codification('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', distributed).
narrative_ontology:cs_authority_grounding('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', distributed).
narrative_ontology:cs_reading_relation('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', foundational, bodily_integrity_categorically_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', bodily_integrity_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', foundational, outcome_consequences_cannot_justify_medical_coercion).
narrative_ontology:cs_axiom_status(outcome_consequences_cannot_justify_medical_coercion, holdable).
narrative_ontology:cs_axiom_grounding('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', outcome_consequences_cannot_justify_medical_coercion, deontological).
narrative_ontology:cs_reference_frame('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', jacobson_era_deferential_public_health_authority).
narrative_ontology:cs_drift_state('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', post_covid19_mandate_litigation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2bc5e19f-20ce-45cd-ab49-0a8ee6c94138', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, religious_and_philosophical_exemption_claimants).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_vulnerable_elderly).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_infants_too_young_for_vaccination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_agencies).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, substantive_due_process_over_bodily_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigate against mandates, lobby legislatures for exemption expansion, and organize public opposition campaigns. They set the doctrinal agenda by advancing the bodily-autonomy-absolute framing in courts and statehouses, and gain political capital, fundraising, and legal precedent every time an exemption or injunction is won.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary).

% Retain the right to decline vaccination without losing employment, school enrollment, or public accommodation access wherever this reading prevails. Their exit options are genuinely open — they can decline, relocate to permissive jurisdictions, or claim exemptions — because the constraint they benefit from is precisely the absence of coercive mandate enforcement against them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_individuals, beneficiary,
    moderate, biographical, mobile, regional).

% Use broadened exemption categories created by this reading's doctrinal wins to opt out of mandates while retaining full institutional participation (schooling, employment, travel). They benefit directly from every exemption precedent the bodily-autonomy framing establishes.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, religious_and_philosophical_exemption_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Cannot be vaccinated themselves or mount adequate immune response even if vaccinated, and depend entirely on herd immunity maintained by others' vaccination status. When this reading prevails and mandates are struck down or exemptions widened, their surrounding population's vaccination rate falls and their personal exposure risk rises with no exit available — they cannot relocate away from every unvaccinated contact, and isolation itself is a severe cost imposed on them, not chosen by them.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Live in congregate settings (care facilities, senior housing) or rely on home health aides and family visits where community vaccination coverage determines their exposure risk. They have no meaningful voice in the mandate-versus-autonomy legal contest and no capacity to individually negotiate the vaccination status of those they must interact with to receive care.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_vulnerable_elderly, payer,
    powerless, immediate, trapped, local).

% Below the age threshold for vaccination against diseases like pertussis or measles, and protected only by cocooning — the vaccination status of parents, siblings, and caregivers. Listed for narrative completeness as a non-agent bearer of risk; they cannot advocate, exit, or consent, and the bodily-autonomy-primacy reading's expansion of adult exemption directly elevates their exposure without their having any standing in the dispute.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_infants_too_young_for_vaccination, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_infants_too_young_for_vaccination).

% Lose enforcement authority and coverage-rate leverage whenever this reading prevails in court or legislature, and must manage outbreak response with fewer tools. Their epidemiological arguments are treated as one input among several in the constitutional balancing test this reading applies, rather than as dispositive — their institutional voice is present but structurally subordinated to the autonomy framing.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_agencies, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_agencies, excluded).

% Would prefer uniform vaccination policy to manage liability and workplace/classroom safety but must accommodate the exemption architecture this reading establishes. They are not parties to the underlying constitutional contest and absorb the operational complexity of case-by-case exemption administration without a seat at the doctrinal table.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, employers_and_schools, excluded,
    moderate, biographical, constrained, regional).

% Adjudicate the doctrinal contest between bodily autonomy and public health authority, applying substantive due process and compelling-interest tests. They generate the precedent record that determines which reading of the kernel controls in a given jurisdiction and era.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, generalizable legal principle — bodily self-sovereignty as a near-absolute constitutional floor — so that individuals and courts do not have to relitigate the boundary of state medical coercion from first principles in every new disease outbreak or vaccine controversy.
% TRANSFER_FUNCTION: Moves exposure risk from the vaccine-declining population (who retain full social and institutional participation without vaccination) to the medically vulnerable population (who cannot vaccinate and depend on herd immunity), and moves enforcement burden away from state public health agencies onto ad hoc, case-by-case accommodation by employers and schools.
% ABSENT_VOICES: Immunocompromised individuals, medically vulnerable elderly, and unvaccinated infants have no direct voice in the constitutional litigation that sets this reading's scope — their interests are represented, if at all, secondhand through state public health agencies' compelling-interest arguments, which this reading's doctrinal framework structurally subordinates to individual autonomy claims.
% DISAPPEARANCE_RATIONALE: If this reading's legal primacy disappeared overnight, courts would revert to rational-basis or Jacobson-style deference toward public health mandates, exemption categories would narrow sharply, liberty advocacy organizations would lose their primary litigation lever, and vaccine-hesitant individuals and exemption claimants would face renewed compliance requirements in employment, schooling, and public accommodation.
% FOUNDING_PROBLEM: Historical instances of state medical coercion — forced sterilization, non-consensual experimentation, coercive quarantine abuses — established that unchecked state power over bodily integrity produces severe, sometimes irreversible harm to individuals, and the founding problem was to build a durable constitutional check against that category of abuse.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists and legal historians outside the liberty advocacy movement corroborate that the founding problem (unchecked coercive medical authority) was genuinely severe and historically real — citing Buck v. Bell, Tuskegee, and forced institutionalization records. However, epidemiologists and public health law scholars, also outside the beneficiary set, attest that the founding problem in its original form (unbounded, non-consensual, irreversible state medical intervention) is structurally distinct from time-limited, exemption-permitting vaccination mandates during communicable disease outbreaks, and that this reading's categorical extension from the former to the latter is a contested doctrinal move rather than a settled historical vindication.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).
:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42, rising over the interval as successive court wins entrench the autonomy-primacy doctrine and widen exemption categories, layering more risk transfer onto the vulnerable population each cycle. Suppression is comparatively low (0.31) because this reading's operative mechanism is the WITHDRAWAL of coercive mandate machinery, not its imposition — the reading actively reduces state suppression of vaccine-decliners while indirectly increasing structural risk exposure for the vulnerable, who experience no direct coercive suppression but a rising background hazard they cannot resist through legal or economic means. Resistance is high (0.78) because state public health agencies, disease-modeling epidemiologists, and vulnerable-population advocates actively contest this reading in litigation, legislative testimony, and public health messaging — it is far from an uncontested settlement. Theater ratio is modest and rising slowly (0.28 by interval end) reflecting some performative invocation of 'bodily autonomy' rhetoric in contexts (e.g., routine occupational vaccination requirements with negligible individual risk) where the underlying autonomy stakes are lower than the rhetoric suggests, without dominating the doctrine's substantive legal function.
 *
 * PERSPECTIVAL GAP:
 *   From the liberty advocacy and vaccine-hesitant seats, this reading is coordination — establishing a stable, principled boundary against future state overreach into bodily integrity that they and future generations rely on. From the immunocompromised and medically vulnerable seats, the identical doctrinal structure operates as an enforced transfer of risk: the same court rulings that free others from mandate compliance strip away the herd-immunity protection those seats depend on for survival, with no compensating mechanism and no meaningful voice in the adjudication. The engine's per-seat computation should register this asymmetry directly from the beneficiary/victim/exit declarations, not from any narrative framing choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberty advocacy movements and exemption claimants are coded as low-d beneficiaries: they gain expanded exemption categories, mobile exit options (they can relocate, claim exemptions, or simply decline without institutional exclusion), and organized power to keep advancing the doctrine. Immunocompromised individuals and medically vulnerable elderly are coded as high-d targets: trapped exit options (they cannot relocate away from every unvaccinated contact, cannot vaccinate themselves in many cases, and bear the accumulating externality with zero negotiating leverage). State public health agencies sit as an institutional payer whose loss is authority and enforcement capacity rather than direct physical risk — a different kind of cost than the vulnerable population bears, which is why they are coded payer/excluded rather than payer/victim in the base_properties sense.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is NOT a resolved mandatrophy — the founding problem (unchecked coercive state medical power) was real and remains partially live (documented historical abuses), which is why founding_problem_status is authored as contested rather than dead. The classification as tangled_rope rather than pure snare or pure rope reflects that a genuine coordination function exists (a durable check against state medical coercion protects real values, including for the vulnerable population in other coercion contexts) alongside genuine, asymmetric extraction (the specific application to communicable-disease vaccination during outbreaks concentrates costs on people with no voice in the doctrinal contest). Collapsing this into pure snare would erase the real historical harms the doctrine guards against; collapsing it into pure rope would erase the documented risk transfer onto the medically vulnerable. The tangled_rope classification, with required beneficiaries, victims, and active enforcement (courts actively striking down mandates and enforcing exemption rights), holds both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_balancing_framework_choice,
    'Is bodily self-sovereignty properly treated as a categorical (deontological) constraint that admits no balancing against outcomes, or is it one weighty interest among several to be balanced against collective harm prevention (as the sibling readings hold)?',
    'This is not empirically resolvable — it is a foundational jurisprudential and moral-philosophical choice between deontological and consequentialist/proportionality frameworks for constitutional rights adjudication. Different judicial traditions and eras have resolved it differently (compare Jacobson v. Massachusetts''s deference era to more recent substantive due process expansions).',
    'If courts durably adopt the categorical framing, this reading''s doctrinal dominance is self-reinforcing and the sibling readings become minority positions; if balancing frameworks prevail, this reading''s practical scope narrows to edge cases and risk_stratification_reading or public_health_primacy_reading gain controlling weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_balancing_framework_choice, conceptual, 'Whether autonomy is categorical or balanceable is the root disagreement generating all three kernel readings.').

omega_variable(
    empirical_herd_immunity_threshold_sensitivity,
    'How sensitive is actual harm to the medically vulnerable to the marginal reduction in vaccination coverage this reading''s exemption expansions produce, for a given disease and population density?',
    'Epidemiological modeling and observed outbreak data in jurisdictions that have adopted broad exemption regimes versus those that maintain strict mandates, controlling for baseline vaccination rates and population mixing patterns.',
    'If sensitivity is high (small coverage drops produce large outbreak risk increases for vulnerable populations), the extractiveness and victim-harm severity authored here are likely understated; if sensitivity is low (herd immunity is robust to marginal exemption growth), the extraction claim weakens substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_herd_immunity_threshold_sensitivity, empirical, 'The magnitude of harm to immunocompromised/vulnerable populations depends on empirically contestable epidemiological thresholds.').

omega_variable(
    sibling_reading_disagreement_location,
    'Where exactly do the three kernel readings disagree — is it about the FACTS (does mandate non-compliance actually impose a meaningful externality?), the VALUES (does bodily autonomy outweigh collective harm prevention even if the externality is real?), or the INSTITUTIONAL QUESTION (who should decide — courts via categorical rule, legislatures via case-by-case public health authority, or agencies via risk-stratified criteria)?',
    'Structural analysis of judicial opinions and legislative debates across all three readings to isolate whether disputants share factual premises about externality and diverge only on the moral/institutional question, or whether factual disagreement about disease transmission and vaccine efficacy is doing hidden work in the values dispute.',
    'If the disagreement is purely institutional/values-based, the three readings are genuinely coexisting normative positions (as coded in reading_relations); if factual disagreement is doing hidden work, resolving the epidemiological facts could collapse the practical distance between readings even without resolving the values question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_location, conceptual, 'Locating whether the kernel''s readings disagree on facts, values, or institutional allocation of decision authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.27).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 24, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'vaccine mandate legitimacy' kernel per the ε-invariance principle. bodily_autonomy_primacy_reading (this story) treats state coercion as categorically impermissible and places the immunocompromised/medically vulnerable in the victim set with liberty advocacy movements as beneficiary. public_health_primacy_reading inverts much of this structure, treating unvaccinated status as an externality justifying mandate authority — its beneficiary/victim sets differ substantially from this story's. risk_stratification_reading occupies a middle position, making legitimacy contingent on actuarial risk threshold rather than categorical rule, producing a more mixed and threshold-dependent beneficiary/victim allocation. Each story carries its own stable ε and its own claimed_type; they are linked here via affects_constraints rather than merged into one parameterized story, per Rule 1 of the committer frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
