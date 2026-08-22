% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__critical_psychiatry_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)
 *   domain: medical/epistemological/economic
 *
 * SUMMARY:
 *   The DSM (Diagnostic and Statistical Manual) is the foundational taxonomy
 *   of psychiatric diagnosis in North America. This constraint story
 *   instantiates the critical psychiatry reading: DSM categories are
 *   reverse-engineered from pharmaceutical availability rather than
 *   discovered from underlying neurobiological disease entities. The reading
 *   argues that pharmaceutical manufacturers and psychiatrists with industry
 *   ties shape the revision process to expand diagnostic scope, pathologize
 *   mild variants of normal functioning, and construct markets for
 *   psychotropic drugs. Patients are the primary victims, subjected to
 *   unnecessary diagnosis and overprescription with serious adverse effects.
 *   The beneficiaries are pharmaceutical capital and psychiatrists positioned
 *   within industry-funded structures. The constraint enforces this
 *   arrangement through professional gatekeeping, clinical guidelines,
 *   insurance reimbursement rules, and the marginalization of alternative
 *   etiological and treatment frameworks.
 *
 * KEY AGENTS:
 *   - Pharmaceutical manufacturers — agenda-setters and primary beneficiaries; shape category definitions to match available drugs
 *   - DSM revision committees — institutional agenda-setters; members have conflicts of interest; revise categories in directions that expand diagnosis
 *   - Psychiatrists with industry funding — beneficiaries; prescribe to populations matching expanded categories; have financial incentives for diagnostic expansion
 *   - Patients subjected to overprescription — primary victims; receive diagnoses and medications based on categories reverse-engineered from drugs; experience adverse effects
 *   - Treatment-resistant populations — trapped victims; experience blame rather than framework revision when drugs fail
 *   - Alternative theoretical frameworks and excluded researchers — structurally excluded from taxonomy authority; lack pharmaceutical funding
 *   - Psychiatric survivors and advocacy movements — excluded; document harm; lack institutional power to shift paradigms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical/epistemological/economic").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '042032c8-1b9a-40c4-8984-4b2d29640198').
narrative_ontology:cs_kernel_codification('042032c8-1b9a-40c4-8984-4b2d29640198', formalized).
narrative_ontology:cs_authority_grounding('042032c8-1b9a-40c4-8984-4b2d29640198', extraction).
narrative_ontology:cs_interpretation_layer_present('042032c8-1b9a-40c4-8984-4b2d29640198').
narrative_ontology:cs_reading_relation('042032c8-1b9a-40c4-8984-4b2d29640198', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('042032c8-1b9a-40c4-8984-4b2d29640198', dsm_taxonomy_kernel__neurodiversity_reading, influences).
narrative_ontology:cs_axiom('042032c8-1b9a-40c4-8984-4b2d29640198', foundational, dsm_categories_reverse_engineered_from_drugs).
narrative_ontology:cs_axiom_status(dsm_categories_reverse_engineered_from_drugs, holdable).
narrative_ontology:cs_axiom_grounding('042032c8-1b9a-40c4-8984-4b2d29640198', dsm_categories_reverse_engineered_from_drugs, empirically_contingent).
narrative_ontology:cs_axiom('042032c8-1b9a-40c4-8984-4b2d29640198', foundational, pharmaceutical_profit_incentives_shape_taxonomy).
narrative_ontology:cs_axiom_status(pharmaceutical_profit_incentives_shape_taxonomy, holdable).
narrative_ontology:cs_axiom_grounding('042032c8-1b9a-40c4-8984-4b2d29640198', pharmaceutical_profit_incentives_shape_taxonomy, empirically_contingent).
narrative_ontology:cs_reference_frame('042032c8-1b9a-40c4-8984-4b2d29640198', evidence_based_psychiatric_nosology).
narrative_ontology:cs_drift_state('042032c8-1b9a-40c4-8984-4b2d29640198', contemporary_pharmaceutical_market_saturation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('042032c8-1b9a-40c4-8984-4b2d29640198', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_receiving_industry_support).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, treatment_resistant_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, academic_psychiatry_departments).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_companies_and_healthcare_payers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop psychotropic drugs and work backward to construct DSM categories that define patient populations who need them. Sponsor diagnostic criteria research, fund psychiatrist continuing education, shape treatment guidelines through professional societies. Collects revenue directly from expanded market definitions and increased prescription volume. Can exit by product development in other therapeutic areas but are deeply embedded in psychiatric taxonomy construction.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, agenda_setter).

% Convenes psychiatrists to revise diagnostic criteria. Receives substantial funding from pharmaceutical industry; committee members have financial ties to manufacturers. Sets definitions that determine who gets diagnosed and treated. Justifies expansions as detecting previously unrecognized disease; critics argue they are expanding markets to match available drugs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_revision_committees, agenda_setter,
    institutional, generational, constrained, global).

% Receive speaking fees, research funding, consulting payments, and continuing education sponsorship from pharmaceutical companies. Prescribe medications to patients matching DSM categories; increased diagnosis rates increase their patient load and revenue. Most are not overtly corrupt but operate within an incentive structure that rewards diagnostic expansion and prescription volume. Exiting requires abandoning major funding and prestige pathways.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_receiving_industry_support, beneficiary,
    powerful, biographical, constrained, global).

% Receive diagnoses based on DSM categories that may not represent underlying neurobiological disease but rather behaviors matching category definitions. Prescribed psychotropic medications with serious adverse effects (metabolic syndrome, movement disorders, cognitive dulling, withdrawal phenomena). Once diagnosed and medicated, exiting is difficult because the diagnosis and medication history become part of their psychiatric identity and influence how providers treat them; seeking alternatives risks being labeled non-compliant or treatment-resistant.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription, payer,
    powerless, biographical, identity_locked, global).

% Experience genuine psychiatric distress that does not respond to DSM-mapped medications; are subjected to polypharmacy (multiple drugs in combination), escalating doses, and off-label use. Are blamed for their non-response rather than the category/drug mismatch being examined. Have no institutional advocate; clinical practice offers only deeper entrenchment in the pharmaceutical model.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, treatment_resistant_populations, payer,
    powerless, biographical, trapped, global).

% Psychosocial models, peer support approaches, trauma-informed care, neurodiversity paradigms, and critical psychiatry are structurally excluded from DSM revision and treatment guidelines. Their proponents lack pharmaceutical funding and institutional representation on taxonomy committees. Cannot participate in defining diagnostic legitimacy even when they offer explanatory power for treatment response patterns.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, alternative_theoretical_frameworks, excluded,
    powerless, generational, trapped, global).

% Reimburse for DSM-diagnosed conditions and prescribed medications. Expanded DSM categories increase their treatment costs and pharmaceutical expenses. They contest some definitions in coverage policy but lack the institutional power to reshape taxonomy or counter pharmaceutical influence on medical literature and professional societies.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_companies_and_healthcare_payers, payer,
    powerful, biographical, constrained, global).

% Conduct research on alternative etiologies (social determinants, trauma, structural violence) but face difficulty publishing in high-impact journals, obtaining competitive research funding, and influencing clinical practice. Their work is marginalized as 'non-biological' despite addressing mechanisms the pharmaceutical-driven taxonomy ignores.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_researchers_outside_pharmaceutical_orbit, excluded,
    moderate, biographical, constrained, global).

% Document adverse drug effects, recovery without medication, and iatrogenic harm from the DSM-pharmaceutical complex. Organize politically to shift paradigms away from categorical disease models. Are structurally excluded from taxonomy revision, dismissed as lacking medical authority, and accused of denying mental illness when they critique the pharmaceutical approach.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_survivors_and_advocacy_movements, excluded,
    organized, biographical, constrained, global).

% Develop treatment guidelines based on DSM categories and drug trial evidence. Heavily influenced by pharmaceutical-funded research and professional societies with industry ties. Guidelines become the standard of care, and deviating from them exposes physicians to liability, creating enforcement pressure for DSM-compliant prescribing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, clinical_practice_guidelines_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Receive substantial research funding from pharmaceutical companies; faculty obtain grants, speaker fees, and prestige from pharmaceutical-aligned research agendas. Department recruitment, promotion, and research priorities align with funding availability, creating institutional capture of psychiatric science.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, academic_psychiatry_departments, beneficiary,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The DSM creates a standardized diagnostic taxonomy enabling communication between clinicians, insurance systems, and researchers about psychiatric conditions. It coordinates professional language and treatment protocols across healthcare systems.
% TRANSFER_FUNCTION: Moves revenue from patients, insurance systems, and healthcare budgets to pharmaceutical manufacturers and to psychiatrists receiving industry support. Moves epistemic authority from alternative explanatory frameworks (psychosocial, trauma-informed, structural) to biologically-indexed disease categories that map to marketed drugs.
% ABSENT_VOICES: Psychiatric survivors describing recovery without medication and iatrogenic harm from drugs; researchers working on non-pharmaceutical etiologies; alternative paradigms (neurodiversity, peer support, trauma-informed, structural approaches); populations harmed by overprescription lack institutional representation in DSM revision and lack power to shift professional consensus.
% DISAPPEARANCE_RATIONALE: If the DSM-pharmaceutical linkage were severed (categories decoupled from drug availability, taxonomy revised to exclude reverse-engineered markets, funding streams disclosed and separated), clinical practice would reorganize: diagnostic frameworks would need to align with empirical treatment response rather than with available drugs; prescription rates would likely drop; pharmaceutical R&D would require different targeting; psychiatry would need alternative sources of legitimacy and funding.
% FOUNDING_PROBLEM: In the mid-20th century, psychiatry lacked a standardized diagnostic language; asylums operated with folk taxonomies; communication between clinicians was poor; no systematic nosology for insurance reimbursement existed.
% FOUNDING_PROBLEM_CORROBORATION: Historians of psychiatry (Healy, Whitaker, Kirk, Kutchins), psychiatric epidemiologists, and critical psychiatry movements attest the founding coordination problem (standardization of diagnosis) was solved by DSM-III and persists in attenuated form. Pharmaceutical industry documents (internal emails, marketing plans leaked in litigation) and former pharmaceutical executives (Healy's interviews, industry whistleblowers) corroborate that drug availability now drives category construction rather than disease discovery driving drug development. Academic psychiatry institutions have documented the expanding scope of DSM categories in each revision, with epidemiological rates of diagnosis rising to match drug marketing rather than disease burden trends.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderate-high because the arrangement generates direct pharmaceutical revenue, increases prescription volume beyond what empirical disease prevalence would predict, and transfers mental-health authority from psychosocial approaches to drug-indexed categories. Suppression (0.72) is high because the constraint's persistence depends on active enforcement: DSM authority enforced through professional licensing, clinical guidelines aligned with DSM, insurance reimbursement keyed to DSM diagnosis, marginalization of alternative frameworks in professional journals and training, and institutional pressure on practitioners to follow pharmaceutical-aligned protocols. Theater ratio (0.58) is moderately elevated because the diagnostic expansion narrative (detecting previously unrecognized disease) performs the real function (expanding drug markets) without openly declaring the pharmaceutical reverse-engineering. Accessibility collapse (0.64) is moderate because some alternatives (peer support, psychosocial explanation, trauma-informed care) exist but are starved of funding and institutional legitimacy; once a patient is diagnosed and labeled, exiting the pharmaceutical frame is difficult. Resistance (0.51) is moderate because psychiatric survivors, critical psychiatry movements, and some academic researchers actively contest the framework, but lack institutional power to reshape professional consensus. The measurement series show extractiveness accelerating from 1980–2000 (DSM-III/IV expansions), then plateauing at higher suppression requirement around 2010 as resistance movements grew and critiques accumulated; theater ratio rising throughout as the framework becomes more performative relative to its empirical grounding.
 *
 * PERSPECTIVAL GAP:
 *   The institutional beneficiaries (psychiatrists with industry ties, pharmaceutical manufacturers) experience this constraint as genuine coordination: the DSM provides standardization, enables communication, funds research, and supports professional legitimacy. The victims experience it as enforced extraction with theater: they receive diagnoses based on categories that reverse-engineer drug availability, are prescribed medications with serious adverse effects, and find alternatives systematically excluded from legitimacy. Treatment-resistant populations experience it as a trap: their non-response is interpreted as a personal failure (treatment resistance) rather than as feedback that the category-drug mapping may be wrong. The institutional gatekeepers (DSM committees, clinical guideline bodies) experience the constraint as inevitable professional progress; external critics experience it as institutional capture. The engine computes this divergence from the structural data: how power, exit options, and identified beneficiary/victim status differ across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers and industry-funded psychiatrists sit at the beneficiary end of the directionality spectrum (d near 0.0): the constraint directly increases their revenue, prestige, and institutional influence. Patients subjected to overprescription sit at the target end (d near 1.0): they bear the costs of diagnosis (stigma, identity effects) and medication (adverse effects, dependence, withdrawal difficulty) with minimal benefit under this reading. Treatment-resistant populations are fully trapped (d at 1.0): they experience blame rather than inquiry into framework mismatch. Insurance payers are caught in the middle (d near 0.5): they bear cost from expanded diagnosis but have some institutional power to contest coverage; they lack the power to reshape taxonomy. Excluded populations (alternative researchers, psychiatric survivors, neurodiversity advocates) face identity-locking: their ontological commitments are incompatible with DSM authority, making exit difficult even when they might benefit from alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (standardizing psychiatric diagnosis for communication and reimbursement) is DEAD or substantially SOLVED: psychiatrists communicate clearly using DSM; insurance systems have stable coding; professional training is systematic. Yet the constraint persists and has intensified (extractiveness and suppression rising from 1980–2010). This is classic mandatrophy: the founding problem no longer justifies the constraint, but the constraint persists because beneficiary interests (pharmaceutical revenue, psychiatrist prestige and funding) have become entrenched. Alternative frameworks that could solve the original coordination problem without pharmaceutical reverse-engineering (transparent diagnostic criteria, mechanism-based classification, psychosocial integration) are actively suppressed rather than competing on merit. The rising theater ratio and plateauing suppression requirement indicate that the constraint now persists primarily through institutional inertia and gatekeeping rather than through the coordination function it originally provided. Resistance movements (psychiatric survivors, critical psychiatry) correctly identify the mandatrophy: they argue the founding problem is solved and the framework should be radically revised or abandoned. The pharmaceutical and institutional psychiatry response is to defend the DSM by denying the mandatrophy — asserting that detection of mental illness is still improving, that drug development is still advancing, that alternatives are unproven — rather than openly defending the extractive arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reverse_engineering_hypothesis_empirical_sufficiency,
    'Can the pharmaceutical reverse-engineering hypothesis be distinguished empirically from genuine disease discovery?',
    'Examine the time sequence: did pharmaceutical development precede diagnostic category expansion? Does category expansion correlate with drug availability rather than with epidemiological disease burden or biological discovery? Historical comparative analysis of DSM revisions against pharmaceutical pipeline timelines, epidemiological data, and neuroscientific advances.',
    'If reverse-engineering is confirmed, the constraint is clearly extractive and the therapy ratio indicates institutional theater. If disease discovery precedes drugs, the constraint''s classification shifts toward coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_engineering_hypothesis_empirical_sufficiency, empirical, 'Whether DSM categories are reverse-engineered from drugs or discovered from disease.').

omega_variable(
    structural_alternative_to_pharmaceutical_model,
    'Can psychiatry maintain functional diagnostic standardization (original coordination goal) while decoupling from pharmaceutical incentives and integrating psychosocial etiologies?',
    'Natural experiment: jurisdictions that attempt DSM-independent diagnostic systems (e.g., ICD-11 in some countries, peer-led alternatives in some communities); measurement of outcomes (treatment response rates, adverse effect rates, functional recovery, patient satisfaction, cost-effectiveness).',
    'If alternative systems achieve comparable outcomes with lower adverse effects, the current constraint is revealed as suboptimal for the coordination goal itself. If alternatives fail, the pharmaceutical-linked taxonomy may be justified despite its extractive features.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_alternative_to_pharmaceutical_model, empirical, 'Whether the coordination function can be achieved without the pharmaceutical reverse-engineering mechanism.').

omega_variable(
    identity_lock_mechanism_in_diagnosed_patients,
    'Is the identity-locked exit option status of diagnosed patients structural (external barriers prevent exit) or internalized (patients'' self-concept and psychiatric identity prevent exit despite absent external barriers)?',
    'Longitudinal study of patients who exit psychiatric treatment: do they retain the diagnosed identity? Do they continue to see themselves as having the disease? Can they reframe their experiences in non-pathological terms? What triggers sustained non-compliance or alternative treatment seeking?',
    'If identity-lock is primarily internalized, suppression operates through non-material mechanisms (diagnosis as identity fusion, medication as proof of illness, clinical relationships as identity anchors); exiting requires not just material access to alternatives but cognitive decoupling from the psychiatric frame. If structural, overcoming exit barriers is sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_diagnosed_patients, empirical, 'The mechanism of identity-lock in psychiatric diagnosis: structural or internalized barriers.').

omega_variable(
    sibling_reading_epistemological_reconciliation,
    'Can the biomedical and critical psychiatry readings be reconciled within a single commitment framework?',
    'Formal analysis of the core premises: biomedical reading asserts disease entities are discoverable; critical reading asserts categories are reverse-engineered from drugs. These premises logically contradict if the claim is about CURRENT DSM categories (are they discovered or reverse-engineered?). However, both could be true of DIFFERENT categories or DIFFERENT time periods (some discovered, some reverse-engineered). The question is whether every category admits both explanations or whether the readings are genuinely disjoint.',
    'If the readings can coexist (some categories discovered, some reverse-engineered), the constraint classification is more complex and requires per-category analysis rather than a uniform type. If they are fully disjoint, the kernel is genuinely contested and one reading''s victory forecloses the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_epistemological_reconciliation, conceptual, 'Whether the biomedical and critical-psychiatry readings of DSM taxonomy are logically compatible or mutually foreclosing.').

omega_variable(
    institutional_capture_vs_legitimate_funding,
    'At what level of pharmaceutical funding do institutional decision-makers shift from legitimate professional collaboration to captured agents serving pharma interests rather than patients?',
    'Empirical threshold analysis: correlate funding levels (speaker fees, research grants, continuing education sponsorship, consulting retainers) with voting patterns on DSM committees, with diagnostic expansion decisions, with treatment guideline conservatism, with publication patterns. Statistical test for correlation and causal inference.',
    'A clear threshold would enable policy intervention (transparency requirements, funding caps, structural separation). Absence of a threshold (all levels of funding correlate with industry-favorable decisions) suggests structural capture rather than individual corruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_legitimate_funding, empirical, 'The funding-capture relationship in psychiatric governance institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dsm__tr_t5, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(dsm__tr_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(dsm__tr_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(dsm__tr_t25, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 30, 0.57).
narrative_ontology:measurement(dsm__tr_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dsm__be_t5, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(dsm__be_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(dsm__be_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(dsm__be_t25, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(dsm__be_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dsm__su_t5, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(dsm__su_t15, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(dsm__su_t20, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(dsm__su_t25, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(dsm__su_t40, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.2).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_marketing_to_physicians).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrist_prescribing_incentives).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, diagnostic_expansion_in_epidemiological_studies).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dsm_taxonomy_kernel. The kernel is the DSM itself. The sibling readings (biomedical and neurodiversity) instantiate different constraints with different beneficiary/victim structures, different ε values, and different classifications. Do NOT merge the readings into one story or try to average across them. Each reading is a separate constraint authored independently; their differences are the evidence the corpus measures. The network edges link the family members and establish their mutual influence: the critical psychiatry reading influences the other readings by challenging their epistemic authority; the biomedical reading influences this one by claiming empirical primacy; the neurodiversity reading influences both by reframing pathology as variation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
