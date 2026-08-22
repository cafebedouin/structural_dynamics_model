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
 *   human_readable: DSM-Pharmaceutical Market Coupling (Critical Psychiatry Reading)
 *   domain: medical/pharmaceutical/epistemic
 *
 * SUMMARY:
 *   The Diagnostic and Statistical Manual of Mental Disorders (DSM) is the
 *   primary diagnostic taxonomy in psychiatry, used to classify mental
 *   disorders and direct treatment. This constraint story instantiates the
 *   critical psychiatry reading: DSM categories are structurally
 *   reverse-engineered from available pharmaceutical treatments to construct
 *   and expand markets for psychotropic drugs, rather than discovered through
 *   neuroscience. The reading holds that pharmaceutical manufacturers and
 *   industry-affiliated psychiatrists shape DSM revision processes to ensure
 *   new diagnostic categories align with drugs already developed, expanding
 *   the population eligible for prescriptions. Patients receive diagnoses and
 *   medications for conditions of dubious neurobiological validity, suffer
 *   serious adverse effects, and are locked into psychiatric identity frames
 *   that suppress exit. The constraint persists because psychiatrists'
 *   professional authority and financial incentives depend on the
 *   pharmaceutical-disease model, and because patients are identity-locked
 *   into the diagnostic categories themselves. This is the critical
 *   psychiatry reading of a contested kernel; sibling readings (biomedical,
 *   neurodiversity) offer different structural assessments of the same
 *   DSM-as-authority-text.
 *
 * KEY AGENTS:
 *   - Pharmaceutical manufacturers (institutional beneficiary, agenda-setter) — profit from DSM-aligned drug marketing and expanded indications
 *   - Industry-affiliated psychiatrists (institutional beneficiary, agenda-setter) — receive funding, secure professional authority through DSM-centric nosology
 *   - Patients subjected to overprescription (powerless victim, identity-locked) — receive diagnoses and medications with serious adverse effects
 *   - DSM revision committees (institutional agenda-setter) — formally author diagnostic criteria under pharmaceutical and professional influence
 *   - Non-affiliated psychiatrists (organized, constrained) — benefit from diagnostic coordination but pressured to prescribe within pharmaceutical norms
 *   - Critical psychiatry scholars (moderate, excluded) — document market-making but lack institutional power to revise the system
 *   - Public health authorities (institutional observer) — regulate but face capture through industry relationships and ideological commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.71).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM-Pharmaceutical Market Coupling (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical/pharmaceutical/epistemic").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '629b2bb6-66da-422d-8044-cca6255d6355').
narrative_ontology:cs_kernel_codification('629b2bb6-66da-422d-8044-cca6255d6355', formalized).
narrative_ontology:cs_authority_grounding('629b2bb6-66da-422d-8044-cca6255d6355', extraction).
narrative_ontology:cs_interpretation_layer_present('629b2bb6-66da-422d-8044-cca6255d6355').
narrative_ontology:cs_reading_relation('629b2bb6-66da-422d-8044-cca6255d6355', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('629b2bb6-66da-422d-8044-cca6255d6355', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('629b2bb6-66da-422d-8044-cca6255d6355', foundational, dsm_categories_market_constructed).
narrative_ontology:cs_axiom_status(dsm_categories_market_constructed, holdable).
narrative_ontology:cs_axiom_grounding('629b2bb6-66da-422d-8044-cca6255d6355', dsm_categories_market_constructed, empirically_contingent).
narrative_ontology:cs_axiom('629b2bb6-66da-422d-8044-cca6255d6355', foundational, pharmaceutical_profit_extraction_structural).
narrative_ontology:cs_axiom_status(pharmaceutical_profit_extraction_structural, holdable).
narrative_ontology:cs_axiom_grounding('629b2bb6-66da-422d-8044-cca6255d6355', pharmaceutical_profit_extraction_structural, deontological).
narrative_ontology:cs_reference_frame('629b2bb6-66da-422d-8044-cca6255d6355', neurobiological_disease_model).
narrative_ontology:cs_drift_state('629b2bb6-66da-422d-8044-cca6255d6355', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('629b2bb6-66da-422d-8044-cca6255d6355', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_industry_affiliated).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, off_label_prescription_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, non_affiliated_psychiatrists).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, non_affiliated_psychiatrists).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, clinical_psychologists_non_prescribers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and markets psychotropic drugs; funds research into DSM categories and disease awareness campaigns; finances psychiatrist continuing education, professional societies, and conference speakers. Profits from expanded diagnostic categories that increase potential patient populations and prescription volumes. Can exit to alternative markets but is heavily invested in psychiatric nosology as the primary channel for drug legitimacy.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive speaker fees, consulting payments, and research funding from pharmaceutical companies; author DSM editions and revisions; serve on boards of professional organizations that set diagnostic standards; build careers on the credibility of psychiatric nosology. Their professional identity fuses with the psychiatric taxonomy system; departing from pharmacological framing of psychiatric disorders threatens career legitimacy and institutional standing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_industry_affiliated, agenda_setter,
    institutional, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_industry_affiliated, beneficiary).

% Officially author each DSM edition; consist primarily of psychiatrists with industry ties; face pressure to expand categories to align with available pharmaceutical interventions; operate under the authority of the American Psychiatric Association (APA), a professional body whose financial health depends on DSM licensing and whose governance is influenced by psychiatrists who benefit from pharmaceutical markets.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_revision_committees, agenda_setter,
    institutional, generational, constrained, global).

% Receive psychiatric diagnoses from DSM categories that have expanded to capture normal distress, grief, social nonconformity, or transient behavioral patterns; are prescribed psychotropic medications with serious adverse effects (metabolic syndrome, movement disorders, sexual dysfunction, cognitive dulling, dependence); cannot easily exit psychiatric care because the diagnosis becomes part of their medical and social identity, and psychiatric symptoms (real or iatrogenic) are medicalized as evidence requiring continued treatment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_subjected_to_overprescription, payer,
    powerless, biographical, identity_locked, global).

% Receive psychotropic drugs for conditions not FDA-approved for those drugs (children prescribed antipsychotics for ADHD, antidepressants for anxiety in off-label combinations, anticonvulsants for bipolar disorder); experience adverse effects not accounted for in the off-label use; have minimal recourse because off-label prescribing is legal and diagnostic categories are elastic enough to justify the prescription after the fact.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, off_label_prescription_recipients, payer,
    powerless, biographical, trapped, global).

% Benefit from diagnostic categories that organize clinical knowledge and offer treatment pathways; face pressure to prescribe within pharmaceutical marketing norms and DSM frameworks or risk professional isolation; are not financially compensated by industry but operate within a system where industry influence shapes the diagnostic language itself, constraining their alternatives.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, non_affiliated_psychiatrists, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, non_affiliated_psychiatrists, payer).

% Cannot prescribe in most jurisdictions; use DSM categories for diagnosis and treatment planning but are excluded from defining them; compete with psychiatrists for patients but are systematically disadvantaged when diagnoses get framed as requiring pharmacological intervention; are largely absent from DSM revision committees and industry-funded research networks.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, clinical_psychologists_non_prescribers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, clinical_psychologists_non_prescribers, excluded).

% Regulate psychiatric drug approval and off-label use; conduct or commission pharmacovigilance; monitor psychiatric diagnosis rates and prescription trends; face institutional capture through industry-funded research, regulatory agency-pharma revolving doors, and ideological commitment to the pharmacological model of psychiatric illness.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% Publish research documenting the pharmaceutical industry's role in DSM expansion, the lack of neurobiological validation for most psychiatric diagnoses, and the adverse effects of psychotropic drugs; are largely excluded from mainstream psychiatric institutions, professional organizations, and grant-funded research; publish primarily in alternative journals and face stigmatization as fringe critics within their field.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_scholars, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common diagnostic language across clinicians, researchers, and pharmaceutical companies; enables standardized treatment protocols and facilitates communication about psychiatric patient populations; provides a framework for insurance reimbursement and clinical trial design.
% TRANSFER_FUNCTION: Moves profit from patients and public health systems to pharmaceutical manufacturers through expanded prescription volumes and off-label use; moves professional prestige, consulting fees, and research funding from pharmaceutical companies to industry-affiliated psychiatrists and DSM revision committees; moves credibility from alternative explanatory frameworks (social, environmental, existential) to the pharmaceutical-disease model.
% ABSENT_VOICES: Patients with lived experience of psychiatric treatment who feel harmed by overprescription are largely excluded from diagnostic revision processes; critical psychiatry scholars and psychosocial treatment advocates are absent from DSM committees; families and community members who question pharmaceutical frames are not seated in official nosology discussions; voices documenting long-term adverse effects of psychiatric drugs are systematically marginalized.
% DISAPPEARANCE_RATIONALE: If this constraint—the coupling of DSM categories to pharmaceutical markets—vanished, diagnostic categories would likely contract, off-label prescribing would face institutional scrutiny, prescription volumes would decline sharply, and pharmaceutical profit margins from psychiatric drugs would compress. Alternative frameworks (social determinants, existential suffering, environmental stressors, normal human variation) would regain legitimacy. Psychiatrists' financial relationships with industry would need to be transparently managed or eliminated. The field would reorganize around diagnostic agnosticism, psychosocial treatment, and evidence-based limits on pharmacological intervention.
% FOUNDING_PROBLEM: Mid-20th-century psychiatry lacked systematic nosology; clinicians used inconsistent terminology; pharmaceutical development lacked clear disease targets; insurance reimbursement needed categorical diagnoses; international psychiatric research required common language.
% FOUNDING_PROBLEM_CORROBORATION: The original coordination problem (lack of common diagnostic language) was substantially solved by the DSM-III (1980) and has been maintained through subsequent editions. Psychiatry now has standardized terminology, international coordination (ICD), and diagnostic reliability. However, this founding problem is attested as 'solved' only by the psychiatric establishment itself. Critical psychiatry scholars, pharmaceutical reform advocates, and patients' rights organizations attest that the contemporary DSM serves primarily to expand markets and legitimize pharmaceutical intervention, not to solve a live diagnostic coordination problem. Academic historians of psychiatry document the explicit shift in DSM-III away from psychodynamic/social causation toward categorical disease entities aligned with pharmaceutical treatment targets.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises steeply from 1950 (0.15, low—DSM-I was psychodynamically oriented, few psychotropic drugs) to 1980 (0.42, DSM-III shift to categorical disease entities aligned with pharmaceutical targets) and continues rising through 2024 (0.68—modern DSM reflects successive rounds of pharmaceutical input and category expansion). The measurement series uses a shared time grid (1950, 1980, 1994, 2005, 2015, 2024) so every metric is authored at every point. Theater ratio rises more slowly (0.25 to 0.52), indicating that while the functional purpose of the DSM includes genuine diagnostic coordination, an increasing share of the constraint's enforcement activity defends pharmaceutical market expansion rather than clinical communication. Suppression (0.71 at endpoint) is high because the constraint's persistence requires active suppression of alternative psychiatric frameworks (psychosocial, existential, neurodiversity, defect-model critique) through institutional, financial, and epistemic barriers. Accessibility collapse (0.64) reflects that once patients are diagnosed and medicated, exiting the psychiatric framework is difficult—alternatives become cognitively unavailable due to identity fusion and pharmacological dependence. Resistance (0.58) is substantial because critical psychiatry scholars, patients' advocates, and alternative practitioners actively challenge the pharmaceutical-disease model, though they lack institutional power to unseat it.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (pharmaceutical manufacturers, industry psychiatrists), the DSM represents a genuine scientific achievement—progressively accurate classification of objective diseases, enabling evidence-based treatment and helping patients access needed care. Professional identity and career trajectories are deeply invested in this framing. From the patient seats (especially powerless victims of overprescription), the DSM is a mechanism of over-medicalization and iatrogenic harm—using normal human distress or neurodiversity as justification for dangerous drugs. From the critical psychiatry scholar seats, the DSM is a constructed market structure disguised as science. From the public health seat, the DSM is an authority framework whose legitimacy is partly captured by pharmaceutical influence, creating regulatory blind spots. The engine computes each seat's classification from the structural data (power, exit, beneficiary/victim role); the perspectival gap emerges naturally from asymmetric structural positions. The claim (tangled_rope) reflects the reading's judgment that both coordination and extraction are structurally present; the metrics (high extractiveness, high suppression, moderate theater) describe the reading's view of how the constraint actually operates.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical manufacturers and industry-affiliated psychiatrists occupy the beneficiary-to-agenda-setter end of the directionality spectrum (d near 0.0–0.2): they collect the extraction (profit, professional prestige, research funding) and control the rules (DSM revision, continuing education standards). Patients subjected to overprescription occupy the target end (d near 0.8–1.0): they pay through adverse effects, identity entrapment, and dependence while having minimal exit options. Non-affiliated psychiatrists sit near the symmetric point (d ≈ 0.5): they genuinely benefit from diagnostic coordination (solving a real communication problem) but also bear costs (pressure to prescribe within pharmaceutical norms, reputational constraints if they question the model). Critical psychiatry scholars are excluded rather than coordinated, facing career penalties for dissent. This asymmetry—beneficiary control, target powerlessness, forced entry, limited exit—is what makes this tangled_rope (both coordination and extraction) rather than rope (pure coordination) or snare (pure extraction with fake coordination). The coordination function (standardized diagnostic language) is genuine; the extraction (pharmaceutical profit and professional control) is real and substantial.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of standardized psychiatric diagnostic language, enabling interoperability and research) has been SOLVED—the DSM now provides the taxonomy. Yet the constraint persists and has strengthened (extractiveness rising from 0.15 to 0.68). This is mandatrophy: the original function has atrophied or been displaced, but the constraint persists and new functions (pharmaceutical market expansion, professional gatekeeping) have taken over. The theater ratio (0.52) shows that roughly half the constraint's operation is now performative—DSM revision committees theatrically debate diagnostic criteria while pharmaceutical influence ensures outcomes align with market interests. The six_questions verdict captures this: founding_problem_status=dead, disappearance_verdict=world_rearranges. If the constraint vanished, the diagnostic coordination problem would be solved through alternative mechanisms (international ICD systems, evidence-based diagnosis independent of pharmaceutical markets), but pharmaceutical profit margins would compress and psychiatrists' financial incentives would collapse. This asymmetry—the founding problem is solved but the constraint persists, now serving extractive functions—is the diagnostic signature of mandatrophy. The constraint should be flagged for potential remediation: either sunset the coupling of DSM categories to pharmaceutical markets (regulatory intervention), or decompose the DSM from the pharmaceutical-approval apparatus (institutional reform), or establish explicit caps on diagnostic category expansion (epistemic governance). The classification resists the false-summit diagnosis (the constraint is not a natural law) and avoids the piton trap (the constraint is actively enforced, not merely theatrical inertia—theater_ratio is moderate, not high).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_necessity_vs_profit_motive,
    'Is DSM category expansion driven by genuine psychiatric innovation—new disease discovery via improved neuroscience—or primarily by pharmaceutical profit incentives seeking larger patient populations for drugs already developed?',
    'Comparative analysis of (a) DSM category additions and their relationship to preceding pharmaceutical drug development timelines; (b) diagnostic criteria expansion in categories where new drugs became available vs. categories where drugs were already established; (c) pharmaceutical industry funding flows to research supporting each new DSM category; (d) academic studies independent of industry funding.',
    'If expansion follows drug development more closely than neuroscience advancement, the extractive reading gains structural support and the biomedical reading''s claim to objective disease discovery is undermined. If expansion correlates with neuroscience progress and precedes drug development, the biomedical reading gains support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_necessity_vs_profit_motive, empirical, 'Whether DSM expansion reflects pharmaceutical market-making or genuine disease discovery.').

omega_variable(
    industry_capture_mechanism,
    'How thoroughly do pharmaceutical industry relationships constrain the behavior of psychiatrists and committees that set DSM standards? Is the suppression of alternative explanations (social, existential, neurodiversity) due to genuine epistemic disagreement or institutional capture?',
    'Survey of DSM revision committee members disclosing financial relationships with industry; analysis of speaker fees, consulting payments, and research funding flowing to prominent psychiatrists; comparison of career outcomes for psychiatrists who accept vs. reject industry funding; study of whether non-industry-affiliated psychiatrists show different diagnostic practices; longitudinal analysis of dissent within psychiatric organizations.',
    'Evidence of systematic capture would establish that suppression (0.71) is actively maintained through financial incentives and career constraints rather than scientific consensus. This would support the tangled_rope type (genuine coordination function coordinated alongside asymmetric extraction) and the critical psychiatry reading''s validity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_capture_mechanism, empirical, 'Extent of pharmaceutical industry capture of DSM standard-setting.').

omega_variable(
    kernel_reading_frame_underdetermination,
    'Is this constraint reading one READING of a single kernel (DSM categories as a fixed text/practice under contestation) or does it describe a structurally different constraint altogether from the biomedical reading?',
    'Examine whether the two readings (critical and biomedical) describe the SAME phenomenon (DSM as authority structure) assessed differently, or whether they describe DIFFERENT constraints with different ε values (pharmaceutical-market-coupling vs. disease-classification). If the readings describe genuinely different constraints—different referents, different victim/beneficiary sets, different extractiveness—then they should be separate constraint stories, not readings of one kernel.',
    'If this is truly a reading of a shared kernel, the cs_structure.reading_relations properly route to coexists_with (both readings live, different parties hold them). If the critical reading describes a different constraint (market-coupling) while the biomedical reading describes genuine disease classification, they are separate constraint stories and should not share a kernel. This omega documents the framing ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_frame_underdetermination, conceptual, 'Whether the critical psychiatry reading is a genuine kernel reading or a structurally distinct constraint.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) of alternative psychiatric frameworks primarily structural—institutional barriers, career penalties, funding unavailability—or have patients and practitioners internalized the pharmaceutical-disease frame such that alternatives feel inaccessible even absent external coercion?',
    'Post-exit studies: when patients discontinue psychiatric treatment and diagnosis, do suppressive beliefs about psychiatric illness persist? When psychiatrists leave industry-funded networks, do their diagnostic practices shift? Do practitioners in countries with weaker pharmaceutical influence show different diagnostic frames? Do critical psychiatry scholars inside mainstream institutions report different barriers than structural barriers (funding denial, publication barriers) alone?',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests—patients and practitioners carry the frame with them after exit. If structural barriers are primary, removing them (funding transparency, institutional reform, career path diversification) would weaken the constraint more directly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of alternative psychiatric frames is structural or internalized.').

omega_variable(
    coordination_function_genuine_or_post_hoc,
    'Does the DSM''s coordination function (standardized diagnostic language) represent a genuine collective-action solution that participants benefit from, or is the coordination function post-hoc justification for a constraint primarily designed to expand pharmaceutical markets?',
    'Survey practitioners about whether they perceive the DSM as solving a real problem (diagnostic confusion, inconsistent terminology) vs. as an obstacle to nuanced clinical judgment. Analysis of whether diagnostic standardization would be achievable through non-pharmaceutical-aligned mechanisms (e.g., ICD-11 without pharmaceutical framing). Examine whether expansion of diagnostic categories improves clinical outcomes or merely increases prescription volumes.',
    'If coordination is genuine and valued, the constraint is closer to tangled_rope (real coordination + extraction). If coordination is a post-hoc cover story, it shifts toward snare (extraction with a coordination facade). Clinical outcome data showing that DSM expansion correlates with adverse outcomes, not improved treatment, would support snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuine_or_post_hoc, empirical, 'Whether DSM standardization solves a genuine clinical coordination problem or serves primarily as a market structure.').

omega_variable(
    sibling_reading_frame_differences,
    'The biomedical and neurodiversity readings of this kernel—do they describe the same DSM-as-authority-structure under contestation (a genuine kernel), or do they describe different constraints entirely?',
    'The biomedical reading treats DSM categories as progressively accurate descriptions of objective neurobiological entities; the neurodiversity reading treats them as pathologization of natural neurological variation. These describe different REFERENTS for ε: one asks ''is this category accurately mapping neurobiological disease?'' the other asks ''is this taxonomy pathologizing normal variation?'' If the kernel is the DSM-as-text, both readings should produce different ε values for the same phenomenon. If they are structurally different constraints (disease-validity vs. pathologization), they should be separate stories with a network link.',
    'If genuinely the same kernel, cs_structure.reading_relations should properly route all sibling pairs. If the referents differ, the committer frame may be misconstrued and separate stories should be authored. This omega documents the frame-ambiguity risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_frame_differences, conceptual, 'Whether the biomedical and neurodiversity readings share a kernel or describe different constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1950, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1994, 0.43).
narrative_ontology:measurement(dsm__tr_t2005, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(dsm__tr_t2015, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2015, 0.51).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1950, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1994, 0.55).
narrative_ontology:measurement(dsm__be_t2005, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(dsm__be_t2015, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1950, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1994, 0.62).
narrative_ontology:measurement(dsm__su_t2005, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(dsm__su_t2015, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_marketing_disease_mongering).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_professional_gatekeeping).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychotropic_drug_adverse_effects_market).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the DSM_taxonomy_kernel, which admits three structurally distinct readings: biomedical (disease discovery), critical_psychiatry (market construction), and neurodiversity (pathologization). Each reading produces different ε values and victim/beneficiary sets for the same DSM-as-authority-text. The critical reading here argues that pharmaceutical market interests structurally shape the DSM; the biomedical reading argues the DSM progressively maps objective diseases; the neurodiversity reading argues the DSM pathologizes normal variation. These are not mere differences of opinion but competing framings of the kernel's legitimacy and function. All three readings should be authored as separate constraint stories (with shared kernel_id, unique reading_id, and network links) to preserve the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
