% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Categories as Pharmaceutical Market-Construction Device (Critical Psychiatry Reading)
 *   domain: medical epistemology / psychiatric taxonomy / social construction of illness
 *
 * SUMMARY:
 *   Since DSM-III (1980), the American Psychiatric Association's diagnostic
 *   manual has supplied the shared vocabulary through which psychiatric care,
 *   research, reimbursement, and drug regulation operate. This story authors
 *   the critical-psychiatry account of that standing arrangement: diagnostic
 *   categories expand and reshape along the contours of what available
 *   compounds can treat, so that each widening of a category enlarges the
 *   addressable market for the drug matched to it. The arrangement retains a
 *   real coordination function — a common nosology is load-bearing for
 *   clinical communication, research aggregation, and insurance
 *   administration — while a parallel transfer runs through the same
 *   structure: prescription volume flows to manufacturers, honoraria and
 *   trial funding flow to opinion leaders, royalties flow to the manual's
 *   publisher, and the costs (adverse effects, dependence, withdrawal,
 *   lifelong medication) concentrate on diagnosed patients. Enforcement is
 *   active and distributed: coding gates at insurers, indication approvals at
 *   the regulator, guideline compliance in clinics, and professional
 *   marginalization of dissent. KEY AGENTS (by structural relationship): -
 *   pharmaceutical_manufacturers: primary beneficiary
 *   (institutional/arbitrage) — collects the prescription revenue created by
 *   category-drug alignment and shapes the categories it feeds on -
 *   american_psychiatric_association: agenda setter (institutional/arbitrage)
 *   — owns and revises the manual; collects royalties and sponsorship -
 *   industry_funded_key_opinion_leaders: beneficiary
 *   (powerful/identity_locked) — career-fused intermediaries between
 *   categories and compounds - long_term_psychotropic_patients: primary
 *   target (powerless/trapped) — bears the chronic medication burden; exit
 *   runs through the same codes - off_label_prescribed_children: target
 *   (powerless/trapped) — medicated under expanding childhood categories -
 *   frontline_prescribers: dual-positioned conduit (organized/constrained) —
 *   paid by the pipeline, liable within it - insurance_industry: enforcing
 *   gatekeeper (institutional/arbitrage) — makes the code the price of
 *   covered care - fda_regulators: institutional observer — approves the
 *   indications binding drugs to populations - critical_psychiatry_scholars:
 *   analytical observer — documents the financial wiring -
 *   patient_advocacy_networks: excluded voice (moderate/trapped) — contests
 *   from lived experience, absent from workgroups
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.71).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.61).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Categories as Pharmaceutical Market-Construction Device (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical epistemology / psychiatric taxonomy / social construction of illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262').
narrative_ontology:cs_kernel_codification('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', formalized).
narrative_ontology:cs_authority_grounding('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', extraction).
narrative_ontology:cs_interpretation_layer_present('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262').
narrative_ontology:cs_reading_relation('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', foundational, diagnostic_categories_track_treatment_availability).
narrative_ontology:cs_axiom_status(diagnostic_categories_track_treatment_availability, holdable).
narrative_ontology:cs_axiom_grounding('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', diagnostic_categories_track_treatment_availability, empirically_contingent).
narrative_ontology:cs_axiom('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', secondary, nosological_expansion_serves_market_construction).
narrative_ontology:cs_axiom_status(nosological_expansion_serves_market_construction, holdable).
narrative_ontology:cs_axiom_grounding('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', nosological_expansion_serves_market_construction, empirically_contingent).
narrative_ontology:cs_reference_frame('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', neutral_descriptive_nomenclature).
narrative_ontology:cs_drift_state('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', post_blockbuster_pharmaceutical_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7e7f82a5-0b67-4e5f-a9bb-cdf9f81ca262', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_key_opinion_leaders).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, american_psychiatric_association).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, long_term_psychotropic_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, off_label_prescribed_children).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_prescribers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_prescribers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_industry).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, chemical_imbalance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, patent, and sell psychotropic medications. Revenue depends on prescription volume written under diagnostic labels, so they fund the trials, speaker bureaus, continuing education, and guideline panels through which diagnostic categories and drug indications travel together. When a category boundary widens, the addressable market for the matching compound widens with it. They can redirect portfolios toward whichever diagnostic territories are expanding and away from contracting ones.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers, agenda_setter).

% Owns and publishes the diagnostic manual, convenes the workgroups that define and revise each category, and sets the criteria that determine what counts as a treatable condition. Manual editions are major revenue events for the association, and its conferences and journals carry substantial pharmaceutical sponsorship. Workgroup members disclose financial conflicts, but the association maintains that the revision process is scientifically independent.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, american_psychiatric_association, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, american_psychiatric_association, beneficiary).

% Senior clinicians and researchers whose careers advanced through industry-funded trials, speaker engagements, and advisory boards. They author the chapters, present the findings, and sit on the workgroups where category definitions are drafted. Their professional standing, income, and research programs are built inside the diagnostic-pharmaceutical pipeline; stepping outside it would mean abandoning the platform their reputations stand on.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_key_opinion_leaders, beneficiary,
    powerful, biographical, identity_locked, global).

% Carry diagnoses that route them into long-term medication regimens, often multiple drugs taken for years or decades. They bear the adverse effects, metabolic burdens, dependence, and difficult withdrawal that accompany chronic psychotropic use, and their access to therapy, disability support, and sometimes housing runs through the same diagnostic codes that keep them in the medication channel. Leaving the diagnostic system means losing coverage and services.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, long_term_psychotropic_patients, payer,
    powerless, biographical, trapped, global).

% Children diagnosed under expanding childhood categories and medicated, frequently with compounds approved only for adult conditions. Guardians consent within a system where the diagnostic label unlocks school accommodations and insurance payment, and the children themselves have no independent position from which to refuse or exit.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, off_label_prescribed_children, payer,
    powerless, immediate, trapped, national).

% Psychiatrists and primary-care physicians who write the prescriptions. Reimbursement for short medication-management visits depends on billable diagnostic codes, so the arrangement pays their salaries; at the same time they absorb liability for adverse outcomes, formulary and guideline compliance pressure, and the erosion of professional authority as prescribing protocols tighten around them. Practicing outside the coded system means forfeiting insurance participation.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_prescribers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_prescribers, payer).

% Requires a diagnostic code before authorizing payment for any mental-health encounter, making the manual the gate through which all covered care passes. They also absorb the drug bills that widening categories generate, and respond by tightening prior authorization, shifting costs, and adjusting formularies rather than by challenging the categories themselves.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_industry, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_industry, payer).

% Evaluates trial evidence and approves the indications that formally connect each drug to a diagnostic population. Takes testimony from sponsors, academics, and patient groups, issues warnings when harms surface, and can widen or narrow the labeled territory a compound may claim.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, fda_regulators, observer,
    institutional, generational, analytical, national).

% Clinicians, historians, and social scientists who document the financial ties between manual revision and drug marketing, publish analyses of category expansion, and propose alternative frameworks. They hold no administrative power over the manual and are frequently characterized by the profession's leadership as ideological rather than scientific.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_scholars, observer,
    moderate, generational, analytical, global).

% Organized groups of current and former patients — withdrawal-support communities, harm-reduction networks, reform campaigns — who contest category expansion and chronic polypharmacy from lived experience. They are largely absent from workgroup deliberations and guideline panels; their participation is limited to public-comment periods and protest.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patient_advocacy_networks, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_manufacturers).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a single shared diagnostic vocabulary that lets thousands of otherwise unconnected clinicians, researchers, insurers, and regulators describe the same patient the same way — enabling case communication, cumulative research, standardized billing, and epidemiological counting across a decentralized care system.
% TRANSFER_FUNCTION: Moves prescription revenue from patients and public/private payers to pharmaceutical manufacturers; moves honoraria, consultancy fees, and trial funding from manufacturers to opinion leaders and workgroup members; moves royalty and sponsorship income to the manual's publisher; and moves diagnosed individuals into long-term, often multi-drug, pharmacological treatment whose adverse-effect burden they carry.
% ABSENT_VOICES: Long-term patients experiencing adverse effects and withdrawal are absent from workgroup deliberation; independent pharmacologists and unfunded researchers are absent from the trial literature that justifies category-drug pairing; psychotherapists practicing modalities without medication indications are absent from the coding committees that decide what gets reimbursed. Organized patient networks reach public-comment channels only.
% DISAPPEARANCE_RATIONALE: If the category-drug coupling vanished overnight — categories frozen, indications severed, coding gates opened — prescribing volumes would contract sharply, manufacturers would reroute portfolios, opinion-leader income streams would dry up, and care would reorganize around indication-free clinical judgment and non-drug pathways. The coordination vocabulary itself would survive in diminished form, which is precisely the seam this reading draws between the arrangement's function and its yield.
% FOUNDING_PROBLEM: Psychiatric diagnosis lacked reliability: before the 1980 recoding, different clinicians gave the same patient different labels, research could not accumulate, and the field's scientific standing was under existential attack. The manual was rebuilt to make diagnosis reproducible and communicable.
% FOUNDING_PROBLEM_CORROBORATION: Historians of psychiatry and health-services researchers outside the industry attest the original reliability project and its partial success; former workgroup members publishing without manufacturer ties attest that later expansions outran the reliability mandate. The manufacturer and publisher seats attest the opposite — that each expansion answers live clinical need — and no source inside the beneficiary set can adjudicate the dispute; the contest is therefore corroborated as a contest rather than settled by any party's self-description.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.71 (moderate-high, per this reading's assessment of the standing arrangement): prescription volume scales with category breadth, and the margin between drug prices and the clinical necessity of chronic multi-drug regimens is the arrangement's yield. Suppression is 0.61, authored as a raw structural property (only extraction is scaled by the engine): the coding gate at insurers makes the diagnosis the price of covered care, and professional sanction disciplines dissenting clinicians, but non-medical alternatives persist at the margins. Theater_ratio 0.52: guideline rituals, checklist interviews, advisory boards, and awareness campaigns increasingly perform compliance and market-shaping while core clinical judgment persists alongside. Accessibility_collapse 0.55: once the category-drug coupling is visible, alternatives (psychotherapy-first pathways, non-DSM formulations, deprescribing) remain reachable but are penalized by reimbursement design. Resistance 0.55: a sustained critical-literature current, patient withdrawal movements, and periodic regulatory warnings meet the arrangement without displacing it. Claimed type is tangled_rope on structural grounds independent of these scores: a genuine coordination function (shared nosology) and an asymmetric transfer (sales up, burden down onto patients) run through the same enforced structure. The temporal series share one grid (1980-2025, eight points) so no metric borrows another's end-state; all three trajectories rise together — extraction accumulation and enforcement ratcheting in step — which is the signature this reading expects of a coordination structure progressively converted to market service. On coalition potential for the powerless victim seats: withdrawal-support networks and deprescribing campaigns are precisely coalition formation, but the same coding gate that traps individuals fragments class action, since each patient's coverage is individually contingent on retaining a diagnosis.
 *
 * PERSPECTIVAL GAP:
 *   Seats should diverge sharply. From the manufacturer seat the arrangement computes as functioning coordination it invests heavily to maintain — the categories are the market. From the trapped patient seats the same structure computes as enforced extraction with no exit. The prescriber seat sits between: paid by the pipeline, disciplined by it, and increasingly aware of the gap between the two descriptions. The workgroup seat experiences revision as scientific labor; the scholar and advocate seats experience the same revision as commercial choreography. The opinion-leader seat is identity_locked: the fusion is career-path dependence (decades of funded work constitute the reputation) as much as worldview — if the frame broke, exit would still be blocked by sunk career capital, which is why the lock is structural as well as psychological. The engine computes these per-seat types from the structural data; the divergence is the finding, not something the authored claim resolves.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (manufacturers, opinion leaders, the association) derive low d for those seats; the arbitrage exit available to manufacturers and the association presses them toward the beneficiary pole. Victim declarations (long-term patients, prescribed children) with trapped exit derive high d — trapped targets sit near the full-target end. The override for the organized power atom exists because frontline prescribers carry no entry in the beneficiary or victim arrays: the canonical fallback for organized agents would place them near symmetric, but their net position is slightly target-side — reimbursement gains are thin per-visit margins while liability, compliance burden, and professional-erosion costs accrue to them continuously — so d is overridden to 0.40. Insurers enforce the gate (agenda-side) while absorbing drug bills (payer-side); their net d sits near symmetric and is left to derivation. Scholars and the regulator hold analytical seats and take observer directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Reading the arrangement as a pure snare would erase the real coordination the manual performs — clinical communication, research aggregation, and reimbursement administration genuinely depend on a shared nosology, and abolishing the categories overnight would break those functions, not just the rents. Reading it as a pure rope would erase the asymmetric transfer the same structure carries — category breadth tracks treatment availability closely enough that the coordination cannot be evaluated apart from the market it builds. Mandatrophy is not yet resolved: the founding problem (reliable shared diagnosis) is contested rather than dead, because reliability remains a live clinical need even as this reading holds that the manual's growth phase now serves market construction. If a successor framework (biomarker-based or functional) ever absorbs the coordination function while the category-drug coupling persists in legacy form, the residual would decay toward a piton — theatrical maintenance of categories nobody clinically needs; the theater_ratio series is the early-warning instrument for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the critical_psychiatry_reading of the dsm_taxonomy_kernel; the biomedical_reading and neurodiversity_reading siblings instantiate different constraints over the same manual. Which reading correctly characterizes the causal structure connecting the diagnostic categories to the pharmaceutical treatments?',
    'Adversarial structured comparison across the three readings'' predictions: historical sequencing of category revisions against drug-development timelines, litigation-released industry documents, and cross-jurisdictional natural experiments (markets where a category exists but its matching drug never gained approval).',
    'If the biomedical reading is right, this story''s victim set and extractiveness dissolve into a much lower-extraction constraint; if the neurodiversity reading is right, the victim set relocates to pathologized variation and the transfer mechanism shifts from drug sales to norm enforcement. Each resolution yields a different constraint file, not a revision of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing: one reading of the DSM kernel among three.').

omega_variable(
    category_drug_causal_order,
    'Does the causal arrow run from treatments to categories (categories drafted to fit what drugs can treat) or from categories to treatments (science first, drugs developed after)?',
    'Timeline analysis of DSM-III/IV/5 category additions versus compound development and patent timelines; internal pharmaceutical strategy documents disclosed in litigation; testimony of workgroup members on drafting order.',
    'Treatment-first ordering supports this reading''s tangled_rope structure with high extraction; category-first ordering would shift weight toward the biomedical sibling and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_drug_causal_order, empirical, 'Direction of the category-drug causal arrow.').

omega_variable(
    coordination_or_cover,
    'Is the manual''s coordination function (shared language, billing, research aggregation) a genuine function the market construction rides on, or is the coordination story cover for a pure extraction arrangement?',
    'Counterfactual accounting: quantify how much clinical communication, research meta-analysis, and reimbursement administration would survive if category definitions were redrawn without regard to drug indications; survey unaffiliated clinicians on which categories they would retain.',
    'If coordination survives category-by-category without the drug linkage, the arrangement sits closer to a snare wearing a coordination costume; if the linkage is load-bearing for the coordination itself, the tangled_rope claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_cover, conceptual, 'Whether the coordination function is genuine or cover.').

omega_variable(
    patient_suppression_mechanism,
    'Is the suppression keeping long-term patients inside the medication channel structural (insurance coding gates, service eligibility) or internalized (patients'' own conviction that their distress is a chemical imbalance requiring continuous medication)?',
    'Longitudinal follow-up of patients who exit the diagnostic system through guided-tapering or alternative-service programs: if fear of relapse and medication-dependence beliefs persist after coverage barriers are removed, the internalized component is confirmed.',
    'Internalized suppression raises the constraint''s effective suppression above the structural measure and predicts persistence of the arrangement even if reimbursement gates fall; purely structural suppression would release quickly if the coding gate were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_suppression_mechanism, empirical, 'Structural versus internalized suppression of patients.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement_basis(dsm__tr_t1980, observed).
narrative_ontology:measurement(dsm__tr_t1987, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1987, 0.25).
narrative_ontology:measurement_basis(dsm__tr_t1987, observed).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1994, 0.3).
narrative_ontology:measurement_basis(dsm__tr_t1994, observed).
narrative_ontology:measurement(dsm__tr_t2001, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2001, 0.36).
narrative_ontology:measurement_basis(dsm__tr_t2001, observed).
narrative_ontology:measurement(dsm__tr_t2008, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2008, 0.42).
narrative_ontology:measurement_basis(dsm__tr_t2008, observed).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2013, 0.47).
narrative_ontology:measurement_basis(dsm__tr_t2013, observed).
narrative_ontology:measurement(dsm__tr_t2019, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2019, 0.5).
narrative_ontology:measurement_basis(dsm__tr_t2019, observed).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(dsm__tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(dsm__be_t1980, observed).
narrative_ontology:measurement(dsm__be_t1987, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1987, 0.42).
narrative_ontology:measurement_basis(dsm__be_t1987, observed).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1994, 0.5).
narrative_ontology:measurement_basis(dsm__be_t1994, observed).
narrative_ontology:measurement(dsm__be_t2001, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement_basis(dsm__be_t2001, observed).
narrative_ontology:measurement(dsm__be_t2008, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2008, 0.64).
narrative_ontology:measurement_basis(dsm__be_t2008, observed).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement_basis(dsm__be_t2013, observed).
narrative_ontology:measurement(dsm__be_t2019, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2019, 0.7).
narrative_ontology:measurement_basis(dsm__be_t2019, observed).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2025, 0.71).
narrative_ontology:measurement_basis(dsm__be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.34).
narrative_ontology:measurement_basis(dsm__su_t1980, observed).
narrative_ontology:measurement(dsm__su_t1987, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1987, 0.39).
narrative_ontology:measurement_basis(dsm__su_t1987, observed).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1994, 0.46).
narrative_ontology:measurement_basis(dsm__su_t1994, observed).
narrative_ontology:measurement(dsm__su_t2001, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement_basis(dsm__su_t2001, observed).
narrative_ontology:measurement(dsm__su_t2008, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2008, 0.57).
narrative_ontology:measurement_basis(dsm__su_t2008, observed).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement_basis(dsm__su_t2013, observed).
narrative_ontology:measurement(dsm__su_t2019, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2019, 0.61).
narrative_ontology:measurement_basis(dsm__su_t2019, observed).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2025, 0.61).
narrative_ontology:measurement_basis(dsm__su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the DSM' decomposes into three structurally distinct constraints — one per reading of dsm_taxonomy_kernel. This file (critical_psychiatry_reading) authors epsilon for the standing arrangement as a treatment-derived market-construction device (victims: chronically medicated patients; beneficiaries: manufacturers and funded leaders). The biomedical sibling authors the same manual as approximate disease-entity mapping (negligible extraction); the neurodiversity sibling authors it as pathologizing of natural variation (victim set relocated to norm-deviant individuals; transfer runs through norm enforcement rather than drug sales). The siblings are linked because each cites the others' failures: this reading uses the biomedical reading's unmet translational promises as evidence, and the neurodiversity reading inherits this reading's commercial critique. Upstream/downstream: biomedical claims lend the legitimacy this reading parasitizes; this reading's market critique supplies ammunition the neurodiversity reading deploys.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
