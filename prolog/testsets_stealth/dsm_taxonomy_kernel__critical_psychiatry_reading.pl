% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: DSM Category Architecture as Pharmaceutical Market Constructor (Critical Psychiatry Reading)
 *   domain: medical epistemology / psychiatric taxonomy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the DSM taxonomy kernel: the
 *   critical-psychiatry claim that diagnostic categories are
 *   reverse-engineered from available treatments to construct markets for
 *   psychotropic drugs. On this reading the standing arrangement under
 *   contest — the manual-plus-coding-plus-reimbursement regime as it actually
 *   operates — carries a genuine coordination function (a shared clinical
 *   vocabulary) whose category boundaries are continuously reshaped by the
 *   commercial interests that fund the evidence base. The ε referent is that
 *   standing arrangement, assessed by this reading's own lights; it is NOT
 *   the rights-respecting or reform nosology this tradition would prefer, and
 *   it is NOT averaged with the sibling readings, which are separate
 *   constraints with separate files. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope from this seat, and the metrics
 *   are authored independently as the descriptive truth this reading asserts
 *   — the engine computes per-seat classifications from the structural data,
 *   and any divergence between claim and computed type is the measurement the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - american_psychiatric_association: Agenda setter (institutional/identity_locked) — owns, revises, and defends the manual; collects licensing and publishing revenue
 *   - pharmaceutical_capital: Primary beneficiary (institutional/arbitrage) — funds the evidentiary pipeline and collects the market the categories construct
 *   - industry_funded_key_opinion_leaders: Secondary beneficiary (powerful/identity_locked) — define boundaries from inside funded workgroups and panels
 *   - frontline_clinical_psychiatrists: Payer with coordination benefit (moderate/constrained) — reimbursed only through the codes, liable under the guidelines
 *   - diagnosed_patients: Primary target (powerless/trapped) — bears overprescription, adverse effects, and lifetime record consequences
 *   - offlabel_expansion_populations: Concentrated target (powerless/trapped) — children and elders prescribed under widened boundaries
 *   - psychiatric_survivor_movements: Excluded voice (organized/trapped) — hold harm testimony the workgroup process does not admit
 *   - health_insurers_and_formulary_boards: Enforcement-adjacent beneficiary (institutional/mobile) — run daily adjudication on the codes
 *   - regulatory_agencies: Institutional observer (institutional/analytical) — approve and police the drugs the categories route
 *   - independent_critical_researchers: Analytical observer (moderate/analytical) — reconstruct genealogies and funding flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.62).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Category Architecture as Pharmaceutical Market Constructor (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical epistemology / psychiatric taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '450c5986-db01-46ef-8933-610ec694bef8').
narrative_ontology:cs_kernel_codification('450c5986-db01-46ef-8933-610ec694bef8', fixed_text).
narrative_ontology:cs_authority_grounding('450c5986-db01-46ef-8933-610ec694bef8', extraction).
narrative_ontology:cs_interpretation_layer_present('450c5986-db01-46ef-8933-610ec694bef8').
narrative_ontology:cs_reading_relation('450c5986-db01-46ef-8933-610ec694bef8', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('450c5986-db01-46ef-8933-610ec694bef8', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('450c5986-db01-46ef-8933-610ec694bef8', foundational, diagnostic_categories_commercially_constructed).
narrative_ontology:cs_axiom_status(diagnostic_categories_commercially_constructed, holdable).
narrative_ontology:cs_axiom_grounding('450c5986-db01-46ef-8933-610ec694bef8', diagnostic_categories_commercially_constructed, empirically_contingent).
narrative_ontology:cs_axiom('450c5986-db01-46ef-8933-610ec694bef8', secondary, category_expansion_net_harms_patients).
narrative_ontology:cs_axiom_status(category_expansion_net_harms_patients, holdable).
narrative_ontology:cs_axiom_grounding('450c5986-db01-46ef-8933-610ec694bef8', category_expansion_net_harms_patients, instrumental).
narrative_ontology:cs_reference_frame('450c5986-db01-46ef-8933-610ec694bef8', neutral_descriptive_nosology).
narrative_ontology:cs_drift_state('450c5986-db01-46ef-8933-610ec694bef8', contemporary_post_payment_disclosure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('450c5986-db01-46ef-8933-610ec694bef8', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, american_psychiatric_association).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_key_opinion_leaders).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, diagnosed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, offlabel_expansion_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_clinical_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, health_insurers_and_formulary_boards).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_clinical_psychiatrists).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, health_insurers_and_formulary_boards).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, operationalized_diagnostic_criteria).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, reliability_over_validity_doctrine).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, disease_specific_pharmacotherapy_model).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the workgroups that draft category definitions, owns the manual's text and revision cycle, licenses the codes on which reimbursement runs, and defends the categories in public controversy. Its organizational authority, publishing revenue, and professional standing are fused with the manual it administers; abandoning its own framework is not a live option for the organization.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, american_psychiatric_association, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, american_psychiatric_association, beneficiary).

% Funds the registration trials, advisory boards, ghostwritten publications, and continuing education through which diagnostic categories acquire their evidentiary presentation, and collects prescription revenue wherever category boundaries widen. It does not write the manual; it supplies the money and material that shape what the manual says, and it can redirect portfolios across drug classes and national markets if any single category loses profitability.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital, beneficiary,
    institutional, biographical, arbitrage, global).

% Sit on workgroups and guideline panels, author the trials and review articles that define disorder boundaries, and receive honoraria, research support, and speaker fees. Chairs, citations, and scholarly legacies are built inside the categories they help define, so public disavowal of the framework would wound their own careers and identities.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_key_opinion_leaders, beneficiary,
    powerful, biographical, identity_locked, global).

% Must attach a manual code to every encounter for the visit to be reimbursed, document to audit standards, and prescribe within category-shaped guidelines. They gain a shared shorthand, billing legitimacy, and malpractice cover, but bear the paperwork load, liability exposure, and prescribing pressure that intensify whenever boundaries expand.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_clinical_psychiatrists, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, frontline_clinical_psychiatrists, beneficiary).

% Receive diagnoses that unlock access to care and legitimize distress, but the same diagnoses follow them through medical records, insurance pricing, employment screening, custody proceedings, and disability determinations. Declining a diagnosis can mean losing treatment access; accepting one can mean long-term polypharmacy with metabolic, sexual, and cognitive adverse effects they did not bargain for.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, diagnosed_patients, payer,
    powerless, biographical, trapped, global).

% Children labeled under widened pediatric categories, adolescents, and nursing-home residents sedated under dementia-related codes are prescribed under boundaries drawn far from the populations the trials covered. Their capacity to consent, refuse, or report harm is the weakest in the system, so the adverse-effect burden concentrates here.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, offlabel_expansion_populations, payer,
    powerless, biographical, trapped, global).

% Service-user and survivor coalitions that document coercive treatment, withdrawal harm, and diagnostic inflation, and campaign against forced outpatient commitment and category creep. They hold collective testimony the workgroup process does not formally admit, and because their histories are recorded in coded charts, the categories follow them even when they reject the labels.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_survivor_movements, excluded,
    organized, generational, trapped, global).

% Adjudicate every claim against manual codes and steer prescribing through formularies and prior authorization. They gain standardized claims processing and cost-shifting levers, and they pay for the medications the categories route, recovering the outlay through premiums. They enforce the coding regime daily without having authored it.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, health_insurers_and_formulary_boards, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, health_insurers_and_formulary_boards, payer).

% Approve drugs for category-defined populations, police promotional claims, and monitor post-marketing harm signals. They take testimony from the other seats and can compel label changes, boxed warnings, or new trials, actions that alter how categories translate into prescriptions without touching the manual itself.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% Historians, philosophers, social scientists, and heterodox clinicians who reconstruct category genealogies and trace funding flows through disclosed payments and archived correspondence. They publish outside the funded pipeline, hold no lever over the manual, and supply the documentary record the other seats argue with.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, independent_critical_researchers, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared diagnostic vocabulary lets clinicians, researchers, insurers, courts, and registries describe the same patient the same way: it standardizes communication, aggregates research subjects, adjudicates reimbursement, and counts epidemiology. Whatever else it does, the manual solves a real common-language problem.
% TRANSFER_FUNCTION: Moves money — prescription revenue, consulting honoraria, continuing-education funding, licensing income — from patients, public payers, and taxpayers toward manufacturers, funded opinion leaders, and the manual's publisher. It also moves diagnostic authority away from patients' accounts of their own lives and toward the institutions that hold the category definitions.
% ABSENT_VOICES: Psychiatric survivors and service users, family members reporting adverse effects, practitioners of non-Western healing traditions, and critical psychologists outside the workgroup process would all object to how boundaries are drawn; they are absent because workgroup seats, trial funding, and guideline panels are allocated inside the professional-industrial circuit, not because they have nothing at stake.
% DISAPPEARANCE_RATIONALE: If the coding regime vanished overnight, reimbursement would halt until a replacement vocabulary existed, running trials would lose their enrollment criteria, disability and forensic proceedings would lose their evidentiary anchor, and decades of epidemiological series would break. Because the coordination demand is real, a successor nosology would be rebuilt quickly — which is precisely the rope-half of the structure.
% FOUNDING_PROBLEM: Before the 1980 operational revolution, psychiatric diagnosis was unreliable: different clinicians gave the same patient different labels, which wrecked research replication, made treatment evaluation impossible, and gave insurers grounds to deny claims.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated from outside the benefiting parties: archival studies of the DSM-III feasibility project (Kirk and Kutchins), the methodological writings of the revision's own architect, and the later public testimony of the DSM-IV chair that the reliability project succeeded and was then repurposed. No one outside this reading's proponents corroborates the stronger claim that market construction was the founding intent; that claim rests on funding-flow analyses and leaked industry documents, and is precisely what the omega variables hold open.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.68 (moderate-high per the expected structural delta): category expansions repeatedly preceded or accompanied blockbusters (social anxiety disorder, pediatric bipolar, adult ADHD broadening), and the revenue accrues to seats that did not bear the trial risks. Suppression is 0.62 as a RAW structural property — unscaled by power or scope: licensure, reimbursement rules, and the legal weight of a coded diagnosis close exits, though rival nosologies (ICD chapters, RDoC, HiTOP, the Power Threat Meaning Framework) remain nominally available, which is why suppression stops well short of a snare's ceiling. Theater ratio 0.45: a large share of boundary-maintenance activity — advisory boards styled as science, continuing education functioning as detailing, key-opinion lectureships — performs objectivity while transmitting commercial framing, but the underlying reliability machinery is real work, so the ratio sits below the piton threshold. Accessibility collapse 0.60: once the funding architecture is visible, the alternative of practicing outside the coded economy collapses for most clinicians, yet conceptual alternatives persist in research and in a few health systems. Resistance 0.55: survivor movements, whistleblower litigation, disclosure regulation, and internal dissent (including from the framework's own former stewards) impose real costs on the arrangement. The three series share one seven-point grid (edition milestones 1980–2026); every metric is authored at every point, so no scalar substitution contaminates early rows. The trajectories rise monotonically through the direct-to-consumer advertising era, then ease slightly after 2013 as payment-disclosure databases and open-science norms raised the cost of overt capture — a plateau, not a reversal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter and beneficiary seats (association, manufacturers, funded leaders), the arrangement presents as coordination they built, fund, and staff — a rope experienced from inside. From the trapped payer seats (patients, off-label populations), the same structure operates as enforced extraction with no exit — snare-flavored. Frontline psychiatrists sit between: billed into compliance, compensated by the same channel. The engine derives these per-seat classifications from the power, exit, and role data above; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the association, manufacturers, and funded leaders sit near the beneficiary end (low d, damped or inverted χ), with the association's identity_locked exit reinforcing subsidy rather than target position. Victims — diagnosed patients and off-label expansion populations — sit near the full-target end, amplified by trapped exit and global scope, which raises verification difficulty and effective extraction. Frontline psychiatrists derive a middling-to-high d from their payer role, moderated by their secondary beneficiary position. Insurers derive low d from their beneficiary role despite paying claims, which is descriptively fair: they pass drug costs to premiums and are not the arrangement's targets. No directionality overrides were needed — the beneficiary/victim plus exit data produce the right relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the fact that even the harshest critics rely on the manual's coordination function daily; classifying it as rope would erase the documented capture channels through which category boundaries track portfolio strategy. The founding problem — inter-rater reliability — was substantially solved within a decade of the 1980 operational turn, and the arrangement's persistence now rides on functions (market construction, authority maintenance) beyond that founding mandate; but because the coordination function itself remains live and load-bearing, the mandate has mutated rather than died, so mandatrophy_resolved is deliberately NOT declared. Identity-lock dynamics matter at two seats: the association has institutionally become its manual (organizational identity fusion), and funded leaders face career path dependence — if either identity frame broke (a decisive scandal, a defection cascade among workgroup alumni), the enforcement cost of maintaining current boundaries would spike and the structure would slide toward scaffold-or-piton territory pending replacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of kernel dsm_taxonomy_kernel (reading: critical_psychiatry_reading). How would instantiating the biomedical_reading or the neurodiversity_reading instead change the structural facts — victim set, ε, and computed type — over the same standing arrangement?',
    'Cross-reading comparison of the sibling story files: hold the referent (the operating manual-plus-coding regime) fixed and compare each reading''s authored beneficiary/victim structure and ε.',
    'Under the biomedical reading the victim set empties, ε collapses toward negligible, and the arrangement approaches a mountain-like settled classification; under the neurodiversity reading the victim set expands to variation-conflicting populations and extraction re-keys to norm enforcement. The classification of THIS file is conditional on its reading, not on the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Reading-indexed divergence across the DSM kernel''s three declared readings.').

omega_variable(
    reverse_engineering_design_intent,
    'Does the documentary record establish deliberate reverse-engineering of categories from treatment portfolios, or convergent evolution in which drug development and category definition mutually influenced each other without design intent?',
    'Archival analysis of workgroup correspondence, trial-protocol timing against criteria drafts, and disclosed payment networks; natural experiments where a category was proposed without a sponsoring compound.',
    'Deliberate design intent supports the full extractiveness authored here; demonstrated convergence would lower ε toward the coordination-cost floor and push the computed type toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_engineering_design_intent, empirical, 'Whether category boundaries were designed to fit treatments or evolved alongside them.').

omega_variable(
    coordination_extraction_separability,
    'Is the manual''s coordination function separable from its commercial shaping — could a shared vocabulary persist if category boundaries were insulated from funding interests?',
    'Track outcomes in settings using alternative nosologies (research domains, dimensional taxonomies, formulation-based services): if communication, research aggregation, and care continuity hold while commercial shaping is removed, the functions are separable.',
    'If separable, the measured extraction is rent riding on a real coordination function (pure tangled-rope confirmation); if inseparable, part of ε is the irreducible price of any shared classification and the excess shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components of the taxonomy are structurally separable.').

omega_variable(
    clinician_conformity_mechanism,
    'Is clinician conformity to the coded economy structural (reimbursement compulsion, audit regimes, licensure) or internalized (professional formation that makes the categories feel like perception itself)?',
    'Post-reform conformity trajectory: if practices persist unchanged where payment rules stop requiring codes, the residual is internalized; if practice tracks the incentive surface, it is structural.',
    'If substantially internalized, effective suppression exceeds the structural measure and survives formal reform — remedies aimed at incentives alone would fail; if structural, payment-rule reform suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clinician_conformity_mechanism, empirical, 'Structural versus internalized mechanism of professional conformity to the coding regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 1980, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(dsm__tr_t1987, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1987, 0.27).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 1994, 0.35).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2013, 0.48).
narrative_ontology:measurement(dsm__tr_t2022, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2022, 0.46).
narrative_ontology:measurement(dsm__tr_t2026, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 2026, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(dsm__be_t1987, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1987, 0.44).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 1994, 0.53).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2000, 0.59).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2013, 0.67).
narrative_ontology:measurement(dsm__be_t2022, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2022, 0.69).
narrative_ontology:measurement(dsm__be_t2026, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(dsm__su_t1987, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1987, 0.47).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2013, 0.66).
narrative_ontology:measurement(dsm__su_t2022, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2022, 0.64).
narrative_ontology:measurement(dsm__su_t2026, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, neurodiversity_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the ε-invariance principle: the colloquial label 'what the DSM is' covers three structurally distinct claims that cannot share one ε. This file (critical_psychiatry_reading) authors the market-construction claim with moderate-high ε and a patient victim set; biomedical_reading authors the disease-entity claim with negligible ε and no victim set; neurodiversity_reading authors the pathologization-of-variation claim with a norm-conflict victim set. Each story carries its own claimed_type, metrics, and stakeholders; the family edges here record that the upstream biomedical claim is routinely cited as legitimating cover by the arrangement this reading indicts, and that the neurodiversity critique draws on the same funding-flow evidence base. Linkage is declarative kinship, not endorsement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
