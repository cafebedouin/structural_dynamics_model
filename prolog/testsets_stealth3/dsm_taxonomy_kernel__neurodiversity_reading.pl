% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Categories as Conformity-Enforcing Taxonomy (Neurodiversity Reading)
 *   domain: medical epistemology/psychiatric taxonomy/social construction of illness
 *
 * SUMMARY:
 *   The DSM's diagnostic categories, on the neurodiversity reading, function
 *   less as discoveries of discrete illnesses than as an institutional
 *   sorting technology: behavioral variation that schools, employers, and
 *   courts find costly gets routed through clinical evaluation and returned
 *   labeled, treated, and adjusted to standard environments. The same code
 *   set that gates insurance payment and special-education services also
 *   supplies the legitimacy for behavior plans, medication regimes,
 *   competency determinations, and guardianship. This story instantiates the
 *   neurodiversity_reading of the dsm_taxonomy_kernel and therefore authors
 *   epsilon over the standing classification-and-treatment arrangement as
 *   that reading sees it: pathologization itself figures as harm, and the
 *   arrangement's beneficiaries are the conformity-requiring institutions,
 *   not the diagnosed. Sibling readings of the same kernel are separate
 *   constraints with different epsilon: the biomedical_reading authors low
 *   extraction over the same arrangement (legitimate disease detection), and
 *   the critical_psychiatry_reading authors high extraction with a
 *   pharmaceutical-market beneficiary structure; all three are linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   diagnosed_neurodivergent_children: primary target (powerless/trapped) —
 *   evaluated, labeled, and normalized without consent -
 *   adult_neurodivergent_individuals: primary target
 *   (moderate/identity_locked) — bear the category in employment, benefits,
 *   and self-concept - neurodiversity_self_advocacy_movement: organized payer
 *   seat (organized/identity_locked) — resists the frame while depending on
 *   it for access - american_psychiatric_association: agenda setter
 *   (institutional/arbitrage) — owns and revises the kernel -
 *   psychiatric_profession: street-level administrator and collector
 *   (organized/constrained) - public_school_systems,
 *   conformity_oriented_employers, courts_and_correctional_institutions,
 *   pharmaceutical_manufacturers: beneficiary seats drawing order,
 *   predictable labor, administrable populations, and prescribable markets
 *   from the categories - families_of_diagnosed_children: dual-positioned —
 *   buy access with the label, pay in stigma and regimen -
 *   label_refusing_support_seekers: excluded voice — needs the services,
 *   refuses the terms - disability_bioethicists: analytical observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.76).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.75).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Categories as Conformity-Enforcing Taxonomy (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical epistemology/psychiatric taxonomy/social construction of illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'fbc64c20-2e4a-461e-a99c-27899ef5a44c').
narrative_ontology:cs_kernel_codification('fbc64c20-2e4a-461e-a99c-27899ef5a44c', formalized).
narrative_ontology:cs_authority_grounding('fbc64c20-2e4a-461e-a99c-27899ef5a44c', expertise).
narrative_ontology:cs_interpretation_layer_present('fbc64c20-2e4a-461e-a99c-27899ef5a44c').
narrative_ontology:cs_reading_relation('fbc64c20-2e4a-461e-a99c-27899ef5a44c', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('fbc64c20-2e4a-461e-a99c-27899ef5a44c', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('fbc64c20-2e4a-461e-a99c-27899ef5a44c', foundational, neurological_difference_is_not_pathology).
narrative_ontology:cs_axiom_status(neurological_difference_is_not_pathology, holdable).
narrative_ontology:cs_axiom_grounding('fbc64c20-2e4a-461e-a99c-27899ef5a44c', neurological_difference_is_not_pathology, deontological).
narrative_ontology:cs_axiom('fbc64c20-2e4a-461e-a99c-27899ef5a44c', secondary, accommodation_over_normalization).
narrative_ontology:cs_axiom_status(accommodation_over_normalization, holdable).
narrative_ontology:cs_axiom_grounding('fbc64c20-2e4a-461e-a99c-27899ef5a44c', accommodation_over_normalization, instrumental).
narrative_ontology:cs_reference_frame('fbc64c20-2e4a-461e-a99c-27899ef5a44c', descriptive_variation_catalog).
narrative_ontology:cs_drift_state('fbc64c20-2e4a-461e-a99c-27899ef5a44c', contemporary_neurodiversity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbc64c20-2e4a-461e-a99c-27899ef5a44c', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, conformity_oriented_employers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, courts_and_correctional_institutions).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, diagnosed_neurodivergent_children).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, adult_neurodivergent_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, families_of_diagnosed_children).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_self_advocacy_movement).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, families_of_diagnosed_children).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes and periodically revises the diagnostic manual, convenes the workgroups that write criterion sets, and controls which proposed categories and threshold changes reach print. Insurers, courts, schools, and clinicians treat its text as authoritative. It collects publishing revenue from the manual and related products, and its revision process admits invited advisors, including growing but limited patient-advocate participation.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, american_psychiatric_association, agenda_setter,
    institutional, generational, arbitrage, global).

% Licensed psychiatrists, psychologists, and allied clinicians assign the categories in daily practice. A diagnosis is the ticket without which insurance will not reimburse treatment, schools will not open a service plan, and courts will not entertain certain determinations, so clinical judgment operates inside a fee-for-code structure. Many clinicians privately regard the labels as rough descriptors; their livelihoods and legal authority nonetheless run through the manual, and they perform the case-level gatekeeping the system requires.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession, agenda_setter).

% Districts must educate everyone assigned to them and are evaluated on attendance, test scores, and classroom order. A behavioral diagnosis channels a disruptive student toward medicated compliance, a separate placement, or an aide, each of which restores classroom manageability. The same diagnosis legally obligates the district to fund evaluations, services, and due-process protections under special-education law, so districts both draw on and pay into the diagnostic channel.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems, payer).

% Large employers staff standardized schedules, open-plan offices, and customer-facing scripts. Fitness-for-duty examinations, accommodation-process paperwork, and short-term-disability classifications all run on psychiatric categories, letting human-resources functions convert cognitive mismatch into a managed medical file. Workers whose attention, sensory tolerance, or social style departs from the standard workflow are processed as individual medical cases rather than as design feedback about the workplace.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, conformity_oriented_employers, beneficiary,
    powerful, biographical, constrained, global).

% Courts use psychiatric categories for competency to stand trial, civil commitment, insanity defenses, and guardianship; prisons rely on them to sort and medicate large incarcerated populations. A shared diagnostic vocabulary renders unpredictable people administrable — assignable to housing units, medication protocols, and release criteria. Judges and corrections officials have no alternative classification system with equivalent legal standing.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, courts_and_correctional_institutions, beneficiary,
    institutional, generational, trapped, national).

% Manufacturers develop and market medications indicated for manual categories — stimulants for attention diagnoses, antipsychotics for irritability in autism, antidepressants across the mood and anxiety sets. Each category that reaches the manual defines a prescribable population; indications, dosing guidance, and sales detailing all cite the criteria. Revenue follows the diagnosed population, and portfolio strategy anticipates which new categories will open markets.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Children flagged by teachers or parents undergo school or clinic evaluation and emerge with a category attached to their record. The category unlocks services — therapy sessions, classroom aides, insurance-covered treatment — and simultaneously subjects them to behavior plans with compliance targets, sometimes daily medication, and a file that follows them through schooling. They cannot consent to or decline the process; parents and professionals decide, and parental refusal can trigger neglect proceedings in some jurisdictions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, diagnosed_neurodivergent_children, payer,
    powerless, biographical, trapped, national).

% Adults carrying a diagnosis navigate workplaces, benefits systems, and healthcare with the category on file. Disclosure can unlock accommodations and disability protections; nondisclosure leaves them unsupported; disclosure invites stigma, promotion filtering, and legal-capacity questioning. Many describe the category as both the key to support and a ceiling on how they are read — and after years of community life built around the label, declining it would mean losing accommodations, community, and a self-description that finally fit.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, adult_neurodivergent_individuals, payer,
    moderate, biographical, identity_locked, national).

% Organized networks of autistic and otherwise neurodivergent adults run advocacy organizations, testify to legislatures, protest at psychiatric conventions, and publish alternative framings of their own traits. Members typically hold diagnoses themselves — the movement's access to services, legal standing, and even its identity vocabulary runs through the categories it contests. It has won language changes in some professional bodies and seats in some guideline processes while the billing and eligibility infrastructure stayed intact.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_self_advocacy_movement, payer,
    organized, generational, identity_locked, continental).

% Parents pursue evaluation to unlock services their child is otherwise denied, then live inside the category's consequences: insurance battles, treatment regimens, school meetings, and social stigma attaching to the family. Some become fierce defenders of the diagnostic pathway that delivered their child's aide; others become its sharpest critics after watching behavioral programs distress their child. Their position mixes relief at access with grief and burden.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, families_of_diagnosed_children, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, families_of_diagnosed_children, payer).

% People with pronounced cognitive, sensory, or attentional differences who conclude the available categories misdescribe them — or who refuse the pathology framing on principle — find that support systems have no door for them: no code, no reimbursement, no legal accommodation. They remain outside the conversations that assign categories, though the categories govern whether they eat steadily, keep housing, or hold work.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, label_refusing_support_seekers, excluded,
    powerless, biographical, trapped, national).

% Academic philosophers and bioethicists study how the categories distribute authority, how capacity and guardianship determinations are made, and what justice requires for cognitively atypical people. They publish critiques and sit on ethics boards; they hold no vote in the manual's workgroups and no client caseload, and their analyses circulate mainly among peers.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, disability_bioethicists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, diffuse).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, operationalized vocabulary that lets strangers coordinate care decisions: insurers can specify covered conditions, schools can define service-eligibility rules, courts can attach procedures to statuses, researchers can assemble comparable cohorts, and clinicians can hand off cases with a common descriptor. Whatever its accuracy, the code set solves a real who-gets-what-by-what-rule problem at scale.
% TRANSFER_FUNCTION: Moves behavioral compliance and self-determination from diagnosed individuals to the institutions that process them — classrooms get order, workplaces get predictable workflows, courts get administrable populations. Alongside, it moves money: premiums and public funds flow through diagnostic codes to hospitals, clinics, and drug manufacturers, and definitional authority flows to the professions that write the criteria.
% ABSENT_VOICES: Label-refusing support seekers stand outside the room: they need the services the codes gate but reject the categories' terms, and no procedural slot exists for them. Severely disabled autistic people who cannot speak for themselves are represented by parents and professionals whose interests partly diverge from theirs. Historically, diagnosed people held no voting seats in manual workgroups; advocate participation arrived late, invited and bounded.
% DISAPPEARANCE_RATIONALE: Insurance claims for mental-health care would become unprocessable overnight; special-education eligibility would lose its legal criteria; courts would lose the vocabulary for commitment, competency, and guardianship; drug indications would lose their target populations; and millions of people holding services, accommodations, and identities keyed to categories would need a replacement classification before anything worked again.
% FOUNDING_PROBLEM: Through the 1970s, American psychiatric diagnosis was notoriously unreliable — two clinicians assessing the same patient frequently disagreed on category, undermining research replication, treatment evaluation, insurance reimbursement, and professional credibility. The manual's modern editions were built to fix that: operational criteria, explicit thresholds, shared categories.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the DSM-III revolution corroborate the reliability mandate from archival sources; the National Institute of Mental Health publicly declared in 2013 — from outside the professional body — that the categories lack the validity needed for research and redirected funds to an alternative framework; insurance actuaries attest the codes remain indispensable for payment. Whether the founding problem is solved (reliability largely achieved) or transformed into an unresolved validity problem is disputed between the professional body and its outside critics.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.76) because the reading counts pathologization itself — the conversion of trait into disorder — plus coercive normalization and denied self-determination as the arrangement's operating cost to the diagnosed, layered on top of the financial flows the codes route. Suppression (0.75) records the enforcement machinery: mandatory-evaluation pathways, insurance gating that makes the label the only door to help, behavior plans with compliance targets, medication administered to minors, and legal-capacity determinations; suppression is authored as a raw structural property and is deliberately not scaled — the engine applies directionality and scope amplification downstream. Theater (0.40) reflects a growing performative layer — destigmatization campaigns, advisory panels, and affirming language that leave the billing and eligibility infrastructure untouched — atop machinery that remains functional at what this reading takes to be its real work. Accessibility collapse sits mid-range (0.50): alternatives exist (rival coding schemes, research frameworks outside the manual, affirming-practice models, self-identification communities) but none carries equivalent legal or reimbursement standing, so exit from the categorical system is costly rather than impossible. Resistance (0.65) is substantial and organized. The measurement series share one six-point grid (1980-2025) across all three tracked metrics; the rising base_extractiveness series is authored as real accumulation — categories proliferated, enforcement scaled, and rent layers accreted — and is expected to trip the extraction-accumulation hypothesis for investigation. Enforcement capacity demonstrably built over the interval (state coverage mandates, school-system integration of evaluation, telehealth scaling), which is why suppression_requirement is tracked rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute sharply different types from identical structural facts. A diagnosed child's seat — powerless, trapped, near-full-target directionality — should compute as enforced extraction with no meaningful exit; an adult's seat adds identity lock, which raises effective extraction further while making exit self-destructive; the movement's seat damps extraction through organization but stays a payer. The school, employer, court, and manufacturer seats sit at the beneficiary pole and should compute subsidy or cheap coordination. The association's seat sees governance of an instrument it revises. The engine derives these per-seat classifications from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (schools, employers, courts, manufacturers, the profession) drive those seats toward the beneficiary pole; victim declarations (diagnosed children, diagnosed adults) drive those seats toward the target pole, with trapped and identity_locked exits pushing children and adults respectively toward full-target. No directionality overrides are authored: the override surface keys on power atoms, and this story's atoms are internally heterogeneous (three different true positions sit at 'organized'; two at 'moderate'), so any per-atom override would flatten real differences the structural declarations already encode. Families of diagnosed children are deliberately left out of the beneficiary array — their position is genuinely mixed (access bought with stigma) and is carried in their stakeholder situation and secondary role rather than forced into a one-directional declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading this arrangement as pure extraction would erase the coordination its own targets rely on — the code set really does route services, and self-advocates use diagnoses strategically as access keys; reading it as pure coordination would erase the asymmetry — the same structure that delivers an aide also delivers the behavior plan, the medication, and the file. The tangled-rope claim keeps both halves visible. On genealogy: the founding problem (diagnostic unreliability) was substantially addressed, and the arrangement persisted and grew by taking on conformity-enforcement work the founding problem never named; founding_problem_status is authored contested because the professional body recast the mandate as a live validity project while outside bodies (the federal research agency's framework switch, outcome researchers) treat the original warrant as spent. The classification prevents mandatrophy mislabeling in both directions: it blocks the charitable reading in which a solved problem explains continued existence, and the cynical reading in which no real function ever existed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading — neurodiversity_reading — of the dsm_taxonomy_kernel; what would adopting a sibling reading change structurally?',
    'Compare compiled classifications across the three sibling stories: the biomedical reading (categories as discovered disease entities), the critical-psychiatry reading (categories reverse-engineered from drug markets), and this reading (categories enforcing institutional conformity).',
    'Biomedical adoption would shrink or empty this reading''s victim set and drop measured extraction toward coordination cost; critical-psychiatry adoption would relocate the primary beneficiary seat to pharmaceutical manufacturers and re-key the transfer function to market construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the DSM kernel; disagreement located in what the categories are and whom they serve.').

omega_variable(
    sibling_disagreement_location,
    'Where exactly do the three readings locate their disagreement — in the ontology of the categories (discovered entities versus constructed instruments) or in the valence of pathologization (harm versus legitimate detection)?',
    'Structural comparison of the sibling stories'' victim and beneficiary sets: ontology disputes flip beneficiary identity; valence-only disputes flip the sign and magnitude of epsilon while leaving seats intact.',
    'If the dispute is ontological, no amount of outcome data reconciles the readings and the family stays permanently split; if it is valence-only, shared evidence on treatment and accommodation outcomes could converge the sibling epsilon values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_disagreement_location, conceptual, 'Locates the axis along which sibling readings of the DSM kernel diverge.').

omega_variable(
    pathology_vs_environmental_mismatch,
    'Is the distress and dysfunction recorded in the categorized conditions intrinsic to the neurology, or produced by environments built to a narrow behavioral standard?',
    'Longitudinal and cross-environment studies comparing outcomes for the same neurotype under accommodated versus unaccommodated conditions; developmental research on demand-induced phenotypes.',
    'If most recorded dysfunction is mismatch-produced, the categories measure institutional fit rather than individual disorder and this reading''s high extraction estimate is conservative; if substantial dysfunction is intrinsic, part of the classification burden is legitimate medical response and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathology_vs_environmental_mismatch, empirical, 'Whether the harm attributed to pathologization is intrinsic to the neurology or produced by environmental mismatch.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of neurodivergent resistance structural (insurance gating, legal compulsion, school mandates) or internalized (self-stigma and masking habits that persist after barriers lift)?',
    'Post-exit trajectory studies: whether self-advocacy capacity and help-seeking recover when coercive structures are removed, for example in affirming-practice clinics or after guardianship reform.',
    'If internalized components dominate, effective suppression exceeds the structural measure and persists after institutional reform; classification consequences ride on the structural share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression into structural and internalized mechanisms.').

omega_variable(
    payer_coalition_capacity,
    'Can the organized self-advocacy seat convert class-wide payer position into binding revision power over the taxonomy (voting seats, criterion vetoes), or does consultative inclusion absorb resistance without transferring authority?',
    'Track advocacy outcomes across successive revision cycles and guideline processes: seats gained, proposals adopted versus merely acknowledged, and whether billing or eligibility infrastructure changed.',
    'If coalition power binds, the payer seat''s effective extraction falls and the arrangement drifts toward negotiated coordination; if inclusion is absorptive, extraction persists behind participatory performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(payer_coalition_capacity, empirical, 'Whether organized neurodivergent advocacy can bind the taxonomy''s administrators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(dsm__tr_t2017, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2017, 0.34).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1990, 0.61).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(dsm__be_t2017, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2017, 0.73).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(dsm__su_t2017, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2017, 0.73).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, resource_allocation).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the DSM' decomposes into three structurally distinct claims about the same category set. This (neurodiversity) reading authors high epsilon with conformity-institution beneficiaries; the biomedical sibling authors low epsilon over the same arrangement; the critical-psychiatry sibling authors high epsilon with a pharmaceutical beneficiary structure. The upstream sibling (biomedical) supplies the legitimacy the other two contest; family edges run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
