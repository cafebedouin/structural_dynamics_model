% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: DSM Taxonomy — Neurodiversity Reading: Pathologization of Neurological Variation
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint story captures the neurodiversity reading of the DSM
 *   taxonomy kernel: the claim that DSM categories pathologize natural human
 *   neurological variation (autism, ADHD, dyslexia, Tourette's, etc.) because
 *   that variation conflicts with institutional behavioral norms —
 *   neuronormative expectations for attention, sociality, communication, and
 *   emotional regulation that schools, workplaces, and carceral systems
 *   require. The constraint operates by converting neurological difference
 *   into medical disorder, which then legitimates coercive normalization
 *   (behavioral interventions, social skills training, medication for
 *   compliance) and denies accommodation (refusing to restructure
 *   environments). The victim set is neurodivergent individuals subjected to
 *   these harms; the beneficiary set is institutional systems that extract
 *   conformity and predictability. This reading is one of three declared
 *   readings of the dsm_taxonomy_kernel — the biomedical reading and critical
 *   psychiatry reading instantiate separate constraints with different ε
 *   referents, victim sets, and beneficiary structures.
 *
 * KEY AGENTS:
 *   - neurodivergent_individuals: Primary victims (powerless/identity_locked) — bear pathologization, coercive normalization, denied accommodation
 *   - educational_institutions: Primary beneficiaries (institutional/arbitrage) — extract compliance, standardized throughput, funding tied to diagnosis
 *   - employer_systems: Beneficiaries (institutional/arbitrage) — extract neuronormative productivity, avoid accommodation costs
 *   - carceral_systems: Beneficiaries (institutional/trapped for subjects) — extract behavioral compliance, pathologize non-conformity as danger
 *   - psychiatric_profession: Agenda setters (institutional/constrained) — administer the taxonomy, gatekeep diagnosis, split between biomedical and neurodiversity-affirming frames
 *   - pharmaceutical_industry: Beneficiaries (powerful/arbitrage) — extract market creation via diagnostic expansion (sibling reading's primary beneficiary; present here as secondary)
 *   - insurance_reimbursement_systems: Beneficiaries (institutional/arbitrage) — require DSM codes for payment, entrenching the taxonomy
 *   - neurodiversity_advocates: Observers/Excluded (organized/constrained) — contest the constraint from outside institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.82).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy — Neurodiversity Reading: Pathologization of Neurological Variation").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '510080ad-aa73-4f9b-ad25-a52eb2b263d1').
narrative_ontology:cs_kernel_codification('510080ad-aa73-4f9b-ad25-a52eb2b263d1', formalized).
narrative_ontology:cs_authority_grounding('510080ad-aa73-4f9b-ad25-a52eb2b263d1', extraction).
narrative_ontology:cs_interpretation_layer_present('510080ad-aa73-4f9b-ad25-a52eb2b263d1').
narrative_ontology:cs_reading_relation('510080ad-aa73-4f9b-ad25-a52eb2b263d1', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('510080ad-aa73-4f9b-ad25-a52eb2b263d1', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('510080ad-aa73-4f9b-ad25-a52eb2b263d1', foundational, neurological_variation_is_natural_not_pathological).
narrative_ontology:cs_axiom_status(neurological_variation_is_natural_not_pathological, holdable).
narrative_ontology:cs_axiom_grounding('510080ad-aa73-4f9b-ad25-a52eb2b263d1', neurological_variation_is_natural_not_pathological, deontological).
narrative_ontology:cs_axiom('510080ad-aa73-4f9b-ad25-a52eb2b263d1', foundational, pathologization_of_difference_is_epistemic_injustice).
narrative_ontology:cs_axiom_status(pathologization_of_difference_is_epistemic_injustice, holdable).
narrative_ontology:cs_axiom_grounding('510080ad-aa73-4f9b-ad25-a52eb2b263d1', pathologization_of_difference_is_epistemic_injustice, deontological).
narrative_ontology:cs_axiom('510080ad-aa73-4f9b-ad25-a52eb2b263d1', secondary, accommodation_not_normalization_is_just_response).
narrative_ontology:cs_axiom_status(accommodation_not_normalization_is_just_response, holdable).
narrative_ontology:cs_axiom_grounding('510080ad-aa73-4f9b-ad25-a52eb2b263d1', accommodation_not_normalization_is_just_response, instrumental).
narrative_ontology:cs_reference_frame('510080ad-aa73-4f9b-ad25-a52eb2b263d1', neuronormative_institutional_order).
narrative_ontology:cs_drift_state('510080ad-aa73-4f9b-ad25-a52eb2b263d1', contemporary_neurodiversity_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('510080ad-aa73-4f9b-ad25-a52eb2b263d1', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, employer_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, carceral_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, insurance_reimbursement_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, autistic_adults).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, adhd_adults).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_children_in_schools).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, institutionalized_neurodivergent_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience their neurological variation (autism, ADHD, dyslexia, Tourette's, etc.) as natural difference, not disorder. Bear pathologization: diagnostic labels that follow them through education, employment, healthcare, and legal systems. Subject to coercive normalization (ABA, social skills training, medication for compliance) and denied accommodation (refusal to restructure sensory, social, cognitive environments). Exit is identity_locked — they cannot cease being neurodivergent; masking extracts unsustainable cognitive cost. The DSM taxonomy is the gatekeeper: without a diagnosis, no accommodations; with a diagnosis, pathologization and its consequences.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, global).

% Specific neurodivergent population targeted by DSM-5 'autism spectrum disorder' category. Experience childhood behavioral interventions targeting 'normalization' of stimming, eye contact, social performance. As adults, face employment discrimination, denied workplace accommodations, guardianship proceedings, and carceral system targeting. The DSM category is the legal and institutional basis for both support access and rights removal — a double bind.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, autistic_adults, payer,
    powerless, biographical, identity_locked, global).

% Targeted by DSM 'ADHD' category. Childhood experience: stimulant medication for classroom compliance, behavioral interventions targeting 'executive function deficits' framed as pathology. Adult experience: workplace discrimination, denied accommodations (flexible scheduling, written instructions, sensory management), gatekept medication access via DSM diagnosis requirement. The taxonomy frames attention variation as deficit; institutions extract neuronormative productivity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, adhd_adults, payer,
    powerless, biographical, identity_locked, global).

% Children in K-12 education systems where DSM diagnosis is the mandatory gateway to IEP/504 accommodations. Experience coercive normalization: behavioral plans targeting 'appropriate' social behavior, suppression of stimming, forced eye contact, compliance training. Denied genuine accommodation (sensory spaces, alternative communication, interest-based learning). Exit is trapped — compulsory attendance, no alternative schools, parental advocacy capacity varies.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_children_in_schools, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_children_in_schools, payer).

% Neurodivergent people in psychiatric hospitals, group homes, prisons, immigration detention. DSM diagnosis is the basis for involuntary commitment, forced medication, behavioral control programs, solitary confinement for 'behavioral issues.' Exit is trapped — physical confinement compounds the taxonomy's epistemic violence. Highest suppression intensity of any seat.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutionalized_neurodivergent_people, payer,
    powerless, biographical, trapped, national).

% School districts, state education departments, special education systems. Use DSM categories to: gatekeep special education funding (IDEA requires disability categories), standardize behavioral interventions (PBIS, ABA), enforce neuronormative developmental benchmarks, manage classroom compliance. Extract: predictable student throughput, federal funding tied to diagnosis counts, legal protection from discrimination claims via 'individualized' plans that target normalization. Could adopt alternative frameworks (neurodiversity-affirming IEPs, universal design) but DSM is the entrenched legal/financial infrastructure.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, educational_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Corporate HR, workplace disability systems, occupational health. Use DSM categories to: gatekeep ADA accommodations (requiring 'substantial limitation' framed by DSM criteria), enforce neuronormative performance metrics (attention, sociality, processing speed), deny neurodivergent hiring/promotion via 'culture fit.' Extract: predictable neuronormative workforce, avoided accommodation costs, legal defensibility. Could adopt neurodiversity hiring programs, flexible work design — but DSM-based compliance is the default.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, employer_systems, beneficiary,
    institutional, generational, arbitrage, global).

% Prisons, jails, juvenile detention, immigration detention, forensic psychiatric hospitals. Use DSM categories to: classify incarcerated people for housing/treatment, justify solitary confinement for 'behavioral non-compliance' (often neurodivergent distress), mandate 'treatment' programs targeting neuronormative behavior, deny disability accommodations. Extract: behavioral control, warehouse management, legal immunity via 'medical necessity.' Exit is arbitrage for the institution; trapped for the neurodivergent incarcerated person.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, carceral_systems, beneficiary,
    institutional, generational, arbitrage, national).

% APA, DSM workgroups, clinical practitioners, academic psychiatry. Administer the taxonomy: write/revise criteria, gatekeep diagnosis, train clinicians, set practice guidelines. Split between biomedical frame (categories = disease entities) and neurodiversity-affirming frame (categories = access tickets). Benefit from professional authority, insurance reimbursement tied to DSM codes, pharmaceutical research funding. Constrained by: neurodiversity movement critique, critical psychiatry, insurance/legal mandates, internal dissent. Could revise taxonomy toward neurodiversity paradigm — but institutional inertia, financial ties, and professional identity resist.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_profession, agenda_setter,
    institutional, generational, constrained, global).

% Pharma companies producing psychotropics (stimulants, antipsychotics, antidepressants). Benefit from DSM diagnostic expansion (new markets, pediatric indications, maintenance prescribing). Fund DSM workgroups, CME, patient advocacy groups. Primary beneficiary in critical_psychiatry_reading; secondary here (pathologization enables markets but this reading's extraction is pathologization itself, not drug sales). Exit is arbitrage — can pivot to other disease areas, but DSM is the regulatory gateway.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Private insurers, Medicaid, Medicare. Require DSM codes for all mental health reimbursement. Entangle the taxonomy into payment infrastructure: no code = no payment = no treatment access. Benefit: standardized cost control, utilization review, denial authority. Could adopt ICD-only or functional impairment codes — but DSM is the entrenched US standard. Exit is arbitrage for the system; constrained for the patient who needs the code.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, insurance_reimbursement_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Autistic self-advocacy organizations (ASAN, AWN), ADHD advocacy, neurodiversity movement, disability justice collectives. Contest the constraint: demand identity-first language, oppose ABA, push for accommodation mandates, challenge DSM criteria. Excluded from DSM workgroups, insurance policy tables, special education rulemaking. Their knowledge (lived experience) is epistemically subordinated to professional authority. Exit is constrained — they organize outside the system but must engage it for material gains.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic language across clinical, educational, legal, insurance, and research systems — a common taxonomy for classifying human distress and difference that enables resource allocation, treatment planning, and epidemiological tracking.
% TRANSFER_FUNCTION: Moves autonomy, self-determination, and epistemic authority from neurodivergent individuals to institutional systems (schools, employers, carceral, medical). Transfers the power to define what counts as 'normal' from the subject to the institution. Transfers material resources (accommodation funding, disability benefits, special education services) through the gate of pathologization — you must accept the disorder label to access support.
% ABSENT_VOICES: Neurodivergent individuals with high support needs who cannot self-advocate (non-speaking autistic people, people with intellectual disability co-occurring) — their experience of the constraint is mediated by caregivers and professionals who may reinforce pathologization. Also absent: neurodivergent people in Global South contexts where DSM is imposed via Western psychiatry but local epistemologies of difference exist. These voices would object to both the pathologization and the Western universalism of the categories.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight: special education law (IDEA) would lose its disability categories — requiring new eligibility frameworks. Insurance reimbursement would collapse — requiring new coding systems. Disability benefits (SSI/SSDI) would lose their medical evidence basis. Carceral classification would lose its diagnostic foundation. Pharmaceutical marketing would lose its indication structure. The neurodiversity movement would lose its primary target but also its primary access ticket to accommodations. The world would rearrange profoundly — but the rearrangement could be toward neurodiversity-affirming frameworks or toward new pathologizing systems.
% FOUNDING_PROBLEM: Late 19th/early 20th century psychiatry lacked a standardized classification system for mental disorders. Asylums, courts, insurance, and early research needed a common language to communicate about patients, commit people involuntarily, and track outcomes. The APA's Statistical Manual (1918) and DSM-I (1952) were built to solve this coordination problem: a shared nosology for institutional record-keeping and inter-clinician communication.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (standardized institutional classification for asylums/courts/early research) is acknowledged as dead by historians of psychiatry (Shorter, Scull, Grob) and by the APA's own DSM-5 field trials documentation — the current DSM's structure serves insurance reimbursement, special education law, disability determination, and pharmaceutical regulation, not the original asylum coordination problem. The APA attests the problem is still live (citing 'clinical utility' and 'research validity'), but this is self-assertion by the beneficiary institution. No independent corroboration supports 'live' status.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.82) because pathologization itself constitutes harm — identity theft, epistemic injustice, internalized stigma — and enables material extraction (forced treatment, denied autonomy, accommodation gatekeeping). Suppression is high (0.78) because the constraint actively enforces neuronormativity: behavioral compliance is coerced in schools (IEPs that target normalization not accommodation), workplaces (performance management on neuronormative metrics), and carceral settings (solitary confinement for neurodivergent 'behavior'). Theater ratio (0.45) reflects genuine clinical utility for some (diagnostic access to support) mixed with performative 'evidence-based' authority that masks the neuronormative foundation. Accessibility collapse (0.65): alternatives (neurodiversity paradigm, social model of disability, accommodation-first frameworks) exist and are growing but are structurally excluded from insurance, education law, and clinical guidelines. Resistance (0.72): neurodiversity movement, mad pride, critical psychiatry, and disability justice movements actively contest the constraint across multiple fronts.
 *
 * PERSPECTIVAL GAP:
 *   From the neurodivergent subject position (identity_locked, powerless), the constraint is experienced as snare: pure extraction via pathologization, enforced by institutions they cannot exit. From the psychiatric profession (institutional, constrained), the constraint reads as tangled_rope: genuine coordination (shared diagnostic language, research common ground) mixed with extraction (gatekeeping, pharmaceutical alignment). From educational/employer/carceral institutions (institutional, arbitrage), the constraint reads as rope: it solves their coordination problem (standardized classification for resource allocation, compliance management) with minimal cost to them. The engine computes this per-seat divergence from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are the primary victims: they bear the full weight of pathologization (d → 1.0). Their exit is identity_locked — neurological identity cannot be shed; masking is costly and unsustainable. Educational institutions, employers, and carceral systems are primary beneficiaries: they extract conformity and avoid accommodation costs (d → 0.0). Their exit is arbitrage — they can switch classification systems if advantageous but the DSM is the entrenched standard. Psychiatric profession sits near symmetric (d ~ 0.5): they administer the constraint and benefit from its authority but also face internal contestation and liability. Pharmaceutical industry and insurance systems are beneficiaries (d → 0.15) — they extract value but are not the primary enforcers. Neurodiversity advocates are excluded observers (d not applicable — they are not governed by the constraint but contest it).
 *
 * MANDATROPHY ANALYSIS:
 *   The DSM taxonomy's founding problem (DSM-I/II: standardize psychiatric classification for institutional record-keeping and treatment planning) is substantially dead — the current constraint persists not because it solves that problem better than alternatives, but because it has become the infrastructure for insurance reimbursement, special education law, disability benefits, carceral classification, and pharmaceutical regulation. The mandate (standardized classification) has atrophied into a rent-extraction mechanism for institutional conformity. This is not a piton (theatrical maintenance of a dead function) — the constraint actively extracts and actively enforces. It is a snare: the coordination story (shared diagnostic language) is cover for the extraction story (neuronormative enforcement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the neurodiversity reading a distinct constraint from the biomedical and critical psychiatry readings of the same DSM taxonomy kernel, or a perspectival shift on one constraint?',
    'Structural decomposition: if the three readings produce different victim sets, beneficiary structures, and extraction referents that cannot be reconciled by changing the observer seat, they are distinct constraints linked by network.affects_constraints.',
    'If distinct constraints, each gets its own ε, its own stakeholder surface, its own classification. If one constraint, the ε-invariance principle is violated and the story must be restructured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the neurodiversity reading instantiates a separate constraint from sibling readings of the DSM taxonomy kernel').

omega_variable(
    pathologization_as_extraction_mechanism,
    'Is the pathologization of neurological variation itself the extraction mechanism, or is pathologization a cover for pharmaceutical market construction (critical psychiatry reading) or genuine disease identification (biomedical reading)?',
    'Compare harm profiles: if neurodivergent individuals experience coercive normalization, denied accommodation, and identity-based stigma as the primary harms — distinct from drug side effects or untreated disease — pathologization is the extraction mechanism for this reading.',
    'Confirms this reading''s high extractiveness (0.82) refers to pathologization-as-harm, not drug-market extraction or diagnostic error. Sibling readings would author different ε referents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathologization_as_extraction_mechanism, conceptual, 'Whether pathologization itself constitutes the extraction in this reading, distinct from sibling readings'' extraction mechanisms').

omega_variable(
    accommodation_denial_as_suppression,
    'Is the denial of accommodation (schools, workplaces, carceral settings) structural suppression by the constraint, or a downstream policy choice separable from the taxonomy?',
    'Trace causal chain: if DSM category assignment is the necessary and sufficient gatekeeper for accommodation access — and the taxonomy''s categories are structured around neuronormative benchmarks — then denial is structural suppression by the constraint.',
    'If structural, suppression (0.78) is authored correctly. If separable, suppression would be lower and the constraint would be more tangled_rope than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_denial_as_suppression, empirical, 'Whether accommodation denial is structural suppression by the DSM taxonomy or separable policy').

omega_variable(
    neurodivergent_coalition_power,
    'Can neurodivergent individuals form effective coalitions across diagnostic categories (autism, ADHD, dyslexia, Tourette''s, etc.) to challenge the constraint, or does the taxonomy''s category structure prevent coalition?',
    'Observe advocacy outcomes: cross-diagnostic neurodiversity movement gains (identity-first language, accommodation mandates, diagnostic criteria reform) vs. category-specific advocacy that reinforces the taxonomy.',
    'If coalition power exists, powerless agents may shift toward moderate/organized over time, altering directionality and effective extraction. If taxonomy prevents coalition, powerless remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodivergent_coalition_power, empirical, 'Whether neurodivergent coalition across DSM categories can challenge the constraint''s power structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t15, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t30, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t45, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 45, 0.41).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_tr_t60, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t15, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t30, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t45, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 45, 0.75).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_be_t60, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t15, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t30, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t45, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 45, 0.71).
narrative_ontology:measurement(dsm_taxonomy_kernel__neurodiversity_reading_su_t60, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.08).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% DSM taxonomy kernel decomposes into three structurally distinct constraints: (1) biomedical_reading — ε ≈ 0.15 (genuine disease mapping, low extraction), Mountain from research seat; (2) critical_psychiatry_reading — ε ≈ 0.75 (pharmaceutical market construction), Snare from patient seat; (3) neurodiversity_reading — ε ≈ 0.82 (pathologization of neurological variation as institutional conformity enforcement), Snare from neurodivergent seat. Each reading has different victim sets, beneficiary structures, and ε referents. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, institutional, 0.1).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, powerless, 0.95).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
