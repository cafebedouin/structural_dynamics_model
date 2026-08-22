% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: DSM Pathologization of Neurological Variation (Neurodiversity Reading)
 *   domain: medical epistemology/psychiatric taxonomy/social construction of illness
 *
 * SUMMARY:
 *   Under the neurodiversity reading, the operating DSM regime functions as a
 *   conformity-enforcement layer wrapped around a genuine classification
 *   vocabulary: categories written against institutional behavioral baselines
 *   (classroom stillness, sustained verbal availability, conventional
 *   sociability) convert neurological difference into disorder, and the
 *   conversion is enforced through service gatekeeping, school referral
 *   pipelines, insurance coding requirements, and commitment statutes. The ε
 *   referent is the standing arrangement — the taxonomy as actually operated,
 *   with its eligibility, billing, and legal dependencies — assessed by this
 *   reading's own lights; it is not the accommodation-first alternative the
 *   reading endorses. The claim/metric gap is deliberate where present: the
 *   taxonomy is publicly claimed as neutral medical description while the
 *   authored metrics describe its operated function under this reading. This
 *   file is one member of a three-story constraint family decomposing the
 *   contested label 'DSM categories'; see network.dual_formulation_note and
 *   the sibling-delta omega. KEY AGENTS (by structural relationship): -
 *   american_psychiatric_association_publishers: Agenda setter
 *   (institutional/arbitrage) — defines which variations count as disorders -
 *   behavioral_conformity_schools: Enforcing beneficiary
 *   (organized/constrained) — converts classroom disruption into diagnosis
 *   and compliance - conformity_dependent_employers: Primary beneficiary
 *   (powerful/mobile) — collects workforce legibility -
 *   psychiatric_carcerality_infrastructure: Beneficiary
 *   (institutional/constrained) — converts categories into lawful processing
 *   reach - coercively_normalized_children: Primary target
 *   (powerless/trapped) — bears pathologization and normalization -
 *   accommodation_denied_adults: Target with partial service unlock
 *   (moderate/constrained) — pays self-definition for support -
 *   parents_of_evaluated_children: Intermediary (moderate/constrained) — both
 *   bears and administers - frontline_diagnosticians: Dual-positioned
 *   operator (organized/constrained) — bills the categories, absorbs their
 *   churn - neurodivergent_self_advocates: Excluded voice
 *   (organized/constrained) — campaigns from outside committee authority -
 *   disability_studies_observers: Analytical observer (moderate/analytical) —
 *   sees the full drafting-and-downstream structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.76).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.7).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurological Variation (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical epistemology/psychiatric taxonomy/social construction of illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '66ab3dfe-83e5-46a3-b6dc-7728d3809990').
narrative_ontology:cs_kernel_codification('66ab3dfe-83e5-46a3-b6dc-7728d3809990', fixed_text).
narrative_ontology:cs_authority_grounding('66ab3dfe-83e5-46a3-b6dc-7728d3809990', expertise).
narrative_ontology:cs_interpretation_layer_present('66ab3dfe-83e5-46a3-b6dc-7728d3809990').
narrative_ontology:cs_reading_relation('66ab3dfe-83e5-46a3-b6dc-7728d3809990', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('66ab3dfe-83e5-46a3-b6dc-7728d3809990', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('66ab3dfe-83e5-46a3-b6dc-7728d3809990', foundational, variation_is_natural_human_diversity).
narrative_ontology:cs_axiom_status(variation_is_natural_human_diversity, holdable).
narrative_ontology:cs_axiom_grounding('66ab3dfe-83e5-46a3-b6dc-7728d3809990', variation_is_natural_human_diversity, empirically_contingent).
narrative_ontology:cs_axiom('66ab3dfe-83e5-46a3-b6dc-7728d3809990', secondary, accommodation_precedes_normalization).
narrative_ontology:cs_axiom_status(accommodation_precedes_normalization, holdable).
narrative_ontology:cs_axiom_grounding('66ab3dfe-83e5-46a3-b6dc-7728d3809990', accommodation_precedes_normalization, deontological).
narrative_ontology:cs_reference_frame('66ab3dfe-83e5-46a3-b6dc-7728d3809990', descriptive_support_oriented_taxonomy).
narrative_ontology:cs_drift_state('66ab3dfe-83e5-46a3-b6dc-7728d3809990', contemporary_post_neurodiversity_movement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66ab3dfe-83e5-46a3-b6dc-7728d3809990', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, behavioral_conformity_schools).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, conformity_dependent_employers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_carcerality_infrastructure).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, coercively_normalized_children).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, accommodation_denied_adults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, accommodation_denied_adults).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, parents_of_evaluated_children).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, frontline_diagnosticians).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, parents_of_evaluated_children).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, frontline_diagnosticians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes and revises the diagnostic manual that determines which patterns of thought, feeling, and behavior count as mental disorders. Convenes the workgroups that draft criteria, decides what enters or leaves each edition, and licenses the code set that hospitals, insurers, courts, and schools rely on. Because it writes the definitions, it can redefine its own product each revision cycle, and no external body can compel a category's removal.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, american_psychiatric_association_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Refer students whose movement, attention, or social style disrupts classroom routine for psychiatric evaluation, and condition placement, discipline, and sometimes continued enrollment on evaluation outcomes and compliance plans. They gain orderly classrooms when disruptive students acquire diagnoses, prescriptions, or separate settings; they bear the costs of evaluation obligations and paperwork. They cannot abandon the referral pipeline without jeopardizing special education funding that is tied to documented disability counts.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, behavioral_conformity_schools, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, behavioral_conformity_schools, beneficiary).

% Run workplaces built around standard shifts, continuous verbal availability, and conventional social performance. A documented diagnosis converts an employee described as difficult into a case with an established management literature, letting the label perform the sorting the employer would otherwise negotiate person by person. They hire inside whichever taxonomy the clinics currently issue, so their position survives every revision.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, conformity_dependent_employers, beneficiary,
    powerful, generational, mobile, global).

% Operate the involuntary evaluation, commitment, and court-mandated treatment pathways that legally require a qualifying diagnosis before detention or mandated treatment can proceed. Every widening of the diagnostic manual enlarges the population they may lawfully process; every narrowing contracts their caseload and statutory reach. Their budgets and staffing scale with the breadth of the category set.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_carcerality_infrastructure, beneficiary,
    institutional, generational, constrained, national).

% Are referred for evaluation because their movement, attention, or way of relating does not fit classroom routine, then carry the resulting label into medication decisions, behavior plans, and permanent educational records. Too young to consent meaningfully and required by attendance law to sit in the room where the mismatch is defined, they experience behavioral normalization as the standing price of remaining enrolled.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, coercively_normalized_children, payer,
    powerless, biographical, trapped, national).

% Need schedule flexibility, sensory-modified environments, or adjusted communication expectations to stay employed and housed, and nearly every legal route to those adjustments runs through a documented diagnosis. Accepting the label unlocks protection while attaching a disorder entry to employment, insurance, and licensing files; declining it preserves self-description but forfeits support. Leaving the documented channels altogether means leaving the job and benefit systems that require them.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, accommodation_denied_adults, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, accommodation_denied_adults, beneficiary).

% Receive the school's referral letter facing a fork: consent to evaluation and accept whatever category follows, in exchange for aides, services, and legal protections the child otherwise goes without, or refuse and contest the school alone. Many then administer at home the behavior plans and medication schedules the category prescribes, becoming intermediaries in the arrangement they did not design.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, parents_of_evaluated_children, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, parents_of_evaluated_children, beneficiary).

% Translate the manual's criteria into billable codes that anchor reimbursement, treatment authorization, and legal documentation. Each added category brings clinical demand and billing volume; each criteria revision brings retraining costs, insurer claim denials for unfavored codings, and liability exposure attached to the labels they assign. Their practice forms are set by committees they do not staff.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, frontline_diagnosticians, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, frontline_diagnosticians, payer).

% Organized networks of autistic and otherwise neurodivergent adults who hold that the categories describe them inaccurately and damagingly, and who campaign for depathologized language, accommodation-first policy, and seats in the drafting process. Historically absent from the committee tables where criteria were written, they remain consultative rather than decisive, and they cannot step outside the taxonomy because services, protections, and public understanding all route through it.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_self_advocates, excluded,
    organized, generational, constrained, global).

% Researchers in disability studies, history of medicine, and medical ethics who reconstruct how the categories were drafted, whose testimony counted in committee, and what happens downstream when a label attaches. They hold no service disputes of their own and can place the manual beside its own archives and outcome records.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, disability_studies_observers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, diffuse).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, operationalized vocabulary that lets clinicians, insurers, researchers, educators, and courts coordinate: one code set for billing, one criterion set for research sampling, one eligibility language for services, accommodations, and involuntary-treatment statutes — problems previously handled ad hoc by each institution separately.
% TRANSFER_FUNCTION: Moves definitional authority over mind and behavior from individuals to committees and institutions: a person who takes the label exchanges self-description for service eligibility, while schools, employers, courts, and insurers receive a standardized instrument that converts behavioral mismatch into a manageable, fundable, and in some pathways detainable category. Secondarily, it moves billing revenue along diagnostic channels.
% ABSENT_VOICES: The drafting committees historically seated no neurodivergent members: the people the criteria would describe had no vote on their own definition, and today's consultative inclusion remains advisory rather than decisive. Also absent are people deterred from clinics by fear of the label, whose objections never enter the clinical record the categories are validated against.
% DISAPPEARANCE_RATIONALE: Service eligibility, insurance reimbursement, special-education placement, research subject selection, and involuntary-commitment statutes are all keyed to the codes. Overnight removal would force every dependent system to rebuild its eligibility language within months; the underlying human variation would persist and be redescribed under whatever successor framework emerged, but the current allocation of authority over it would dissolve.
% FOUNDING_PROBLEM: Postwar psychiatric practice was fragmented: competing schools used incompatible vocabularies, billing was chaotic across institutions, and research samples could not be aggregated because no two clinics meant the same thing by the same term. The manual was built to supply a common nomenclature.
% FOUNDING_PROBLEM_CORROBORATION: Historians of psychiatry working outside the benefiting institutions attest the nomenclature's origin in postwar fragmentation and billing standardization; cross-national ICD users corroborate that a common vocabulary solves a real communication problem independent of the American manual. Neurodivergent self-advocacy organizations and disability studies scholars, speaking from outside the beneficiary set, attest that the current pathologizing function has diverged from that founding purpose. The publisher attests the founding problem remains live; no beneficiary-independent source attests that the expanding pathologizing categories serve it.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extraction is high (ε 0.76) because under this reading the harm is constitutive, not incidental: the act of pathologization itself is the burden, and the same instruments that deliver it also gate every accommodation route. Suppression is substantial (0.70) and predominantly structural — roughly two-thirds by my estimate sits in external machinery (diagnosis-required services, compulsory-schooling leverage, commitment statutes, insurance coding mandates) with the remainder internalized as self-stigma and identity injury; the omega variable suppression_mechanism_split records that estimate's uncertainty. Theater ratio (0.55) reflects pseudo-precision: decimal severity specifiers, symptom-count thresholds, and differential trees that imply a measurement validity the underlying constructs do not possess, alongside real residual functions (communication, billing, research sampling) that keep theatricality below dominance. Accessibility collapse is moderate (0.58): private-pay routes, ICD alternatives, and self-identification communities exist, but legally enforceable accommodations almost always require a diagnosis from the DSM family, so understood alternatives remain partly closed. Resistance (0.62) is real and organized — self-advocacy campaigns, critical scholarship, international depathologization pressure — reflecting genuine coalition formation among otherwise diffuse targets. The temporal series runs on one shared seven-point grid (all three metrics authored at every point, per the alignment rule). Base extractiveness is deliberately non-monotonic: it dips from 0.62 to 0.54 across the 1973 declassification era, then climbs steadily as operationalized criteria multiplied child and adult categories and enforcement infrastructure matured around them. Suppression_requirement is authored because the story specifically traces enforcement-capacity build-up — the school referral pipeline, IDEA-era diagnosis gating, outpatient commitment statutes, insurer utilization management — a rising enforcement ratchet, not merely shifting extraction. The base_properties scalars report the end-state (2026) values.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the publisher's and enforcing institutions' positions, the arrangement presents as functioning classification: order maintained, cases managed, services targeted. From the children's and denied adults' positions, the identical structure operates as enforced pathologization with exits that close on contact — the child cannot decline enrollment, the adult cannot decline the employment and benefit systems that demand the label. Identity-lock dynamics bind both target seats differently: for denied adults the lock is relational-instrumental (the label fuses with the file that follows them through employment, insurance, and licensing), while for children it is developmental (the category arrives before self-concept stabilizes and structures it). A third lock appears where diagnosis becomes reclaimed identity; if the services-from-diagnosis link broke, those locks would loosen and the affected seats' classifications would shift accordingly. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats derive low directionality: conformity_dependent_employers sit nearest the beneficiary end (they collect legibility while holding arbitrage-grade exit — they hire inside whatever taxonomy exists), followed by psychiatric_carcerality_infrastructure (collects statutory reach, moderately exit-constrained by budget dependence on category breadth) and behavioral_conformity_schools (dual-positioned: enforcing administrators who also bear compliance paperwork costs). Same-nominal-level differentiation: schools and employers both hold organized-or-better power on the beneficiary side, but the employer's hiring mobility versus the school's funding-statute captivity separates their effective positions — constraint-specific factors, not global standing, drive the difference. Target seats derive high directionality: coercively_normalized_children sit nearest the full-target end (powerless, trapped by attendance law and minority, labeled at a developmental stage), with accommodation_denied_adults somewhat lower on the target axis because the service unlock returns part of what the label takes (their dual position is genuine, not noise). Parents occupy the middle with a payer-dominant skew; diagnosticians derive low-to-moderate directionality with a payer overlay from criteria churn and liability. Coalition potential among the diffuse targets (children via adult-allied advocacy) is already partially realized and is reflected in the resistance value.
 *
 * MANDATROPHY ANALYSIS:
 *   Holding this as a tangled rope rather than resolving it downward or upward is what prevents misclassification in both directions. Reading it as a pure snare would erase the taxonomy's real coordination residue — clinical communication, billing interoperability, research sampling — that even this reading concedes and that explains why replacement proposals keep failing; reading it as a rope would erase the asymmetric burden the same structure imposes, where coordinated parties share a vocabulary while a defined class of people pays in self-definition, records, and coerced normalization. Active enforcement is load-bearing: the pathologizing function persists through gatekeeping machinery, not through voluntary uptake, which rules the piton reading out despite meaningful theater in the criteria apparatus. On the genealogy: the founding communication problem remains live but subordinated — it is real, corroborated from outside the beneficiary set, and no longer what drives the constraint's growth, which tracks enforcement expansion instead. The mandate has therefore not simply outlived its function (ruling out a resolved-mandatrophy declaration) nor died (ruling out the piton cell); it has been redirected, which is precisely the tangled-rope signature the receipt surface corroborates: gains flow diffusely across conformity-dependent institutions rather than concentrating in any single capturer, and fixing — rebuilding eligibility, billing, and legal-capacity infrastructure on non-pathologizing foundations — is prohibitively costly relative to its benefit as the fixers themselves perceive it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading (neurodiversity_reading) of the dsm_taxonomy_kernel. How would the victim set, beneficiary set, or epsilon change if the same kernel were instantiated under a sibling reading?',
    'Read the sibling stories'' beneficiary/victim declarations and epsilon values directly: the biomedical_reading file and the critical_psychiatry_reading file each carry their own structural data over the same referent period.',
    'Under biomedical_reading the victim set shrinks toward persons left medically untreated and beneficiaries come to include patients gaining treatment access, driving epsilon sharply down; under critical_psychiatry_reading beneficiaries relocate to pharmaceutical manufacturers and marketing-dependent psychiatry, and victims shift toward over-medicated consumers. Classification differences between the files are the measurement, not an error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure routed to omega per the kernel-authoring rules: which reading this file instantiates and what siblings would change.').

omega_variable(
    ontological_status_of_variation,
    'What is the ontological status of the classified variation — intrinsic disease entity (biomedical), constructed market artifact (critical psychiatry), or natural human diversity mismatched to institutional norms (this reading)? The three readings locate the kernel dispute at exactly this element.',
    'Convergent multi-line evidence: cross-cultural prevalence stability, biomarker replication rates, treatment-response specificity, and historical contingency analyses of threshold-setting decisions.',
    'Relocating the status moves victims and beneficiaries wholesale between the three family stories and flips the epsilon profile; the classification of this reading stands or falls with the natural-diversity location.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_status_of_variation, conceptual, 'Where the kernel disagreement is located: the ontology of the variation itself.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression predominantly structural (diagnosis-required services, compulsory-schooling leverage, commitment statutes) or internalized (self-stigma, identity injury that persists after external barriers are removed)?',
    'Post-declassification trajectory studies of populations that lost or shed a label — the homosexuality declassification precedent, DSM-5 autism-consolidation cohorts — measuring whether distress and avoidance persist once gatekeeping is removed.',
    'If the internalized share is large, effective suppression exceeds the structural measure and travels with the target after exit; the trapped and constrained exit attributions for the target seats would need upward revision, and the classification of those seats hardens accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized composition of the suppression burden.').

omega_variable(
    institutional_norm_contingency,
    'Are the behavioral norms the categories protect — classroom stillness, continuous verbal availability, conventional sociability — necessary features of functional mass institutions, or redesignable conventions?',
    'Natural experiments: accommodative school designs, sensory-modified workplace pilots, remote-work productivity data, universal-design implementations at scale.',
    'If the norms are redesignable, the extraction is a policy choice riding on contingent conventions and effective extraction attributable to the institutional beneficiaries rises; if they are necessary, part of the measured burden is the irreducible price of mass coordination, pulling the classification toward the rope side of the hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_norm_contingency, conceptual, 'Contingency of the institutional norms against which variation is judged disordered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1952, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1952, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1952, 0.28).
narrative_ontology:measurement(dsm__tr_t1968, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1994, 0.47).
narrative_ontology:measurement(dsm__tr_t2006, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2006, 0.53).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2013, 0.51).
narrative_ontology:measurement(dsm__tr_t2026, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2026, 0.55).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1952, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1952, 0.62).
narrative_ontology:measurement(dsm__be_t1968, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1968, 0.58).
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.54).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1994, 0.61).
narrative_ontology:measurement(dsm__be_t2006, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2006, 0.68).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2013, 0.73).
narrative_ontology:measurement(dsm__be_t2026, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2026, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1952, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1952, 0.45).
narrative_ontology:measurement(dsm__su_t1968, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.56).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1994, 0.62).
narrative_ontology:measurement(dsm__su_t2006, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2006, 0.67).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2013, 0.69).
narrative_ontology:measurement(dsm__su_t2026, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2026, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'DSM categories' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel (dsm_taxonomy_kernel). This file authors the neurodiversity_reading (epsilon 0.76; victims: coercively normalized and accommodation-denied neurodivergent people; beneficiaries: conformity-dependent institutions). The sibling files author the biomedical_reading (low epsilon; victims: the untreated; beneficiaries: patients and treaters) and the critical_psychiatry_reading (high epsilon; victims: over-medicated consumers; beneficiaries: pharmaceutical manufacturers). The upstream reading by historical establishment is the biomedical_reading, whose disease-entity framing is what the other two readings contest; each file links the others through affects_constraints so contamination and drift propagate across the family. Measuring the family through any single observable would conflate three different epsilons; the decomposition exists so that each claim carries one stable value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
