% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Taxonomy as Pathologization of Natural Variation (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested kernel
 *   dsm_taxonomy_kernel: the neurodiversity_reading. The standing arrangement
 *   under contest is the operating DSM classification regime — a formally
 *   codified taxonomy administered by the American Psychiatric Association
 *   and keyed into school discipline, workplace management, civil commitment,
 *   insurance reimbursement, and service eligibility. Read through the
 *   neurodiversity lens, the regime's operative move is converting natural
 *   human neurological variation into disorder categories precisely where
 *   that variation collides with institutional behavioral demands: the
 *   classroom that cannot tolerate a stimming child, the workplace that
 *   cannot schedule around a circadian-shifted adult, the street that cannot
 *   read an autistic person's affect. Epsilon's referent is that standing
 *   arrangement as this reading assesses it — pathologization itself is
 *   counted as harm — never the variation-affirming arrangement this reading
 *   would prefer. Per the epsilon-invariance principle, the colloquial label
 *   'the DSM' decomposes into three structurally distinct constraints (this
 *   file, the biomedical reading, the critical-psychiatry reading), each with
 *   its own epsilon, victim set, and beneficiaries, linked through
 *   network.affects_constraints. The inter-reading contest is carried in
 *   omega variables, not averaged into this file's numbers. KEY AGENTS (by
 *   structural relationship): - american_psychiatric_association: Agenda
 *   setter (institutional/arbitrage) — administers the taxonomy, collects
 *   professional jurisdiction - public_school_administrations: Primary
 *   beneficiary (institutional/constrained) — converts categories into
 *   discipline warrants and funded placements - performance_norm_employers:
 *   Secondary beneficiary (powerful/mobile) — receives a pre-conformed
 *   workforce, bears nothing - civil_commitment_authorities:
 *   Enforcement-adjacent beneficiary (institutional/constrained) — detention
 *   runs on diagnostic language - service_seeking_parents: Dual-positioned
 *   interface (moderate/constrained) — trades a defect label for services -
 *   autistic_individuals: Primary target (organized/trapped) -
 *   adhd_labeled_students: Primary target (powerless/trapped) -
 *   institutionalized_neurodivergent_adults: Primary target
 *   (powerless/trapped) - autistic_self_advocate_networks: Excluded voice
 *   (organized/no seat in the room) - clinical_bioethicists: Analytical
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.8).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.7).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy as Pathologization of Natural Variation (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec').
narrative_ontology:cs_kernel_codification('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', formalized).
narrative_ontology:cs_authority_grounding('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', expertise).
narrative_ontology:cs_interpretation_layer_present('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec').
narrative_ontology:cs_reading_relation('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', foundational, pathology_attribution_tracks_conformity_demand).
narrative_ontology:cs_axiom_status(pathology_attribution_tracks_conformity_demand, holdable).
narrative_ontology:cs_axiom_grounding('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', pathology_attribution_tracks_conformity_demand, empirically_contingent).
narrative_ontology:cs_axiom('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', foundational, classified_people_hold_category_authority).
narrative_ontology:cs_axiom_status(classified_people_hold_category_authority, holdable).
narrative_ontology:cs_axiom_grounding('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', classified_people_hold_category_authority, deontological).
narrative_ontology:cs_reference_frame('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', variation_affirming_descriptive_taxonomy).
narrative_ontology:cs_drift_state('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', contemporary_neurodiversity_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4bd5efe2-96e0-4a23-ad3b-ed26a7c5d1ec', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, public_school_administrations).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, performance_norm_employers).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, civil_commitment_authorities).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, service_seeking_parents).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, adhd_labeled_students).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, institutionalized_neurodivergent_adults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, service_seeking_parents).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, neurological_deficit_doctrine).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__neurodiversity_reading, compliance_as_clinical_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the workgroups, owns the manual, and decides which patterns of human behavior become billable, fundable, and committable categories. Collects professional jurisdiction, publishing revenue, and the standing that comes from being the arbiter of mental normalcy. It can revise or retire any category at will, but its revenue and prestige structures reward expansion of the category set, and its revision process admits outside voices only on terms it controls.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, american_psychiatric_association, agenda_setter,
    institutional, generational, arbitrage, global).

% Convert diagnostic categories into discipline warrants, restrictive placements, behavior-compliance programs, and funded special-education headcounts. A child with an oppositional-defiance or autism label can be segregated, medicated, or conditioned with parental consent already secured by the diagnosis itself. They cannot easily abandon the system because funding formulas, legal safe harbors, and staffing routines all run through diagnostic counts.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, public_school_administrations, beneficiary,
    institutional, biographical, constrained, national).

% Receive a labor pool pre-shaped to neurotypical schedules, sensory tolerances, and social performance, with the cost of behavioral mismatch displaced onto medicalized individuals who must normalize themselves to keep employment. They bear essentially nothing of the classification apparatus and would replace it with any other mechanism that delivered the same conformity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, performance_norm_employers, beneficiary,
    powerful, biographical, mobile, global).

% Invoke psychiatric categories to detain, medicate, and treat people judged dangerous or gravely disabled; the manual supplies the legal language their statutes require. Their discretion is bounded by diagnostic definitions they did not write and cannot change, which they experience as both instrument and constraint.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, civil_commitment_authorities, beneficiary,
    institutional, biographical, constrained, national).

% Accept a defect label for a child because it is the only key the system offers to therapy, classroom aides, and insurance coverage. They obtain real help while absorbing the framing that the child is broken, and they become the administering hand of home-based normalization programs. No comparably funded non-pathologizing route to the same services exists, so declining the label means forfeiting support.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, service_seeking_parents, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, service_seeking_parents, payer).

% Live under categories written about them without them. The diagnosis gates access to accommodations, legal protection, and community, yet stamps them disordered in the same act; refusing the label costs services, accepting it costs self-description. Organized self-advocacy has won partial voice in recent revisions, but the deficit frame of the manual persists, and exit from the classificatory regime is unavailable without forfeiting what the regime alone provides.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals, payer,
    organized, generational, trapped, global).

% Children whose restlessness, daydreaming, or defiance is converted into a treatable condition inside compulsory schooling. Medication and behavior plans follow the label; they cannot vote, litigate, or leave the institution where the regime operates, and their assent is mediated entirely by adults who signed onto the diagnosis.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, adhd_labeled_students, payer,
    powerless, immediate, trapped, national).

% Adults in hospitals, group homes, and forensic systems whose confinement and forced treatment are authorized by their own diagnostic records. Release runs through demonstrating normalized behavior to the same authorities and criteria that confined them, making the path out a stricter version of the path in.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutionalized_neurodivergent_adults, payer,
    powerless, biographical, trapped, national).

% Filed extensive public comment on the most recent manual revision opposing the deficit framing and specific criterion broadenings, and held no seats on the relevant workgroups. Their objections are documented in records the process was free to disregard; their route back into the conversation runs through the very professional bodies they are contesting.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, autistic_self_advocate_networks, excluded,
    organized, generational, trapped, global).

% Analyze diagnostic authority, consent under commitment, and the ethics of medicating children for classroom manageability. They publish critiques and sit on advisory panels but administer nothing and collect nothing from the arrangement's operation.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, clinical_bioethicists, observer,
    institutional, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__neurodiversity_reading, diffuse).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__neurodiversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared categorical vocabulary that lets clinicians communicate cases across institutions, lets researchers aggregate comparable subjects, lets insurers reimburse, and lets agencies determine service eligibility — a real inter-professional coordination problem that predates and would outlast any particular framing of the categories.
% TRANSFER_FUNCTION: Moves self-determination and behavioral latitude from classified neurodivergent individuals to conformity-dependent institutions: the labeled person acquires an official defect narrative and a duty to normalize, while schools, employers, and commitment authorities acquire a warrant to compel, medicate, segregate, or exclude; parallel funding streams move toward the diagnostic apparatus and away from unconditional support.
% ABSENT_VOICES: Autistic self-advocates and other classified people were absent from the workgroups that wrote their categories; survivors of coerced treatment had no seat; parents who declined the label had no funded pathway to articulate. Their objections survive in public comment records the revision process could and did disregard.
% DISAPPEARANCE_RATIONALE: Special-education eligibility, insurance reimbursement, civil-commitment statutes, disability services, and a large clinical-research economy all key to the manual's codes. Overnight disappearance would force legislatures, insurers, and courts to rebuild eligibility and detention law from scratch while clinicians lost their shared language — thousands of dependent arrangements would rearrange within months.
% FOUNDING_PROBLEM: Mid-century American psychiatry ran on incompatible regional nosologies with demonstrably unreliable diagnosis — two clinicians frequently disagreed about the same patient — undermining research, treatment, and the profession's scientific standing. The 1980 rebuild was undertaken to secure inter-rater reliability.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the neo-Kraepelinian turn corroborate the founding problem from outside the association, and the National Institute of Mental Health publicly declared the manual's categories lacking validity in 2013 and launched an alternative research framework — external attestation that the reliability problem was real but no longer governs the arrangement's persistence. No source outside the benefiting parties attests that pathologizing natural variation was ever necessary to solve it.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.80 at interval end) because under this reading the injury is the classification act itself — the defect narrative and the surrendered self-determination — compounded by the coercive normalization flows the label licenses. Suppression (0.70) is authored as a raw structural property and is deliberately NOT scaled by power or scope: commitment statutes, compulsory schooling, and service gatekeeping punish refusal of the label regardless of who is caught in them; only extractiveness is scaled downstream. Theater ratio (0.50) reflects a split operation: clinical communication genuinely works, while a growing share of surrounding activity — validity rhetoric, neuroscience promissory notes, reliability ceremonies — performs a scientific grounding the categories lack, a share that peaked around the 2013 revision cycle when the funding institute publicly broke with the manual. Accessibility collapse is low (0.35): alternatives persist (functioning-based ICD framings, strengths-based assessment, research-domain approaches) but are unfunded and marginalized rather than eliminated. Resistance is high (0.70): the classified population has partial coalition power — organized self-advocacy networks, participatory research, international framework revisions — which caps achievable suppression and explains the slight enforcement softening after 2013. The temporal series run on one shared grid (1952/1968/1980/1994/2013/2022) with every tracked metric authored at every point; the rising extractiveness trajectory tracks the category set's expansion (operationalization in 1980, childhood-disorder growth through the 1990s, criterion broadening in 2013), not any change in the underlying variation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From a clinician's seat the arrangement is rope-like: a working vocabulary that gets patients served and studies aggregated. From the institutional beneficiary seats it is closer to subsidy: conformity arrives pre-packaged with parental consent already attached. From the classified seats the same structure operates as enforced extraction with a coordination veneer — the vocabulary that helps the clinician is the warrant that confines them. The engine derives these per-seat classifications from the structural data (roles, power, exit options); this story's claimed type adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the schools, employers, commitment authorities, and service-seeking parents near the subsidized end (low d): each collects conformity, funding, or access without bearing the classification's costs — the employers most cleanly, since they are mobile and bear nothing. Victim declarations place the three classified groups near the target end (high d): trapped exit and, for the children and institutionalized adults, powerlessness amplify effective extraction. Two refinements are declared as overrides because the automatic derivation would err. The moderate-power seat (service_seeking_parents) derives as a near-pure beneficiary (~0.15) from the beneficiary roll, but the parents also absorb the defect framing of their own child and administer home-based normalization, so their true position is mixed (0.35). The organized-power seat (autistic_individuals) derives as a near-full target from victim status plus trapped exit, but the diagnosis also confers services, legal protection, and community membership — a real partial offset that leaves them still clearly target-side (0.75). Receipt surface, checked seat by seat: conformity dividends split across schools, employers, and commitment authorities; professional-authority rents accrue to the association; no single seat captures the majority of what the arrangement moves, so gain_flow is authored as 'diffuse' as an affirmative finding, not a default. Fixing cost is 'prohibitive': eligibility law, reimbursement codes, and commitment statutes are welded to the taxonomy, and full decoupling is a multi-decade legislative rebuild exceeding any single fixer's capacity — though the 1973 excision of homosexuality proves targeted removal is feasible where the coalition is strong enough.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — diagnostic unreliability wrecking research and clinical communication — was real, externally corroborated, and partially solved; the arrangement now persists chiefly by serving conformity demand and professional jurisdiction, with the founding problem's status contested rather than plainly dead (reliability gains were real, and the communication function remains live). The classification prevents mislabeling in both directions: calling this a rope honors the genuine coordination function while erasing who pays for it; calling it a snare correctly identifies the victims but falsely implies the coordination story is mere cover, when clinicians and classified people alike put the vocabulary to real use. Tangled rope holds both truths: coordination and extraction run through the same categories, enforcement is active (commitment law, school compliance regimes, gatekept services), and the six-questions mismatch consumer can cross-check the contested founding-problem status against the world_rearranges verdict to test whether the arrangement is living on a mandate it has outgrown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_kernel_reading_indexicality,
    'This constraint is one reading of the kernel dsm_taxonomy_kernel (reading: neurodiversity_reading). Would instantiating the sibling readings — biomedical_reading (categories as discoverable disease entities) or critical_psychiatry_reading (categories reverse-engineered from drug markets) — produce structurally different constraints with different victim sets and epsilon?',
    'Comparative classification of the sibling story files: hold the standing arrangement fixed as referent and re-derive epsilon, victims, and beneficiaries under each reading''s own lights.',
    'If the biomedical reading prevailed, epsilon would fall toward coordination-cost levels (treating a real disease entity is medicine, not extraction) and the victim set would shrink to misdiagnosed individuals. If the critical-psychiatry reading prevailed, epsilon stays high but the receipt surface relocates to pharmaceutical manufacturers and prescriber incentives. This file''s high epsilon is conditional on the neurodiversity reading being the operative frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_kernel_reading_indexicality, conceptual, 'Reading-indexed classification: sibling readings of the same kernel instantiate different constraints.').

omega_variable(
    variation_disorder_boundary,
    'Does the natural-variation premise extend across the full severity range the DSM covers, or does the variation/disorder distinction regain bite at high-support-needs conditions where the classified person cannot sustain self-determination?',
    'Longitudinal outcome data disaggregated by support-needs tier: if wellbeing trajectories diverge sharply at the severe end under identical diagnostic frames, the uniform-variation premise fails there.',
    'If the boundary fails at the severe end, this reading''s victim set narrows and epsilon falls for those specific categories, splitting the constraint into a strong-form and weak-form story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(variation_disorder_boundary, empirical, 'Whether ''natural variation, not pathology'' holds uniformly or breaks at high support needs.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (commitment statutes, compulsory schooling, service gatekeeping that punishes refusal of the label) or internalized (self-stigma and absorbed defect narratives that lead classified people to endorse their own pathologization)?',
    'Post-exit suppression trajectory: track people who legally escape the regime (aging out of schooling, moving to accommodation-friendly jurisdictions) — if deference to the defect frame persists after the enforcing barrier is removed, the internalized component is substantial.',
    'If internalized, effective suppression exceeds the structural measure and survives formal reform; statutory fixes alone would not release the targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in diagnostic gatekeeping.').

omega_variable(
    conformity_demand_primacy,
    'Is the extraction driven by the taxonomy itself, or by the institutional demand for behavioral conformity that purchases the taxonomy — would a strictly neutral descriptive vocabulary serving the same institutions re-extract?',
    'Natural experiment from jurisdictions and pilot programs using strengths-based or functioning-based eligibility frameworks feeding the same schools and employers: if the conformity dividend persists under a neutral taxonomy, the driver is the demand side.',
    'Determines the reform target: revising the manual is sufficient if the taxonomy drives extraction; institutional redesign is required if the conformity demand does.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conformity_demand_primacy, conceptual, 'Whether the manual or the conformity demand it serves is the operative extractor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1952, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_neurodiv_reading_tr_t1952, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1952, 0.3).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_tr_t1952, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_tr_t1968, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1968, 0.33).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_tr_t1968, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.44).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_tr_t1980, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_tr_t1994, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1994, 0.47).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_tr_t1994, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_tr_t2013, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2013, 0.53).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_tr_t2013, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_tr_t2022, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2022, 0.5).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_tr_t2022, observed).

% Extraction over time
narrative_ontology:measurement(dsm_neurodiv_reading_be_t1952, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1952, 0.55).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_be_t1952, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_be_t1968, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1968, 0.58).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_be_t1968, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_be_t1980, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_be_t1994, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1994, 0.72).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_be_t1994, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_be_t2013, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2013, 0.78).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_be_t2013, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_be_t2022, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2022, 0.8).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_be_t2022, observed).

% Suppression requirement over time
narrative_ontology:measurement(dsm_neurodiv_reading_su_t1952, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1952, 0.58).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_su_t1952, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_su_t1968, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_su_t1968, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.64).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_su_t1980, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_su_t1994, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1994, 0.69).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_su_t1994, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_su_t2013, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2013, 0.73).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_su_t2013, observed).
narrative_ontology:measurement(dsm_neurodiv_reading_su_t2022, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement_basis(dsm_neurodiv_reading_su_t2022, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the DSM' covers three structurally distinct claims that must not share one story. The biomedical reading (upstream, highest empirical confidence) is routinely cited as legitimating evidence by the other two, which is why the family edges run through it; the critical-psychiatry reading and this neurodiversity reading are downstream contestants with different victim sets (drug-market enrollees vs. conformity-enforced neurodivergent people) and different beneficiaries (pharmaceutical manufacturers vs. conformity-dependent institutions). Each file carries its own epsilon over the same standing arrangement; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
