% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: State Imposition Pathway (Exogenous Override Reading)
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the exogenous_override_reading of the
 *   imposition_pathway_kernel. The kernel is the M-set framework's
 *   classification of commitment displacement pathways. This reading asserts
 *   that state capacity enables a distinct exogenous override pathway:
 *   top-down imposition without any fringe adoption stage. The Meiji calendar
 *   reform (1872) and dress decrees (1871-1873) are the paradigmatic cases —
 *   state decree created new commitments through enforcement; compliance was
 *   coerced, not emergent; no meaningful pre-decree fringe existed. The M-set
 *   framework is incomplete without an override cell. This reading forecloses
 *   the endogenous_climb_reading (which claims all displacement is fringe
 *   adoption) and influences the hybrid_cascade_reading (which accepts
 *   override initiation but insists climb completes displacement).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.78).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.85).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "State Imposition Pathway (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, '765f2d3b-33fc-4e40-925d-e5a5c7b547ee').
narrative_ontology:cs_kernel_codification('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', formalized).
narrative_ontology:cs_authority_grounding('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', expertise).
narrative_ontology:cs_interpretation_layer_present('765f2d3b-33fc-4e40-925d-e5a5c7b547ee').
narrative_ontology:cs_reading_relation('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', foundational, state_capacity_enables_direct_commitment_displacement).
narrative_ontology:cs_axiom_status(state_capacity_enables_direct_commitment_displacement, holdable).
narrative_ontology:cs_axiom_grounding('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', state_capacity_enables_direct_commitment_displacement, empirically_contingent).
narrative_ontology:cs_axiom('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', foundational, fringe_adoption_not_necessary_for_commitment_displacement).
narrative_ontology:cs_axiom_status(fringe_adoption_not_necessary_for_commitment_displacement, holdable).
narrative_ontology:cs_axiom_grounding('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', fringe_adoption_not_necessary_for_commitment_displacement, empirically_contingent).
narrative_ontology:cs_reference_frame('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', mset_endogenous_climb_only).
narrative_ontology:cs_drift_state('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', meiji_case_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('765f2d3b-33fc-4e40-925d-e5a5c7b547ee', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, subject_population).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, state_capacity_enables_direct_commitment_displacement).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__exogenous_override_reading, fringe_adoption_not_necessary_for_commitment_displacement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues decrees (calendar reform, dress codes, administrative reorganization) that instantiate new commitments across the population without prior fringe adoption. Enforces compliance through police, military, and bureaucratic machinery. Gains legible, standardized subjects and administrative control. The decree is the mechanism; compliance is extracted by enforcement capacity.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Faces new mandatory practices (Gregorian calendar, Western dress, conscription, household registration) imposed by decree. Non-compliance carries penalties: fines, imprisonment, loss of status, exclusion from state services. No meaningful exit: migration is blocked or impractical; identity documents tie them to the new system. Compliance is coerced, not emergent from prior adoption.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, subject_population, payer,
    powerless, biographical, trapped, national).

% Groups that might have formed a pre-decree adoption fringe (e.g., Dutch-learning scholars, Western-dress adopters pre-1871) were too few, too isolated, and too dependent on state patronage to constitute a climb pathway. The decree did not ratify their prior practice; it bypassed them entirely. They are structurally excluded from the mechanism the endogenous reading requires.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, fringe_actors, excluded,
    powerless, immediate, trapped, local).

% Analyze the Meiji case and comparable impositions (Turkey 1920s, Iran 1930s, Soviet collectivization) to test whether a distinct exogenous override cell is needed in the M-set framework. Their dispute maps onto the three readings: endogenous climb denies the cell; exogenous override demands it; hybrid cascade accepts initiation-by-override but insists completion is climb.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: State capacity solves the coordination problem of rapid, uniform commitment displacement across a heterogeneous population when no voluntary convergence exists. The state provides the synchronization mechanism: a single decree replaces millions of pairwise negotiations.
% TRANSFER_FUNCTION: Moves the cost of commitment adoption from the state (which would bear the cost of persuasion, negotiation, gradual incentivization) to the subject population (which bears the cost of compliance, identity disruption, and enforcement risk). The state captures the benefit of administrative legibility and mobilization capacity.
% ABSENT_VOICES: Pre-decree fringe adopters (Dutch-learning scholars, early Western-dress wearers, calendar reform advocates) are absent from the mechanism — they were not the pathway. Their absence is structural: the exogenous override does not require them. Also absent: populations in regions where state capacity was too weak to impose (e.g., peripheral domains in early Meiji), who experienced neither override nor climb.
% DISAPPEARANCE_RATIONALE: If the exogenous override mechanism were removed from the M-set framework, the framework would misclassify Meiji calendar/dress, Turkish hat reform, Soviet collectivization, and similar cases as compressed climbs — predicting fringe adoption stages that historical evidence shows did not exist. The theoretical world rearranges: a whole class of state-led displacements becomes invisible or misread.
% FOUNDING_PROBLEM: The M-set framework (endogenous climb only) could not account for rapid, uniform commitment displacements where historical records show no meaningful pre-decree fringe adoption — specifically Meiji Japan's 1872-1873 calendar and dress decrees, which achieved near-total compliance within months despite zero prior diffusion.
% FOUNDING_PROBLEM_CORROBORATION: Meiji historians (Jansen, Gluck, Vlastos) document the absence of pre-decree fringe adoption for calendar/dress; comparative sociologists (Mann, Tilly, Centeno) identify parallel cases (Turkey 1925 Hat Law, Iran 1936 Kashf-e hijab, Soviet 1929-1932 collectivization) where state capacity directly imposed commitments. No corroboration comes from the endogenous reading's proponents, who argue the fringe was 'invisible' or 'compressed' — a claim the exogenous reading treats as unfalsifiable.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the state extracts compliance costs from the entire population while capturing administrative legibility benefits. Suppression is very high (0.85) because the mechanism's persistence depends entirely on active enforcement — police, military, household registration, penal codes. Theater ratio is moderate (0.42): the state performs 'civilization and enlightenment' rhetoric, but the enforcement machinery is real and the compliance is genuine. Accessibility collapse is high (0.82) because once the decree issues, alternatives (lunar calendar, traditional dress) are legally and practically extinguished. Resistance is moderate (0.55): there were uprisings (Shinpuren, Chichibu) but state capacity crushed them. The claimed type is snare: coordination story (rapid synchronization) is cover; persistence depends on coercion and suppressing the 'no override needed' alternative.
 *
 * PERSPECTIVAL GAP:
 *   From the state_apparatus seat, the constraint appears as necessary coordination (rope-like): 'we solved the synchronization problem.' From the subject_population seat, it is pure extraction (snare): 'we were forced to change our lives on pain of punishment.' From the fringe_actors seat, it is erasure: 'our prior adoption was ignored; the state did not need us.' From the historical_sociologists seat, it is a theoretical fault line: the M-set framework must either add an override cell or misread the evidence. The engine computes this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_apparatus is the structural beneficiary (d near 0.0): it sets the agenda, controls enforcement, captures administrative gains. The subject_population is the full target (d near 1.0): bears all compliance costs, has trapped exit, faces identity_lock through household registration and conscription. Fringe_actors are excluded (not in the mechanism at all) — their absence is the evidence against endogenous climb. Historical_sociologists are analytical observers (d=0.5). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The exogenous override reading prevents mislabeling state imposition as coordination. If the M-set framework only has endogenous climb, it will read Meiji calendar reform as a 'compressed climb' — attributing coordination function to what was actually coercion. This reading names the mandatrophy: the framework's original mandate (classify displacement pathways) has atrophied by excluding the override pathway, turning a snare into a false rope. The override cell restores the framework's descriptive adequacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the imposition_pathway_kernel admit three structurally distinct readings (endogenous climb, exogenous override, hybrid cascade), or are these merely emphasis differences within a single pathway?',
    'Formalize the M-set pathway taxonomy as a constraint system; test whether the three readings produce mutually exclusive classification of the same historical cases (Meiji, Turkey 1925, Iran 1936, Soviet 1929). If classification diverges, readings are structurally distinct.',
    'If readings are structurally distinct, the kernel requires a committe-system structure with foreclosure/influence relations. If not, the kernel is a single constraint with parameter variation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the kernel decomposes into multiple constraint stories per epsilon-invariance principle.').

omega_variable(
    meiji_fringe_evidence,
    'Was there genuinely zero meaningful pre-decree fringe adoption for Meiji calendar and dress reforms, or does the historical record contain overlooked adoption clusters (Dutch-learning scholars, treaty port merchants, Satsuma/Choshu elites)?',
    'Quantitative analysis of pre-1872 Gregorian calendar usage and Western dress adoption in domain records, merchant ledgers, and visual sources. Threshold: ''meaningful'' = >0.1% of relevant population with sustained practice.',
    'If meaningful fringe existed, exogenous_override_reading''s core evidence weakens; hybrid_cascade_reading gains support. If zero fringe, exogenous_override_reading''s claim is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_fringe_evidence, empirical, 'Empirical basis for the exogenous override''s paradigmatic case.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.85) structural (state enforcement machinery) or internalized (population''s belief in state legitimacy, ''civilization'' ideology)?',
    'Post-decree compliance trajectory after enforcement relaxation: if compliance persists without enforcement, internalized component is significant. Compare Meiji (high persistence) vs. Iran 1936 (rapid reversal after 1941).',
    'If internalized, effective suppression is higher than structural measure suggests — the population carries the constraint forward. This would increase the snare classification strength for the subject_population seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in state imposition.').

omega_variable(
    hybrid_cascade_boundary,
    'Where does exogenous override end and hybrid cascade begin? If the state creates an artificial fringe (conscripts, bureaucrats) who then diffuse the commitment, is that still exogenous override or hybrid?',
    'Define ''artificial fringe'' operationally: state employees forced to adopt first. Measure diffusion speed from artificial fringe to general population. If diffusion is enforced (not voluntary), it remains override; if voluntary, it becomes hybrid.',
    'Determines whether hybrid_cascade_reading is a distinct mechanism or a sub-case of exogenous override. Affects reading_relations: forecloses vs. influences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_cascade_boundary, conceptual, 'Boundary between exogenous override and hybrid cascade mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 1868, 1945).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_tr_t1868, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_tr_t1872, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1872, 0.28).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_tr_t1889, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1889, 0.38).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_tr_t1900, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_tr_t1912, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1912, 0.45).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_tr_t1945, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1945, 0.42).

% Extraction over time
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_be_t1868, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1868, 0.35).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_be_t1872, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1872, 0.72).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_be_t1889, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1889, 0.68).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_be_t1900, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_be_t1912, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1912, 0.62).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_be_t1945, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1945, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_su_t1868, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1868, 0.4).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_su_t1872, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1872, 0.88).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_su_t1889, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1889, 0.75).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_su_t1900, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_su_t1912, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1912, 0.68).
narrative_ontology:measurement(imposition_pathway_kernel__exogenous_override_reading_su_t1945, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1945, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the imposition_pathway_kernel into three readings per epsilon-invariance: exogenous_override (this story, snare, high extraction), endogenous_climb (rope, low extraction), hybrid_cascade (tangled_rope, medium extraction). Each has distinct epsilon, stakeholders, and classification. The exogenous reading forecloses endogenous; both influence hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, institutional, 0.05).
constraint_indexing:directionality_override(imposition_pathway_kernel__exogenous_override_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
