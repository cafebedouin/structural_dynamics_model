% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__liberal_due_process_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__liberal_due_process_reading, []).

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
 *   constraint_id: magna_carta_clause_39__liberal_due_process_reading
 *   human_readable: Magna Carta Clause 39 â Liberal Due Process Reading
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   This constraint story instantiates the liberal due process reading of
 *   Magna Carta Clause 39, interpreting the 1215 text ('No free man shall be
 *   seized or imprisoned... except by the lawful judgment of his peers or by
 *   the law of the land') as establishing universal individual rights against
 *   arbitrary state power. The reading constrains executive discretion by
 *   subjecting state action to broad judicial review under due process norms.
 *   It is contested by a feudal reading (class-specific privilege) and an
 *   originalist reading (1215-specific abuse limitation). The story treats
 *   the liberal reading as the active interpretive constraint governing
 *   modern administrative and human rights law.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda-setter and beneficiary (institutional/analytical) â gains power of substantive review
 *   - executive_authorities: Primary payer (powerful/constrained) â loses arbitrary discretion
 *   - citizens_at_large: Mixed beneficiary/payer (organized/constrained) â gain nominal rights but bear judicialization costs
 *   - legal_profession: Secondary beneficiary (organized/mobile) â captures litigation rents
 *   - originalist_interpreters: Excluded analytical seat (moderate/constrained) â marginalized by dominant liberal canon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, 0.72).
domain_priors:suppression_score(magna_carta_clause_39__liberal_due_process_reading, 0.68).
domain_priors:theater_ratio(magna_carta_clause_39__liberal_due_process_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(magna_carta_clause_39__liberal_due_process_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__liberal_due_process_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__liberal_due_process_reading, "Magna Carta Clause 39 â Liberal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__liberal_due_process_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_clause_39__liberal_due_process_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__liberal_due_process_reading, 'adcbdb67-4124-45e3-aa7a-7a172db560ab').
narrative_ontology:cs_kernel_codification('adcbdb67-4124-45e3-aa7a-7a172db560ab', fixed_text).
narrative_ontology:cs_authority_grounding('adcbdb67-4124-45e3-aa7a-7a172db560ab', lineage).
narrative_ontology:cs_interpretation_layer_present('adcbdb67-4124-45e3-aa7a-7a172db560ab').
narrative_ontology:cs_reading_relation('adcbdb67-4124-45e3-aa7a-7a172db560ab', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_reading_relation('adcbdb67-4124-45e3-aa7a-7a172db560ab', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('adcbdb67-4124-45e3-aa7a-7a172db560ab', foundational, universal_individual_due_process).
narrative_ontology:cs_axiom_status(universal_individual_due_process, holdable).
narrative_ontology:cs_axiom_grounding('adcbdb67-4124-45e3-aa7a-7a172db560ab', universal_individual_due_process, deontological).
narrative_ontology:cs_axiom('adcbdb67-4124-45e3-aa7a-7a172db560ab', secondary, judicial_supremacy_in_rights_adjudication).
narrative_ontology:cs_axiom_status(judicial_supremacy_in_rights_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('adcbdb67-4124-45e3-aa7a-7a172db560ab', judicial_supremacy_in_rights_adjudication, conventional).
narrative_ontology:cs_reference_frame('adcbdb67-4124-45e3-aa7a-7a172db560ab', classical_liberal_legal_order).
narrative_ontology:cs_drift_state('adcbdb67-4124-45e3-aa7a-7a172db560ab', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('adcbdb67-4124-45e3-aa7a-7a172db560ab', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__liberal_due_process_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, legal_profession).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__liberal_due_process_reading, citizens_at_large).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, executive_authorities).
narrative_ontology:constraint_victim(magna_carta_clause_39__liberal_due_process_reading, citizens_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces Clause 39 as a broad guarantee of due process, gaining authority to review executive action for substantive fairness. The liberal reading expands judicial power from narrow procedure to overarching supervision of state-citizen interactions.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, judiciary, beneficiary).

% Bear the loss of discretionary power to act without documented legal process. Administrative and police actions must anticipate judicial scrutiny, slowing decision-making and constraining policy flexibility previously exercised under prerogative or discretionary frameworks.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, executive_authorities, payer,
    powerful, biographical, constrained, national).

% Hold nominal rights against arbitrary imprisonment and dispossession, but must bear the procedural costs, delays, and compliance burdens of vindicating those rights through formal legal channels. The expansive reading subjects all individual-state interactions to potential judicial review, diffusing costs across the entire population while centralizing remedy in institutions many cannot afford to access.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, citizens_at_large, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__liberal_due_process_reading, citizens_at_large, payer).

% Captures wealth and status from the litigation, rights-claiming, and compliance activity generated by an expansive due process doctrine. The complexity of liberal judicial review creates demand for professional intermediaries between citizens and the state.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Maintain that Clause 39 is limited to the specific procedural abuses documented in 1215. Their interpretive tradition is structurally marginalized within dominant legal education, precedent, and judicial appointment frameworks that treat the liberal reading as self-evident baseline.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__liberal_due_process_reading, originalist_interpreters, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__liberal_due_process_reading, judiciary).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__liberal_due_process_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes predictable, general legal procedures governing all state-citizen interactions, replacing arbitrary executive discretion with declared rules and judicial oversight.
% TRANSFER_FUNCTION: Transfers discretionary power from executive agencies to judicial interpreters; transfers wealth and attention to the legal profession; transfers nominal rights to citizens while imposing procedural compliance costs on the same population.
% ABSENT_VOICES: Originalist jurists and historians who read Clause 39 as a narrow medieval privilege are structurally excluded from the dominant interpretive canon; their absence allows the universality claim to appear natural and historically grounded.
% DISAPPEARANCE_RATIONALE: If the liberal due process reading vanished overnight, executive agencies would operate with fewer judicial constraints, citizens would lose a primary mechanism for challenging state action, the legal profession would contract, and the modern administrative and human rights order would lose a foundational interpretive pillar.
% FOUNDING_PROBLEM: Arbitrary exercise of state power â imprisonment, dispossession, and violence without legal process â threatened social order and individual security in medieval and early modern England.
% FOUNDING_PROBLEM_CORROBORATION: Liberal legal historians attest the problem persists in modern form. Originalist and critical scholars attest the founding problem was specific to thirteenth-century baronial grievances and that the modern reading projects contemporary concerns onto a medieval text; no neutral party outside the interpretive dispute corroborates either framing exclusively.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__liberal_due_process_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__liberal_due_process_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__liberal_due_process_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__liberal_due_process_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__liberal_due_process_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__liberal_due_process_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__liberal_due_process_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading transfers substantial discretionary power from executive actors to judicial interpreters and subjects all state-citizen interactions to formal legal process. Suppression (0.68) is high because the liberal reading has become doctrinally dominant, structurally marginalizing originalist and feudal alternatives through precedent and legal education. Theater ratio (0.45) reflects moderate performative maintenance: rights rhetoric legitimates judicial power while obscuring the transfer of democratic agency to unelected courts. Accessibility collapse (0.82) is high because, once the liberal reading is accepted, narrow historical alternatives become almost unthinkable within legal discourse. Resistance (0.55) is moderate: originalist movements and executive-branch pushback persist but are contained. The measurement series share a single time grid, tracking the reading's intensification from early modern emergence through contemporary human rights expansion.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary experiences the constraint as genuine coordination (enforcing rule of law) and legitimate authority expansion; executive authorities experience it as extraction of discretion; citizens experience a mixed seat (rights protection versus procedural burden and democratic deficit). The engine computes divergent per-seat classifications from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (judiciary, legal profession, citizens) derive low directionality; victim declarations (executive authorities, citizens as payers) derive high directionality. Citizens are structurally dual-positioned: they benefit from rights against state power but pay through judicialization costs and the democratic deficit created by judicial supremacy. The executive bears concentrated extraction of discretion. No directionality overrides are required because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â arbitrary state violence â is contested in status. Liberal jurists claim it is live; originalists claim the modern reading addresses a different problem than the 1215 text solved. The mismatch (founding_problem_status contested + disappearance_verdict world_rearranges) signals that the constraint has accumulated functions beyond its origin. However, the genuine coordination provided by procedural regularity prevents classification as pure snare; the asymmetric concentration of power in the judiciary and the diffuse costs to citizens make tangled_rope the structurally accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liberal_reading_historical_legitimacy,
    'Is the liberal due process reading a genuine development of the 1215 kernel or an extraction of legitimacy from a feudal class privilege?',
    'Historical philology of ''liber homo'' and ''per legem terrae'' in thirteenth-century legal context, combined with manuscript tradition analysis.',
    'If the text is irreducibly feudal and class-bound, the liberal reading functions as a false summit or snare, using ancient textual legitimacy to mask a modern transfer of power to the judiciary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_reading_historical_legitimacy, empirical, 'Whether the liberal reading is historically anchored or anachronistic').

omega_variable(
    citizen_extraction_mechanism,
    'Do citizens primarily benefit from rights-protection or pay through judicialization and democratic deficit under this reading?',
    'Comparative analysis of litigation access metrics, time-to-resolution in rights-based claims, and democratic participation indices in liberal-legalist regimes.',
    'If procedural costs and judicial supremacy exceed protective benefits for the median citizen, the broad citizen victim set strengthens the extraction profile and pushes classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(citizen_extraction_mechanism, empirical, 'Net direction of extraction for the citizen seat').

omega_variable(
    originalist_suppression_character,
    'Is the marginalization of originalist readings structural (institutional control of precedent, curriculum, and appointment) or internalized (originalists accepting liberal premises as baseline)?',
    'Examination of originalist representation in law school curricula, judicial clerkship pipelines, and published precedent; post-exit trajectory of scholars who abandon originalism.',
    'Structural suppression supports the authored suppression metric; internalized suppression indicates deeper accessibility collapse and identity-lock among legal elites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_suppression_character, conceptual, 'Structural versus internalized suppression of rival readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__liberal_due_process_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(magn_tr_t20, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(magn_tr_t40, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(magn_tr_t60, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(magn_tr_t80, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__liberal_due_process_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(magn_be_t20, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(magn_be_t40, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(magn_be_t60, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(magn_be_t80, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__liberal_due_process_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(magn_su_t20, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(magn_su_t40, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(magn_su_t60, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(magn_su_t80, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__liberal_due_process_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, feudal_prerogative_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__liberal_due_process_reading, originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Magna Carta Clause 39' conflates three structurally distinct interpretive constraints: a feudal privilege preserving hierarchical order, an originalist limitation to 1215-specific abuses, and a liberal universal due process doctrine. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by shared kernel text but separated by divergent normative commitments and institutional effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
