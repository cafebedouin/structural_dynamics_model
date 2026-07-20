% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitutionalism: Evolving Aspirational Framework
 *   domain: constitutional law / legal theory / political philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the living reading of the
 *   us_constitution_1787 kernel. The colloquial label 'the Constitution'
 *   covers multiple structurally distinct interpretive constraints; this
 *   story isolates the claim that constitutional meaning evolves with society
 *   and the text functions as an aspirational framework. Sibling readings
 *   (originalist and positivist) are separate constraints with different
 *   epsilon values and beneficiary/victim structures, linked via
 *   network.affects_constraints. The living reading expands the constraint
 *   set by judicial discovery of modern rights claims (privacy, dignity) but
 *   lowers epistemic demands, creating vulnerability to elite capture of
 *   'evolving norms'.
 *
 * KEY AGENTS:
 *   - federal_judiciary (agenda_setter / institutional / analytical exit) â administers the interpretive method and expands its own authority
 *   - rights_advocacy_coalitions (beneficiary / organized / constrained exit) â collect rights victories without amendment campaigns
 *   - democratic_legislatures (payer / institutional / constrained exit) â lose policy authority to judicial review
 *   - originalist_citizens (payer / moderate / constrained exit) â bear the cost of an unsettled constitutional text
 *   - comparative_constitutional_scholars (observer / analytical / analytical exit) â analyze the comparative legitimacy of adaptive interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.58).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.6).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitutionalism: Evolving Aspirational Framework").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional law / legal theory / political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, '44d431b3-f6ea-4286-b8cd-62a354c8d347').
narrative_ontology:cs_kernel_codification('44d431b3-f6ea-4286-b8cd-62a354c8d347', formalized).
narrative_ontology:cs_authority_grounding('44d431b3-f6ea-4286-b8cd-62a354c8d347', lineage).
narrative_ontology:cs_interpretation_layer_present('44d431b3-f6ea-4286-b8cd-62a354c8d347').
narrative_ontology:cs_reading_relation('44d431b3-f6ea-4286-b8cd-62a354c8d347', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('44d431b3-f6ea-4286-b8cd-62a354c8d347', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('44d431b3-f6ea-4286-b8cd-62a354c8d347', foundational, constitutional_text_aspirational_framework).
narrative_ontology:cs_axiom_status(constitutional_text_aspirational_framework, holdable).
narrative_ontology:cs_axiom_grounding('44d431b3-f6ea-4286-b8cd-62a354c8d347', constitutional_text_aspirational_framework, conventional).
narrative_ontology:cs_axiom('44d431b3-f6ea-4286-b8cd-62a354c8d347', foundational, societal_evolution_generates_binding_rights).
narrative_ontology:cs_axiom_status(societal_evolution_generates_binding_rights, holdable).
narrative_ontology:cs_axiom_grounding('44d431b3-f6ea-4286-b8cd-62a354c8d347', societal_evolution_generates_binding_rights, deontological).
narrative_ontology:cs_reference_frame('44d431b3-f6ea-4286-b8cd-62a354c8d347', living_constitutional_order).
narrative_ontology:cs_drift_state('44d431b3-f6ea-4286-b8cd-62a354c8d347', contemporary_political_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44d431b3-f6ea-4286-b8cd-62a354c8d347', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, rights_advocacy_coalitions).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, democratic_legislatures).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises the power of constitutional review to interpret the text as an evolving aspirational framework, discovering new rights and applying contemporary values to old text. This role expands judicial authority relative to the elected branches and entrenches the Court as the final arbiter of constitutional meaning.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Litigate to establish emerging rights under the evolving-norms framework, bypassing the need for constitutional amendments that would require broader political consensus. They depend on receptive courts to vindicate claims that lack explicit textual basis.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, rights_advocacy_coalitions, beneficiary,
    organized, generational, constrained, national).

% Enact legislation that may be struck down by courts invoking evolving standards of privacy, dignity, or liberty. Their democratic policymaking authority is constrained by judicially discovered rights that lack textual or historical pedigree.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, democratic_legislatures, payer,
    institutional, biographical, constrained, national).

% Hold a constitutional understanding fixed to ratification-era meaning; their interpretive expectations and policy preferences are overridden by decisions that treat the text as aspirational and evolving. They bear the cost of a constitutional settlement they did not agree to.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_citizens, payer,
    moderate, biographical, constrained, national).

% Study constitutional adaptability across regimes; some defend the living reading as necessary for rights protection in diverse societies, others warn of democratic deficit and elite capture inherent in judge-led updating.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permits a written constitution to adapt to unforeseeable social, technological, and moral developments without relying solely on a supermajoritarian amendment process that may be too rigid to secure emergent rights against entrenched majorities.
% TRANSFER_FUNCTION: Moves authority to define constitutional rights and invalidate legislation from democratic legislatures and formal amendment processes to federal courts and organized rights advocates, who can effectuate constitutional change through interpretation rather than ratification.
% ABSENT_VOICES: Majoritarian populations whose legislation is invalidated and citizens without access to constitutional litigation are structurally absent from the interpretive process; their exclusion is inherent to a method that empowers judicial elites and advocacy organizations to speak for evolving norms.
% DISAPPEARANCE_RATIONALE: If the living reading vanished, the doctrinal foundations for unenumerated rights such as privacy, contraception, and same-sex intimacy would collapse unless replaced by amendments or statutes; judicial power to invalidate legislation would contract to textually explicit or historically established rights, and constitutional politics would shift to amendment fights and legislative channels.
% FOUNDING_PROBLEM: A fixed constitutional text ratified in 1787 cannot anticipate every future rights claim or social condition; relying only on formal amendment risks entrenching outdated norms and preventing necessary protections for marginalized groups.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians and comparative constitutionalists attest that rigid amendment-only systems fail marginalized groups. Originalist jurists and democratic theorists attest that the amendment process is functioning as designed and that judicial updating is elite circumvention. No source outside the dispute universally corroborates the problem's status.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__living_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the living reading permits courts to invalidate democratically enacted legislation based on norms not articulated in the constitutional text, transferring authority without ratification. Suppression (0.60) reflects that alternative interpretive methods (originalism, strict textualism) are actively argued but institutionally disadvantaged in courts that have adopted the living framework. Theater ratio (0.40) captures the performative dimension of opinions that frame elite constitutional values as organic societal evolution. Accessibility collapse (0.48) registers that while originalism remains intellectually available, it is institutionally collapsed as a winning argument before living-reading courts. Resistance (0.55) measures the sustained originalist political and jurisprudential backlash. The measurement series share a single time grid (0â100) to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the arrangement is necessary coordination against constitutional obsolescence; from the legislative and originalist seats, the same structure operates as extraction of democratic authority. The engine computes this divergence from structural data rather than adjudicating it. The comparative constitutional scholar seat sees the divergence as a systematic feature of judge-led constitutional updating.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary and rights advocacy coalitions are declared beneficiaries: the judiciary gains institutional power and final interpretive authority, while advocates secure constitutional protections without the high transaction costs of amendment. Their directionality is low (near beneficiary). Democratic legislatures and originalist citizens are declared victims: legislatures lose policymaking autonomy to judicial review, and citizens lose the fixed interpretive settlement they expected. Their directionality is high (near target). The engine will compute per-seat classifications that diverge accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â constitutional obsolescence â is contested but not dead. The coordination function remains partially live (some rights gains may not have passed amendment processes). However, the extraction function has grown through elite capture, preventing the constraint from resolving into a piton or scaffold. It is not a snare because the coordination function is genuine and historically significant; it is not a rope because asymmetric extraction is structurally present. The constraint persists as tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_capture_of_evolving_norms,
    'Does the ''evolving norms'' interpretive method genuinely track broad societal consensus, or has it been captured by judicial and academic elites who impose their own values?',
    'Empirical mapping of judicial outcomes against contemporaneous public opinion, legislative trends, and state constitutional conventions; sustained divergence indicates elite capture.',
    'If captured, the coordination function weakens and the constraint shifts toward snare; if genuinely tracking society, the tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Whether evolving-norm interpretation reflects society or elite preferences.').

omega_variable(
    kernel_reading_boundary,
    'This constraint is the living reading of the us_constitution_1787 kernel. Would adopting the originalist reading instead produce a structurally different classification, and where exactly do the readings diverge?',
    'Compare the compiled constraint stories for each reading: the originalist reading likely claims lower extractiveness and names different beneficiaries (textualist majorities) and victims (rights-seekers blocked by fixed text).',
    'The divergence location (beneficiary/victim inversion) confirms that the kernel must be decomposed into multiple constraints per the epsilon-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural divergence between living and originalist readings of the same constitutional text.').

omega_variable(
    democratic_legitimacy_of_judicial_updating,
    'Is the living reading''s bypassing of formal amendment a necessary evil for rights protection, or an illegitimate transfer of democratic authority to unelected judges?',
    'Comparative analysis of rights protection in jurisdictions with strict amendment requirements versus those with flexible interpretation; historical case studies of rights gained through interpretation that amendments could not secure.',
    'If the amendment process is demonstrably capable of securing rights, the coordination justification weakens and extraction dominates; if incapable, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_of_judicial_updating, preference, 'Normative ambiguity about democratic legitimacy of interpretive updating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscl_tr_t0, us_constitution_1787__living_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(uscl_tr_t20, us_constitution_1787__living_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(uscl_tr_t40, us_constitution_1787__living_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(uscl_tr_t60, us_constitution_1787__living_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(uscl_tr_t80, us_constitution_1787__living_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(uscl_tr_t100, us_constitution_1787__living_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(uscl_be_t0, us_constitution_1787__living_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(uscl_be_t20, us_constitution_1787__living_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(uscl_be_t40, us_constitution_1787__living_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(uscl_be_t60, us_constitution_1787__living_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(uscl_be_t80, us_constitution_1787__living_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(uscl_be_t100, us_constitution_1787__living_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uscl_su_t0, us_constitution_1787__living_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(uscl_su_t20, us_constitution_1787__living_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(uscl_su_t40, us_constitution_1787__living_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(uscl_su_t60, us_constitution_1787__living_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(uscl_su_t80, us_constitution_1787__living_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(uscl_su_t100, us_constitution_1787__living_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Constitution' decomposes into at least three structurally distinct constraints because different interpretive readings produce different epsilon values, beneficiary structures, and enforcement mechanisms. This story instantiates the living reading; sibling stories instantiate originalist and positivist readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
