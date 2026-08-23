% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity Doctrine (Accountability Void Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement
 *
 * SUMMARY:
 *   This constraint story instantiates the accountability_void_reading of the
 *   qualified_immunity_doctrine kernel. The doctrine requires plaintiffs in
 *   constitutional-tort actions to show that the right violated was 'clearly
 *   established' at the time of conduct â a standard that operates as a
 *   near-absolute bar to liability. Under this reading, the doctrine is not a
 *   protective coordination mechanism but a systematic extraction
 *   arrangement: it transfers the costs of constitutional violations from
 *   officers to victims, guarantees impunity, and is actively maintained by
 *   the federal judiciary despite mounting evidence of its
 *   accountability-void effects. The reading is distinguished from its
 *   siblings by its empirical focus on victim remedy extraction and its
 *   normative claim that constitutional accountability is non-waivable.
 *
 * KEY AGENTS:
 *   - civil_rights_plaintiffs: Primary target (powerless/trapped) â bears extraction through barred remedies.
 *   - government_officers: Primary beneficiary (powerful/constrained) â shielded from personal liability.
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â administers the doctrine and controls its expansion.
 *   - government_employers: Secondary beneficiary (institutional/constrained) â enjoys reduced indemnification and discovery exposure.
 *   - legal_reform_advocates: Analytical observer (organized/analytical) â documents the accountability void and resists doctrinally.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.92).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.85).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity Doctrine (Accountability Void Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '072d910b-4a25-4025-93a6-e69502cfa7e3').
narrative_ontology:cs_kernel_codification('072d910b-4a25-4025-93a6-e69502cfa7e3', fixed_text).
narrative_ontology:cs_authority_grounding('072d910b-4a25-4025-93a6-e69502cfa7e3', extraction).
narrative_ontology:cs_interpretation_layer_present('072d910b-4a25-4025-93a6-e69502cfa7e3').
narrative_ontology:cs_reading_relation('072d910b-4a25-4025-93a6-e69502cfa7e3', qualified_immunity_doctrine__protective_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('072d910b-4a25-4025-93a6-e69502cfa7e3', qualified_immunity_doctrine__constitutional_fidelity_reading, coexists_with).
narrative_ontology:cs_axiom('072d910b-4a25-4025-93a6-e69502cfa7e3', foundational, constitutional_remedy_non_waivable).
narrative_ontology:cs_axiom_status(constitutional_remedy_non_waivable, holdable).
narrative_ontology:cs_axiom_grounding('072d910b-4a25-4025-93a6-e69502cfa7e3', constitutional_remedy_non_waivable, deontological).
narrative_ontology:cs_axiom('072d910b-4a25-4025-93a6-e69502cfa7e3', foundational, qualified_immunity_creates_impunity).
narrative_ontology:cs_axiom_status(qualified_immunity_creates_impunity, holdable).
narrative_ontology:cs_axiom_grounding('072d910b-4a25-4025-93a6-e69502cfa7e3', qualified_immunity_creates_impunity, empirically_contingent).
narrative_ontology:cs_reference_frame('072d910b-4a25-4025-93a6-e69502cfa7e3', constitutional_tort_accountability).
narrative_ontology:cs_drift_state('072d910b-4a25-4025-93a6-e69502cfa7e3', post_doctrinal_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('072d910b-4a25-4025-93a6-e69502cfa7e3', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, government_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, government_employers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual officers who benefit from the near-absolute shield against personal liability for constitutional violations. They are relieved of the deterrent and financial costs that ordinary tort liability would impose, allowing conduct to proceed without personal consequence.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, government_officers, beneficiary,
    powerful, biographical, constrained, national).

% Municipalities, states, and agencies that employ officers and benefit from reduced indemnification pressure, dampened discovery exposure, and a chilled plaintiff bar that rarely reaches settlement or trial.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, government_employers, beneficiary,
    institutional, generational, constrained, national).

% Individuals alleging constitutional violations by government actors. They bear the uncompensated harm and the litigation costs, and are systematically barred from recovery by the clearly established law standard, which functions as a moving target operated by the judiciary.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs, payer,
    powerless, immediate, trapped, national).

% Federal courts that create, interpret, and apply the qualified immunity doctrine. They frame the doctrine as balancing litigation burdens against constitutional remedies, while functioning as the active gatekeeper that extracts remedies from plaintiffs through pretrial dismissal.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Civil rights organizations and scholars who document the doctrine's accountability void and lobby for statutory or judicial repeal. They resist through amicus briefs, model legislation, and doctrinal critique but do not control the doctrine's application.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, legal_reform_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinates the balance between civil rights remediation and officer protection; under this reading, the arrangement solves no live coordination problem at the margin â the protective rationale operates as cover for systematic impunity.
% TRANSFER_FUNCTION: Moves the cost of constitutional violations from government officers and their employers to the victims, by extracting the remedy of damages and eliminating the deterrent of personal liability.
% ABSENT_VOICES: Victims whose claims are never filed because attorneys decline unwinnable qualified immunity cases; local communities bearing the externalized costs of unaccountable policing; and officers who would prefer clearer legal guidance but are subordinated to the institutional interest in liability avoidance. They are excluded from the judicial forum by the doctrine's procedural bar and by the institutional framing that treats officer protection as synonymous with public safety.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, Section 1983 suits would proceed to discovery and trial, officers and municipalities would face personal and indemnification liability, plaintiff attorneys would re-enter the market, and the incentive structure for constitutional compliance would shift dramatically â the civil rights enforcement landscape would reorganize around accountability rather than impunity.
% FOUNDING_PROBLEM: The perceived threat that fear of personal liability and voluminous litigation would deter capable individuals from government service and paralyze official decision-making.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and critical scholars outside the beneficiary set attest that the modern doctrine, invented in Pierson v. Ray (1967) and expanded in Harlow v. Fitzgerald (1982), bears little resemblance to any common-law good-faith defense and far exceeds the original litigation-burden rationale. The beneficiary set self-asserts the problem remains live, but no independent corroboration supports the scale of the modern shield.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.92, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because the 'clearly established law' test functions as a near-absolute shield, dismissing meritorious suits before discovery. Suppression is high (0.85) because the judicial enforcement of the doctrine actively collapses the alternative remedy path that Section 1983 appears to guarantee. Theater ratio is elevated (0.60) because courts perform elaborate 'clearly established' analyses that are functionally predetermined, generating legitimizing ritual without altering outcomes. Accessibility collapse is severe (0.92): once a plaintiff is subject to qualified immunity, the alternative of obtaining damages or deterrence through federal litigation nearly vanishes. Resistance is moderate (0.45): sustained scholarly and advocacy criticism exists, and occasional legislative repeal efforts have emerged, but the judicial lock-in has prevented structural change. The claim/metric independence is maintained: the accountability_void_reading claims snare, and the metrics describe highly extractive, actively suppressed operation.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and government officers experience the constraint as a necessary and lawful protection of official discretion; civil rights plaintiffs experience it as a systematic denial of remedy. The engine computes this divergence from the structural data: beneficiaries (officers, employers) sit at low directionality with constrained but protected exit, while payers (plaintiffs) sit at high directionality with trapped exit. The agenda-setter (judiciary) is analytical and mobile (it could overturn the doctrine), which dampens its extraction â yet it actively enforces the barrier, creating the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to government_officers and government_employers, who collect the shield against liability and the reduced fiscal exposure. Victim declarations map to civil_rights_plaintiffs, who bear the uncompensated constitutional harm and the denial of Section 1983's remedial promise. The federal judiciary is the agenda_setter: it does not collect financial rents but exercises institutional power through the doctrine's administration, which gives it a distinct directional position. Legal reform advocates are observers with analytical exit. No override is needed: the structural derivation produces accurate d values â beneficiaries near 0.0, payers near 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   The accountability_void_reading prevents mislabeling by requiring identifiable victims with no remedy path and beneficiaries who are shielded from consequences. If the doctrine were merely a piton, it would show high theater and diffuse costs with no concentrated beneficiary; here, officers are concentrated beneficiaries and plaintiffs are concentrated victims, so the classification is snare. If it were a tangled rope, there would need to be a genuine coordination function alongside the extraction; this reading denies that the protective function is genuine at the margin, so the tangled rope gate is not met. The founding problem (protecting officers from bad-faith litigation) is read as dead, and the doctrine persists as a zombie extraction mechanism â but because the beneficiary set remains concentrated and the victim set remains defined, the classification stays snare rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the accountability_void_reading of the qualified_immunity_doctrine kernel. Would adopting the protective_scaffold_reading''s premise (that immunity solves a live coordination problem) reduce epsilon below the snare threshold, or would the asymmetric extraction remain dominant regardless of framing?',
    'Comparative analysis of the sibling reading''s metrics and structural declarations; if the same beneficiary/victim structure persists with high extraction, the kernel is a tangled rope regardless of reading, but if the victim set empties, the protective scaffold reading may be a genuine rope.',
    'Determines whether the kernel is a family of distinct constraints (different epsilon, different referents) or competing framings of the same structure. Under the epsilon-invariance principle, different referents require different stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Kernel reading location and sibling structural delta for accountability_void_reading').

omega_variable(
    doctrinal_invention_vs_evolution,
    'Is qualified immunity a judicial invention without statutory or common-law basis (constitutional_fidelity_reading), or an evolution of pre-existing protections (protective_scaffold_reading)?',
    'Historical legal analysis of the 1871 Civil Rights Act legislative record and 1960s common-law good-faith defenses.',
    'If invented, the doctrine''s authority rests on extraction (judicial power preserving its own creation); if evolved, it may retain coordination legitimacy. This changes authority_grounding from extraction to lineage/practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_invention_vs_evolution, empirical, 'Whether the doctrine was invented or evolved').

omega_variable(
    remedy_extraction_vs_officer_protection,
    'Does the doctrine''s high extraction stem from the necessary cost of protecting officers, or from a structural decoupling of the shield from any genuine protection need?',
    'Natural experiment from jurisdictions that have abolished or limited qualified immunity (e.g., Colorado, New Mexico): if officer behavior and recruitment remain stable while plaintiff recovery rises, the extraction is separable from the coordination function.',
    'If separable, the constraint is a snare using officer protection as cover; if inseparable, it may be a tangled rope where the extraction is the unavoidable price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_extraction_vs_officer_protection, empirical, 'Whether extraction is separable from protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qual_tr_t10, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(qual_tr_t20, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(qual_tr_t30, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 40, 0.55).
narrative_ontology:measurement(qual_tr_t55, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 55, 0.6).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qual_be_t10, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(qual_be_t20, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(qual_be_t30, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(qual_be_t55, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 55, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qual_su_t10, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(qual_su_t20, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(qual_su_t30, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(qual_su_t55, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 55, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
