% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Basic Law Interpretive Authority
 *   domain: constitutional law / political theory / institutional design
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary_sovereignty_reading of the
 *   contested basic_law_interpretive_authority kernel. It holds that elected
 *   legislatures, by virtue of democratic mandate and representative
 *   accountability, possess final authority over the interpretation of basic
 *   law. The legislature enters the beneficiary set as the institutional seat
 *   of interpretive finality; the judiciary and rights-minorities enter the
 *   victim set because legislative override capacity structurally
 *   subordinates judicial independence and exposes minority protections to
 *   majoritarian will. The constraint coordinates constitutional meaning
 *   through a single democratically accountable locus while asymmetrically
 *   extracting interpretive autonomy from courts and security from
 *   minorities.
 *
 * KEY AGENTS:
 *   - elected_legislature: agenda_setter/beneficiary (institutional/constrained) â retains and exercises final interpretive authority
 *   - judiciary: payer (institutional/constrained) â interprets under shadow of legislative override
 *   - rights_minorities: payer (powerless/trapped) â protections subject to majoritarian legislative will
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.55).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Basic Law Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional law / political theory / institutional design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'd79f4b1a-ac21-45a6-9bfd-e399ac463695').
narrative_ontology:cs_kernel_codification('d79f4b1a-ac21-45a6-9bfd-e399ac463695', formalized).
narrative_ontology:cs_authority_grounding('d79f4b1a-ac21-45a6-9bfd-e399ac463695', lineage).
narrative_ontology:cs_interpretation_layer_present('d79f4b1a-ac21-45a6-9bfd-e399ac463695').
narrative_ontology:cs_reading_relation('d79f4b1a-ac21-45a6-9bfd-e399ac463695', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d79f4b1a-ac21-45a6-9bfd-e399ac463695', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('d79f4b1a-ac21-45a6-9bfd-e399ac463695', foundational, legislative_finality_as_democratic_mandate).
narrative_ontology:cs_axiom_status(legislative_finality_as_democratic_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d79f4b1a-ac21-45a6-9bfd-e399ac463695', legislative_finality_as_democratic_mandate, conventional).
narrative_ontology:cs_axiom('d79f4b1a-ac21-45a6-9bfd-e399ac463695', foundational, legislative_omnicompetence).
narrative_ontology:cs_axiom_status(legislative_omnicompetence, holdable).
narrative_ontology:cs_axiom_grounding('d79f4b1a-ac21-45a6-9bfd-e399ac463695', legislative_omnicompetence, conventional).
narrative_ontology:cs_reference_frame('d79f4b1a-ac21-45a6-9bfd-e399ac463695', legislative_finality_framework).
narrative_ontology:cs_drift_state('d79f4b1a-ac21-45a6-9bfd-e399ac463695', contemporary_rights_charter_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d79f4b1a-ac21-45a6-9bfd-e399ac463695', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains and exercises final authority over constitutional interpretation through ordinary or constitutional legislation; benefits from institutional supremacy over courts and enjoys democratic mandate as justification. While legally capable of abdicating this authority, political incentives and institutional identity make exit from the sovereignty position effectively constrained.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets legislation and constitutional questions under the shadow of legislative override. Possesses institutional standing and legal expertise but lacks final interpretive authority; strategic behavior is shaped by awareness that Parliament can reverse or preempt judicial decisions through subsequent legislation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judiciary, payer,
    institutional, generational, constrained, national).

% Groups relying on minority rights protections, entrenched constitutional guarantees, or judicially enforced limits on majority power find those protections subject to override by ordinary legislative majorities. Exit from the political community is costly or impossible; recourse depends on legislative self-restraint rather than enforceable institutional veto points.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, democratically accountable locus for resolving constitutional ambiguity and ensuring laws align with popular will, preventing persistent inter-institutional deadlock over basic law meaning.
% TRANSFER_FUNCTION: Moves final interpretive authority from courts and diffuse civic contestation to the elected legislature; transfers vulnerability to majoritarian override onto judicial independence and minority rights protections.
% ABSENT_VOICES: Constitutional courts practicing robust judicial review, entrenched rights advocates seeking supermajoritarian protections, and popular constitutionalists who would locate authority in ongoing democratic contestation rather than legislative fiat are structurally sidelined in this reading.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty vanished, courts would regain or assume final interpretive authority, rights protections would become more entrenched against ordinary legislative change, and the institutional balance between branches would shift fundamentally toward judicial or popular constitutionalism.
% FOUNDING_PROBLEM: Inter-institutional deadlock and democratic deficit when unelected judges or diffuse popular movements claim final constitutional authority over elected representatives; the perceived need for a clear, electorally accountable locus of constitutional decision-making.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereignty theorists (Diceyan tradition) and legislative institutionalists attest the problem of judicial overreach and democratic unaccountability. Critics from constitutional jurisprudence and rights advocacy attest that the problem is overstated and that legislative supremacy creates worse pathologies of majoritarian tyranny; no neutral consensus exists, and corroboration is split along institutional lines.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the arrangement systematically transfers interpretive finality from courts to the legislature and exposes minorities to majoritarian override. Suppression is moderate (0.55): parliamentary sovereignty must be actively maintained against judicial review traditions, international rights frameworks, and constitutional entrenchment movements. Theater ratio is modest (0.28) because the democratic mandate justification is substantively operative, though some sovereignty assertions are performative rather than exercised. Accessibility collapse is moderate (0.48) because alternatives such as judicial supremacy and popular constitutionalism remain visible and partially available in comparative constitutional discourse. Resistance is moderate (0.52) because courts and rights advocates actively resist legislative override in practice and theory.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative seat, the constraint appears as democratic coordination that prevents deadlock and preserves electoral accountability over constitutional evolution. From the judicial seat, it appears as structural subordination that constrains interpretive independence. From the rights-minority seat, it appears as exposure to majoritarian override with no institutional veto. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature is a declared beneficiary with institutional power and constrained but present exit options (it could theoretically reform the constitution), placing its directionality near the beneficiary end. The judiciary is a declared victim with institutional power but constrained exit from the legal system, producing high directionality toward the target end despite its formal standing. Rights-minorities are declared victims with low power and trapped exit options, placing them nearest the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the genuine coordination function â a single democratically accountable arbiter of constitutional meaning â this arrangement would read as pure majoritarian domination (snare). Without the asymmetric extraction from courts and minorities, it would read as pure coordination (rope). The Tangled Rope classification captures that both properties are structurally present: democratic accountability is real, and judicial subordination and minority vulnerability are real. Mislabeling as either pure coordination or pure extraction would miss half the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is parliamentary sovereignty a natural or necessary reading of democratic constitutionalism, or a contingent institutional construction that benefits legislative majorities?',
    'Comparative constitutional history tracing whether parliamentary sovereignty emerged inevitably from democratic principles or only from specific contingent paths (Westminster development, colonial inheritance, unitary state formation).',
    'If contingent, the reading''s authority depends on continued democratic choice rather than structural necessity, and its classification stability weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether this kernel reading is structurally necessary or historically contingent').

omega_variable(
    legislative_override_empirical_frequency,
    'How frequently do legislatures actually exercise override authority over judicial rights protections, and does latent power differ from exercised extraction?',
    'Cross-jurisdictional empirical study of legislative override rates in Westminster-derived systems, comparing jurisdictions with and without entrenched rights instruments.',
    'High override frequency would confirm active extraction from rights-minorities; rare override with preserved latent authority would suggest the constraint operates more as background threat than active snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_override_empirical_frequency, empirical, 'Empirical gap between latent legislative authority and exercised override').

omega_variable(
    judicial_independence_erosion_under_latent_authority,
    'Does parliamentary sovereignty structurally erode judicial independence even when override powers are rarely exercised, via anticipatory obedience or strategic restraint?',
    'Qualitative analysis of judicial reasoning in Westminster systems measuring anticipatory deference to foreseeable legislative override.',
    'If erosion occurs even without explicit override, the constraint''s effective extraction exceeds its overt exercise and the victim set is broader than recorded instances suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_erosion_under_latent_authority, empirical, 'Whether latent legislative supremacy produces judicial self-censorship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blia_psr_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(blia_psr_tr_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(blia_psr_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(blia_psr_tr_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(blia_psr_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(blia_psr_tr_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(blia_psr_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(blia_psr_be_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(blia_psr_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(blia_psr_be_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(blia_psr_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(blia_psr_be_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(blia_psr_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(blia_psr_su_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(blia_psr_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(blia_psr_su_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(blia_psr_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(blia_psr_su_t50, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_authority kernel, which decomposes into structurally distinct claims about institutional authority. Parliamentary sovereignty, judicial supremacy, and popular constitutionalism assign final interpretive authority to different agents and generate different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
