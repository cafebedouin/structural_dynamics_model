% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Adaptive Framework (Evolutionary Reading)
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   The common law precedent corpus operates under multiple interpretive
 *   readings. The evolutionary-framework reading holds that precedent
 *   provides an adaptive structure: the precedent-binding rule permits
 *   overruling when normative understanding has evolved sufficiently that old
 *   precedent would perpetuate injustice. This reading empowers appellate
 *   courts as normative updaters and opens pathways for litigants challenging
 *   settled law on grounds of evolved understanding. The constraint is
 *   claimed as a rope (coordination of reliance with adaptive capacity) and
 *   the metrics reflect moderate extractiveness and suppression: the
 *   framework extracts authority from reliance interests and lodges it in the
 *   judiciary, but does not eliminate alternatives (strict stare decisis
 *   remains available through constitutional amendment or legislative
 *   override). The evolutionary reading is one of three live positions in the
 *   kernel contest; it coexists with strict stare decisis (precedent binds
 *   backward absolutely) and pluralist balancing (precedent weight varies by
 *   domain). This story instantiates the evolutionary reading only.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: institutional agenda-setter, interprets precedent as adaptable, normalizes overruling for normative reasons
 *   - reformist_litigants: moderate power, beneficiaries of the framework, gain access to normative challenge
 *   - settled_reliance_interests: powerful but constrained, bear the cost of normalized overruling
 *   - civil_rights_claimants: powerless and trapped, historically benefited from evolutionary overruling (segregation, coverture), would lose access to normative challenge if framework closed
 *   - conservative_legal_tradition: excluded from the framework's legitimacy account, advocates strict stare decisis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.38).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.28).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Adaptive Framework (Evolutionary Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '51e2c207-3264-4a36-ae57-867c1a522169').
narrative_ontology:cs_kernel_codification('51e2c207-3264-4a36-ae57-867c1a522169', fixed_text).
narrative_ontology:cs_authority_grounding('51e2c207-3264-4a36-ae57-867c1a522169', lineage).
narrative_ontology:cs_interpretation_layer_present('51e2c207-3264-4a36-ae57-867c1a522169').
narrative_ontology:cs_reading_relation('51e2c207-3264-4a36-ae57-867c1a522169', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('51e2c207-3264-4a36-ae57-867c1a522169', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('51e2c207-3264-4a36-ae57-867c1a522169', foundational, precedent_permits_normative_evolution).
narrative_ontology:cs_axiom_status(precedent_permits_normative_evolution, holdable).
narrative_ontology:cs_axiom_grounding('51e2c207-3264-4a36-ae57-867c1a522169', precedent_permits_normative_evolution, deontological).
narrative_ontology:cs_axiom('51e2c207-3264-4a36-ae57-867c1a522169', foundational, judiciary_has_authority_to_correct_injustice).
narrative_ontology:cs_axiom_status(judiciary_has_authority_to_correct_injustice, holdable).
narrative_ontology:cs_axiom_grounding('51e2c207-3264-4a36-ae57-867c1a522169', judiciary_has_authority_to_correct_injustice, deontological).
narrative_ontology:cs_reference_frame('51e2c207-3264-4a36-ae57-867c1a522169', precedent_as_living_law).
narrative_ontology:cs_drift_state('51e2c207-3264-4a36-ae57-867c1a522169', contemporary_rights_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('51e2c207-3264-4a36-ae57-867c1a522169', '2026-06-12T14:22:00Z').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, reformist_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, normative_adapters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, civil_rights_claimants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, settled_reliance_interests).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, law_evolves_with_society).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, judicial_prudence_permits_reinterpretation).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__evolutionary_framework, precedent_serves_justice_not_rigidity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets precedent as a living framework permitting reinterpretation in light of contemporary normative understanding. Writes opinions overruling prior holdings when social conditions, scientific understanding, or moral comprehension have shifted. Claims authority to correct the law when injustice persists under old precedent. Maintains the doctrine but modulates its application through doctrine-development and exception-carving.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a precedent framework that permits challenge on grounds of evolved normative understanding. Can argue that settled law was built on outdated premises (e.g., scientific misconceptions, now-rejected moral categories). Their exit option is litigating to courts that adopt the evolutionary framework; if the framework closes, they return to the impossible task of winning under openly unjust precedent.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reformist_litigants, beneficiary,
    moderate, biographical, mobile, national).

% Organized around stable precedent; bear the cost of overruling when it disrupts settled expectations. Property owners relying on real property precedent, businesses structured under settled commercial law, property settlements resting on family law holdings—all face instability when the appellate judiciary normalizes overruling for evolutionary reasons. Cannot exit the legal system; can only advocate for stricter binding precedent.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, settled_reliance_interests, payer,
    powerful, biographical, constrained, national).

% Sit between litigants and appellate guidance. Under the evolutionary framework, they face ambiguity about how much evolution is permissible at their level, which precedent is stable enough to apply, and when to signal that overruling may be warranted. They do not set the framework but live under its uncertainty.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_courts, observer,
    institutional, generational, analytical, national).

% Interpret and debate the framework's legitimacy. The evolutionary reading is defended by scholars arguing law must track moral and social progress; attacked by scholars defending precedential stability. They shape doctrine through treatises, law review articles, and amicus briefs, but do not control the judiciary's adoption of the reading.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_academics, observer,
    moderate, biographical, mobile, national).

% Have historically benefited from evolutionary overruling—precedent that treated them as legally inferior or subordinate persons overruled when normative understanding shifted (segregation, coverture, exclusion from professions). Under the evolutionary framework, their claim that precedent reflects injustice rather than settled law is cognizable in court. If the framework reverted to strict stare decisis, they would have no judicial pathway for normative challenge.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, civil_rights_claimants, beneficiary,
    powerless, biographical, trapped, national).

% Would argue that normalizing overruling for evolutionary reasons destabilizes law itself—precedent's entire function is to bind future cases and prevent each generation from remaking law according to its preferences. They would contend that 'evolved normative understanding' is a disguise for judicial policy preferences. Excluded from the dominant institutional reading but present in legislative calls for originalism and in dissenting opinions.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, conservative_legal_tradition, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for legal certainty while preserving capacity to correct systemic injustice: precedent coordinates reliance, but the framework permits judicial evolution when moral understanding advances and the old precedent conflicts with justice.
% TRANSFER_FUNCTION: Moves legitimacy from settled precedent toward appellate judiciary as normative updater; empowers litigants challenging old precedent on grounds of evolved understanding; weakens reliance interests in stable rules. The constraint redistributes authority to rewrite law from the conservative binding-precedent seat to the progressive adaptation seat.
% ABSENT_VOICES: Conservative legal tradition (strict stare decisis advocates, statutory textualists) would protest that normalizing overruling destroys precedent's binding force and vests too much authority in appellate judges to remake law according to their normative views. They are excluded from the framework's own legitimacy account, though present in dissent and in legislative originalism movements.
% DISAPPEARANCE_RATIONALE: If this framework vanished and strict stare decisis became absolute, civil rights litigants would lose the only mechanism to challenge precedent as unjust; settled reliance interests would gain absolute protection; and appellate courts would lack normative authority to correct law that has become clearly unjust. The entire edifice of twentieth-century constitutional evolution (expansion of civil rights, correction of scientific errors in precedent) rested on the assumption that precedent can be overruled for good reasons.
% FOUNDING_PROBLEM: Early common law operated under strict adherence to precedent, which locked in outcomes that became clearly unjust as society's moral understanding evolved and scientific understanding improved. The problem: how can law adapt to new knowledge and morality without abandoning predictability? How can courts correct injustice when the precedent-binding rule prevents them from doing so?
% FOUNDING_PROBLEM_CORROBORATION: Appellate judges and civil rights scholars attest the problem remains live: precedent constrains correction of injustice. Conservative legal scholars and reliance-interest advocates attest the problem is overdrawn: what we call 'evolved understanding' is often judicial policy preference, and the real problem is judicial overreach disguised as adaptation. Legislative testimony on both sides of the originalism debate reflects the dispute.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the framework redistributes authority but does not eliminate alternatives—conservative majorities can still bind successor courts through supermajority constitutionalization (as with originalism movements) or legislate to override precedent. Suppression is moderate-low (0.28) because the framework permits explicit challenge (overruling arguments are cognizable in court and do not face structural barriers to litigation). Theater is low (0.22) because the framework's main function is genuine (permitting correction of precedent that has become unjust); the rising theater ratio over time reflects increasing doctrinal complexity and exception-carving as courts signal which precedent is stable and which is evolutionarily revisable—this is performative boundary-maintenance growing more elaborate as the framework ages. Measurement series show extraction rising in the early interval (as the reading consolidates in case law) then stabilizing once the norm of evolutionary overruling is established; theater rises and plateaus as courts develop signals for litigants about which holdings are evolutionarily settled. Suppression_requirement rises to accommodate growing reliance-interest advocacy for stricter binding rules.
 *
 * PERSPECTIVAL GAP:
 *   The appellate judiciary and reformist-litigant seats should compute as beneficiaries with low directionality (empowered, gaining access); settled reliance interests should compute as targets with higher directionality (constrained by unpredictability, losing security). Lower courts sit near the center—they gain interpretive flexibility but lose clarity about precedent's binding force. The engine computes these divergences from the structural data. The reading itself is organized to show the evolutionary-framework interpretation; seats adopting strict stare decisis would compute differently from the same constraint skeleton, which is why they are sister readings in the kernel, not seats within this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary benefits from authority to adapt law (low d, beneficiary). Reformist litigants benefit from access to normative challenge (low-moderate d, beneficiary). Settled reliance interests lose security of binding precedent (high d, moderate burden). Civil rights claimants are trapped and powerless but benefit historically from the framework, creating a complex d picture: their mobility is zero but their benefit is structural (life depends on the possibility of overruling unjust precedent); d sits moderate reflecting the tension between powerlessness and essential dependence on the framework's permission to challenge. Conservative legal tradition is excluded: they would experience the evolutionary reading as extraction of binding force, but they are not named stakeholders here because they reject the framework's legitimacy—including them would require authoring them as payers under a framework they deny (a modeling error). The framework itself excludes them by not recognizing their reading as legitimate; they are present as resistance (high resistance value reflects their ongoing advocacy for strict stare decisis).
 *
 * MANDATROPHY ANALYSIS:
 *   The evolutionary framework's founding problem (how to correct precedent that has become unjust) remains live and hotly contested. The framework has not degraded into mere performance; courts genuinely do overrule precedent for evolutionary reasons, and litigants genuinely do use normative-evolution arguments. The framework shows no mandatrophy signature. If the theater ratio rises further, it would signal that evolutionary overruling has become doctrinal theater—courts overrule for form but provide narrow escape routes that neutralize the correction. Currently, the framework does what it was built to do: permit adaptive precedent while preserving some reliance interest. No mandatrophy verdict is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolved_understanding_vs_judicial_preference,
    'When courts claim precedent reflects ''outdated understanding'' and overrule it, are they tracking genuine evolution in normative or empirical knowledge, or are they enacting judicial policy preferences under a cover story?',
    'Track overruling decisions against: (a) consensus shifts in philosophical/scientific disciplines outside the judiciary, (b) legislative evolution in peer democratic bodies, (c) international legal consensus. If overrulings track external consensus, the reading is accurate; if they diverge from or precede external consensus, the reading describes judicial policy preference with a normative cover.',
    'If judicial preferences are driving overruling, the framework is snare-like (extraction of binding constraint, disguised as adaptation). If genuine consensus shifts are driving it, the framework is rope-like (coordination for justice with the cost of reliance uncertainty).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolved_understanding_vs_judicial_preference, empirical, 'Whether evolutionary overruling tracks external normative consensus or judicial preferences.').

omega_variable(
    reading_foreclosure_question,
    'Does the evolutionary reading''s core premise—that precedent permissibly evolves with normative understanding—logically foreclose strict stare decisis, or do both readings remain live positions that can coexist in different jurisdictions or legal traditions?',
    'Test via jurisdictional variation: if both readings remain held by different supreme courts or legal systems with equally valid foundational premises, they coexist. If one reading logically requires denying the other''s core premise (e.g., if evolutionary reading requires denying that binding precedent can ever be absolute), they foreclose. Current evidence suggests coexistence, not foreclosure.',
    'If foreclosure: the three readings form a foreclosure chain and only one can survive long-term institutional selection. If coexistence: they remain live alternatives, and the constraint represents genuine contest over the kernel, not resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_question, conceptual, 'Whether the evolutionary and strict-stare-decisis readings are logically incompatible or just different institutional choices.').

omega_variable(
    suppression_mechanism_structurality,
    'Is the measured suppression (constraints on overruling, procedural barriers to normative challenge, costs to reformist litigants) structural (external barriers, formal doctrine, resource costs) or internalized (litigants have absorbed the precedent-binding frame as legitimate)?',
    'Observe litigant behavior post-exit: if jurisdiction adopts strict stare decisis, do reformist litigants quickly mobilize alternative forums (coexistence indicators) or do they abandon normative-challenge strategies (internalized suppression)? If they mobilize, suppression is structural and reversible; if they abandon, the frame is internalized.',
    'If suppression is structural, the framework''s effective extraction is the measured 0.28; if internalized, the constraint carries the suppression with the litigants even if the framework closes, suggesting deeper institutional capture of the normative challenge capacity itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structurality, empirical, 'Whether suppression of reliance interests in the framework is structural or internalized.').

omega_variable(
    sibling_reading_empirical_basis,
    'What empirical evidence would resolve whether the evolutionary reading or strict stare decisis better predicts stable, just legal outcomes? Is there a fact that would favor one reading over the other?',
    'Compare long-run institutional outcomes: stability of doctrine, incidence of future overruling, public confidence in judiciary, litigation costs, rights protection for minorities. If evolutionary reading leads to instability and whipsaw, strict stare decisis is empirically superior; if strict stare decisis leads to locked-in injustice and social instability, evolutionary reading is empirically superior.',
    'Resolution would favor one reading''s foundational axiom as grounded in better empirical consequences, potentially foreclosing the other if consequences diverge sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_basis, empirical, 'What observable outcomes would empirically vindicate one reading over another.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(comm_tr_t0, projected).
narrative_ontology:measurement(comm_tr_t8, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(comm_tr_t8, observed).
narrative_ontology:measurement(comm_tr_t16, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(comm_tr_t16, observed).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 24, 0.19).
narrative_ontology:measurement_basis(comm_tr_t24, observed).
narrative_ontology:measurement(comm_tr_t32, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 32, 0.21).
narrative_ontology:measurement_basis(comm_tr_t32, observed).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(comm_tr_t40, observed).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(comm_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(comm_be_t0, projected).
narrative_ontology:measurement(comm_be_t8, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 8, 0.28).
narrative_ontology:measurement_basis(comm_be_t8, observed).
narrative_ontology:measurement(comm_be_t16, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 16, 0.33).
narrative_ontology:measurement_basis(comm_be_t16, observed).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(comm_be_t24, observed).
narrative_ontology:measurement(comm_be_t32, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 32, 0.38).
narrative_ontology:measurement_basis(comm_be_t32, observed).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.39).
narrative_ontology:measurement_basis(comm_be_t40, observed).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(comm_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(comm_su_t0, projected).
narrative_ontology:measurement(comm_su_t8, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 8, 0.19).
narrative_ontology:measurement_basis(comm_su_t8, observed).
narrative_ontology:measurement(comm_su_t16, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 16, 0.22).
narrative_ontology:measurement_basis(comm_su_t16, observed).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 24, 0.25).
narrative_ontology:measurement_basis(comm_su_t24, observed).
narrative_ontology:measurement(comm_su_t32, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 32, 0.27).
narrative_ontology:measurement_basis(comm_su_t32, observed).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(comm_su_t40, observed).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(comm_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% The common-law-precedent-corpus kernel admits three distinct constraint stories: (1) evolutionary_framework (this story) — precedent adapts to evolved normative understanding; lower rigidity, normalized overruling, judiciary as normative updater. (2) strict_stare_decisis — precedent binds backward; overruling requires extraordinary justification, essentially never occurs; judiciary constrained by backward-binding. (3) pluralist_balancing — precedent weight varies by domain and context; overruling weighed case-by-case. Each story instantiates a different reading of the same kernel (the precedent-binding rule). They are not observables of one constraint; they are different constraints arising from the same kernel under different interpretive commitments. The ε values differ: evolutionary reading extracts moderate authority from reliance interests; strict stare decisis extracts moderate authority from reformist access; pluralist balancing distributes extraction across domain-specific balance points. All three remain live institutional positions. Neither forecloses the other; all coexist. Network edges link the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
