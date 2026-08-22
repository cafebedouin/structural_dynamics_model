% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement
 *
 * SUMMARY:
 *   Qualified immunity is a judicially created doctrine that shields
 *   government officials, primarily law enforcement officers, from damages
 *   liability for constitutional violations. This constraint story
 *   instantiates the CONSTITUTIONAL FIDELITY READING, which argues the
 *   doctrine lacks any constitutional or statutory authorization and is
 *   therefore illegitimate regardless of policy consequences. The doctrine is
 *   claimed as fabricated institutional overreach; the reading's core premise
 *   is that no legitimate legal authority exists for immunity protection,
 *   making both the immunity claim and the remedial denial per se unlawful.
 *   Under this reading, the beneficiary is the judiciary itself
 *   (institutional power expansion through case law development), and the
 *   victims are citizens denied remedy for constitutional harms. This reading
 *   does NOT argue immunity is bad policy — it argues immunity is
 *   unauthorized doctrine, a constitutional boundary violation by the
 *   judiciary itself.
 *
 * KEY AGENTS:
 *   - Law enforcement officers: constrained beneficiaries under a doctrine the reading holds is illegitimate, operating without actual legal authorization
 *   - Victims of constitutional violations: powerless payers denied remedy by an unauthorized doctrine
 *   - Judiciary (institutional): agenda-setter and beneficiary, expanded institutional power through common-law doctrine creation
 *   - Congress: excluded from doctrine generation and maintenance, structurally responsible for remedy but constrained by judicial entrenchment
 *   - Civil rights advocates: observers contesting the doctrine's constitutional pedigree
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.68).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.71).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'b1dd6a9b-45ef-46e2-8b5b-0c08c9766893').
narrative_ontology:cs_kernel_codification('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', implicit).
narrative_ontology:cs_authority_grounding('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', extraction).
narrative_ontology:cs_interpretation_layer_present('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893').
narrative_ontology:cs_reading_relation('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', foundational, judicial_common_law_requires_textual_warrant).
narrative_ontology:cs_axiom_status(judicial_common_law_requires_textual_warrant, holdable).
narrative_ontology:cs_axiom_grounding('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', judicial_common_law_requires_textual_warrant, deontological).
narrative_ontology:cs_axiom('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', foundational, immunity_without_authorization_forecloses_remedy_right).
narrative_ontology:cs_axiom_status(immunity_without_authorization_forecloses_remedy_right, holdable).
narrative_ontology:cs_axiom_grounding('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', immunity_without_authorization_forecloses_remedy_right, deontological).
narrative_ontology:cs_reference_frame('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', constitutional_text_statutory_limitation).
narrative_ontology:cs_drift_state('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', contemporary_doctrine_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b1dd6a9b-45ef-46e2-8b5b-0c08c9766893', '2026-08-04T00:00:00Z').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary_institutional_power).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_constitutional_violations).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, citizens_denied_remedy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, judicial_supremacy_in_common_law_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy an internally contradictory position: nominally protected from liability by the doctrine, yet operating within a framework the reading declares illegitimate, leaving their actions without lawful authorization. Officers claim immunity protects necessary discretion; the reading argues no doctrine exists to authorize what they do. Their exit from law enforcement is possible but constrained by career dependence and professional identity.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, beneficiary).

% Bear the costs of officer conduct declared constitutional violations yet denied judicial remedy because the doctrine (which this reading holds is fabricated) forecloses damages suits. They are trapped by geography, social position, and the constraint itself: the violation occurs, the doctrine denies recourse, and no alternative remedy pathway exists with comparable force. Exit is not available.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, victims_of_constitutional_violations, payer,
    powerless, immediate, trapped, national).

% Are organized as a class by the doctrine's effect: constitutional harm without legal remedy. They can petition legislatures, mount political pressure, litigate to change the law, or emigrate — but constrained by collective-action costs and the entrenched institutional support the doctrine enjoys. Their remedy-less position generates sustained resistance and reform advocacy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, citizens_denied_remedy, payer,
    organized, biographical, constrained, national).

% Created, maintains, and continuously expands the doctrine through case law despite the absence of constitutional or statutory text authorizing it. The judiciary benefits from the doctrine by expanding its own institutional power (deciding when the doctrine applies, narrowing plaintiff remedies, building jurisprudence that stabilizes judicial discretion). They can revise or abandon the doctrine at will, making their position one of deliberate institutional choice.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary, beneficiary).

% Witnesses the doctrine's internal contradictions across circuits: some judges apply it narrowly, others expansively; the Supreme Court has narrowed it only at the margins; circuit splits persist. Observes that the doctrine cannot be justified by reference to constitutional text or original statutory authority, yet persists through institutional inertia and mutual judicial deference.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary_collective, observer,
    institutional, generational, analytical, national).

% Is structurally excluded from the doctrine's generation and maintenance: the doctrine was judicially created and lives in common law, beyond statutory reach. Congress could theoretically codify or abolish it by legislation, but the doctrine's entrenchment in judicial practice makes legislative remedy politically and institutionally difficult. They are absent from the design but structurally responsible for remedy.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, constrained, national).

% Analyze and contest the doctrine's constitutional pedigree; argue that no legitimate constitutional authority exists for immunity and that the doctrine's persistence is institutional entrenchment rather than legal necessity. They have exit options (shift focus to other civil rights issues, work in other jurisdictions) but are organizationally committed to dismantling the doctrine.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_advocates, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this reading explicitly denies coordination function. The doctrine does not solve a collective-action problem or enable necessary cooperation; it forecloses remedy and consolidates judicial power. No coordination story is present under this reading.
% TRANSFER_FUNCTION: Transfers legal immunity (protection from damages liability) from constitutional violators to officers conducting constitutional violations, and transfers the remedy right from injured citizens to the judiciary (which controls when exceptions apply). The transfer mechanism is jurisprudential: case law that asserts the immunity doctrine without constitutional or statutory warrant.
% ABSENT_VOICES: Constitutional scholars who deny the doctrine has legitimate authorization are present in academic discourse but absent from judicial proceedings where the doctrine is applied. Legislative voices are structurally excluded: Congress did not create the doctrine and cannot directly reverse it. Officers who would defend the doctrine's necessity for law enforcement do not participate in the constitutional-fidelity analysis.
% DISAPPEARANCE_RATIONALE: If the doctrine disappeared overnight, injured parties would recover damages through Section 1983 and similar remedies against individual officers and municipalities. Officers would face liability exposure, forcing institutional changes in training, accountability, and conduct standards. The judiciary would lose a primary tool for limiting damages remedies and would need to adjudicate constitutional claims on their merits rather than by doctrine-gating them. The remedial landscape would reorganize rapidly.
% FOUNDING_PROBLEM: The doctrine was created in Harlow v. Fitzgerald (1982) to address judges' and officials' fear of frivolous litigation and damages liability exposure, enabling vigorous executive action without defensive litigation costs. The underlying problem framed by the Court was unmanageable litigation volume and defensive decision-making by public officials.
% FOUNDING_PROBLEM_CORROBORATION: The Court itself attests to the founding problem in its Harlow opinion and subsequent QI cases. Civil rights scholars and constitutional law experts outside the judiciary contest both the problem's magnitude and whether the judicially fabricated doctrine was the appropriate remedy — legislative process and statutory immunity regimes in other contexts (e.g., the Federal Tort Claims Act) demonstrate alternative solutions. Congressional testimony and law review literature from outside the benefiting judiciary documents the contested status.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the degree to which the doctrine extracts remedy rights from injured parties and concentrates decision authority in the judiciary. Starting at 0.35 in 1982 (the doctrine was nascent and narrower), it has accumulated to 0.68 by 2026 as courts have applied and expanded it through case law. Theater ratio measures the ratio of performative to functional activity: early on, the doctrine was framed as necessary for vigorous law enforcement (functional); over time, courts' focus shifted to doctrine-gating remedies rather than assessing constitutional violations on the merits (performative). Theater climbs to 0.44, indicating that procedural immunity-filtering now occupies more of the judicial landscape than substantive constitutional analysis. Suppression measures the constraint's persistence through enforcement: the judiciary actively enforces the doctrine through procedural motions and summary judgment, making escape from the constraint nearly impossible for plaintiffs. The measurement series are aligned on a single time grid (six points spanning 1982–2026) so every metric is authored at every examined time point. Accessibility collapse is 0.62: once a plaintiff understands the doctrine exists, alternatives (administrative remedies, state law suits, political advocacy) are available but substantially constrained by the doctrine's federal-court dominance.
 *
 * PERSPECTIVAL GAP:
 *   Officers and judges experience the constraint's legitimacy asymmetrically. Officers may believe the doctrine is a necessary protection enabling their work; under the constitutional-fidelity reading, they are actually operating under an unauthorized doctrine that puts them and citizens in constitutional limbo. The judiciary sees the doctrine as a legitimate development of common law and judicious case-law creation; the reading sees it as institutional overreach unsupported by constitutional or statutory text. Victims see the doctrine as a straightforward bar to remedy; the reading contextualizes that bar as extra-constitutional. The perspectival gap is structural: the same constraint is simultaneously framed as legitimate institutional discretion (from the judicial seat) and illegitimate overreach (from the constitutional-fidelity reading seat). The engine computes per-seat type classifications from the power and exit data; this gap explains why the same constraint produces different effective-extraction values across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers occupy a complicated seat: they are nominally protected (beneficiaries of immunity) yet operating under a framework the reading declares illegitimate, leaving their conduct without lawful authorization. Their directionality d is near 0.5 (symmetric) because the immunity benefits them but at the cost of operating under an illegitimate doctrine. Victims sit at d ≈ 0.95 (near full targets): they bear the extraction (remedy denial), have trapped exit (no alternative remedy mechanism comparable to Section 1983), and cannot exit their victimhood status. The judiciary sits at d ≈ 0.05 (near full beneficiary): the doctrine expands their institutional power, they set its scope and application, and they face no consequences for maintaining the unauthorized doctrine. Congress sits in exclusion (not a direct stakeholder in the constraint's operation) but carries responsibility for remedy. The authorized doctrine baseline (what Section 1983 and the Fourth Amendment authorize) cannot protect officers from damages liability for constitutional violations; the doctrine adds protection beyond that authorization, making officers partial beneficiaries of an excess.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unmanageable litigation volume and defensive decision-making by officials) has substantially changed in status since 1982. Federal civil rights litigation has not exploded as feared; procedural and substantive law have evolved to manage frivolous claims without blanket immunity. The doctrine persists not because the founding problem remains live but because it has become institutionally entrenched: judges apply it habitually, it structures legal practice, and removing it would require explicit institutional reversal. This is mandatrophy: a doctrine whose original function has attenuated yet persists through inertia. The rising theater ratio (0.22 → 0.44) is the marker of mandatrophy: the doctrine's function has shifted from enabling vigorous action to gating remedies, increasingly theatrical rather than substantive. Under the constitutional-fidelity reading, mandatrophy is compounded by illegitimacy: the doctrine was never authorized to begin with, so its persistence cannot be defended by functional argument. The constraint should have been legislatively addressed or judicially abandoned decades ago; its persistence is pure institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_authorization_ambiguity,
    'Can the judiciary legitimately create an immunity doctrine from common-law tradition without explicit constitutional or statutory text, or is such creation per se an unauthorized institutional overreach?',
    'Constitutional interpretation dispute: originalist vs. living-constitution frameworks produce different answers. Originalist analysis (text-focused) tends to deny authorization; living-constitution analysis (structural-evolution focused) may permit it. This is a conceptual/interpretive disagreement, not empirically resolvable.',
    'If judicial common-law creation is legitimate, the constitutional-fidelity reading''s core premise fails and the constraint might reclassify as rope (legitimate coordination of official accountability). If judicial creation is per se unauthorized, the reading''s illegitimacy claim stands regardless of policy outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_authorization_ambiguity, conceptual, 'Whether judicial common-law doctrine creation requires explicit constitutional or statutory authorization.').

omega_variable(
    remedy_adequacy_alternative,
    'Is Section 1983 damages liability (without immunity) a sufficiently adequate remedy for constitutional violations, or does immunity serve a legitimate damage-control function despite lacking explicit authorization?',
    'Empirical comparison with other immunity regimes (FTCA, Westfall Act) to assess whether legislative alternatives to judicial immunity produce different litigation or accountability outcomes. Controlled comparison across jurisdictions with and without QI equivalents.',
    'If legislative alternatives produce equivalent litigation levels and accountability, the constitutional-fidelity reading''s claim that no authorization-deficiency exists in remedy design is strengthened. If legislative alternatives produce substantially worse outcomes, the reading''s premise (illegitimacy regardless of consequences) is tested but does not shift (the reading is about authorization, not policy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_adequacy_alternative, empirical, 'Whether authorized statutory immunity regimes achieve the objectives QI claims to pursue.').

omega_variable(
    institutional_identity_lock_on_judiciary,
    'To what extent is the judiciary''s attachment to QI doctrine an identity-locked institutional commitment (the judiciary has ''become'' a discretionary-authority institution) versus a deliberate policy choice that could be reversed?',
    'Observation of circuit-court dissents, Supreme Court concurrences, and judicial statements about QI reform. Signs of identity lock: courts defend QI reflexively without substantive argument. Signs of deliberate choice: courts acknowledge authorization ambiguity but maintain QI as policy judgment.',
    'If identity-locked, the constraint''s persistence is driven by institutional self-concept and would require external (congressional) pressure to reverse. If deliberate choice, the judiciary retains the option to abandon the doctrine without institutional self-transformation. The directionality for the judiciary would shift slightly (from arbitrage toward constrained) under identity lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_lock_on_judiciary, empirical, 'Whether the judiciary is locked into QI defense by institutional identity or retains deliberate choice.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the constitutional-fidelity reading''s claim that QI is unauthorized logically foreclose the protective-scaffold reading''s claim that QI is necessary for vigorous law enforcement?',
    'Logical analysis: if X is unauthorized, can X be justified as necessary? The readings disagree on the precedence of authorization over necessity. The constitutional-fidelity reading says authorization comes first (no authorization = illegitimate, regardless of necessity); protective-scaffold says necessity can justify common-law creation. This is a metatheoretical disagreement about legal reasoning, not empirically resolvable.',
    'The relation assigned in cs_structure.reading_relations (forecloses vs. coexists_with) depends on whether authorization-first or necessity-first is the correct metatheory. Under authorization-first, the readings foreclose each other; under necessity-first, they coexist as competing interpretations of the same institution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether authorization-first or necessity-first reasoning is the correct metatheory for evaluating judicial doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1982, 0.22).
narrative_ontology:measurement_basis(qual_tr_t1982, observed).
narrative_ontology:measurement(qual_tr_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement_basis(qual_tr_t1995, observed).
narrative_ontology:measurement(qual_tr_t2005, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement_basis(qual_tr_t2005, observed).
narrative_ontology:measurement(qual_tr_t2015, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(qual_tr_t2015, observed).
narrative_ontology:measurement(qual_tr_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement_basis(qual_tr_t2020, observed).
narrative_ontology:measurement(qual_tr_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2026, 0.44).
narrative_ontology:measurement_basis(qual_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement_basis(qual_be_t1982, observed).
narrative_ontology:measurement(qual_be_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(qual_be_t1995, observed).
narrative_ontology:measurement(qual_be_t2005, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement_basis(qual_be_t2005, observed).
narrative_ontology:measurement(qual_be_t2015, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement_basis(qual_be_t2015, observed).
narrative_ontology:measurement(qual_be_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement_basis(qual_be_t2020, observed).
narrative_ontology:measurement(qual_be_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(qual_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement_basis(qual_su_t1982, observed).
narrative_ontology:measurement(qual_su_t1995, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(qual_su_t1995, observed).
narrative_ontology:measurement(qual_su_t2005, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2005, 0.61).
narrative_ontology:measurement_basis(qual_su_t2005, observed).
narrative_ontology:measurement(qual_su_t2015, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2015, 0.67).
narrative_ontology:measurement_basis(qual_su_t2015, observed).
narrative_ontology:measurement(qual_su_t2020, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement_basis(qual_su_t2020, observed).
narrative_ontology:measurement(qual_su_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(qual_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.08).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).

% DUAL FORMULATION NOTE:
% Qualified immunity doctrine decomposes into three structurally distinct constraints representing three readings of the same kernel: (1) constitutional_fidelity_reading — doctrine is unauthorized and illegitimate regardless of policy; (2) protective_scaffold_reading — doctrine is justified as transitional protection for law enforcement; (3) accountability_void_reading — doctrine is systematic extraction mechanism guaranteeing impunity. These readings share the same referent (the existing judicial immunity arrangement) but instantiate different constraints because they disagree on the doctrine's authorization, beneficiary structure, and persistence justification. ε_base differs substantially across readings: fidelity reading emphasizes institutional overreach (high extraction), protective reading emphasizes coordination necessity (lower extraction), accountability reading emphasizes pure extraction (highest). All three stories link to each other via network.affects_constraints to form the qualified immunity doctrine family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
