% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__coercion_visibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__coercion_visibility_reading, []).

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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command Under Federal Coercion (Coercion Visibility Reading)
 *   domain: religious_authority/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by the leadership of The Church of Jesus Christ
 *   of Latter-day Saints suspended the practice of polygamy in response to
 *   sustained federal coercion: property seizure (Morrill Act 1862,
 *   Edmunds-Tucker Act 1887), criminal prosecution of church officers, and
 *   disenfranchisement of members. Under the coercion_visibility_reading, the
 *   Manifesto is understood as an institutional capitulation to federal
 *   power, framed theologically as a new revelation to preserve institutional
 *   legitimacy and member faith. The reading holds that the theological
 *   reinterpretation (from 'polygamy is divinely commanded' to 'monogamy is
 *   divinely commanded') is a response to exogenous coercive pressure, not to
 *   independent doctrinal development. This reading is contested by two
 *   sibling readings: the continuationist_reading (polygamy remains
 *   doctrinally valid; the Manifesto is a prudential suspension under duress)
 *   and the substitutionist_reading (monogamy is now doctrinally required;
 *   the Manifesto represents new revelation superseding the prior command).
 *   The coercion_visibility_reading focuses on the mechanism of institutional
 *   authority reinterpretation in response to coercive pressure, with a
 *   potential legitimacy crisis if coercion is valid input to what counts as
 *   revelation.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Agenda-setter; issues the Manifesto as divine revelation to resolve the federal pressure crisis while preserving authority
 *   - polygamist_practitioners: Payers; identity-locked members forced to choose between religious understanding (polygamy is commanded) and institutional obedience (abandon practice)
 *   - federal_government: Excluded coercive actor; applies sustained legal and economic pressure that constrains the theological conversation
 *   - reform_faction: Beneficiaries; members who preferred monogamy gain institutional validation and legitimacy
 *   - continuationist_remnant: Excluded payers; pushed into schism or silence for maintaining the original doctrine
 *   - academic_observers: External analysts of whether coercion is a valid input to revelation and authority reinterpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.72).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command Under Federal Coercion (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '9d5d9abc-69d2-41eb-82a1-0cdbaf568084').
narrative_ontology:cs_kernel_codification('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', fixed_text).
narrative_ontology:cs_authority_grounding('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', extraction).
narrative_ontology:cs_interpretation_layer_present('9d5d9abc-69d2-41eb-82a1-0cdbaf568084').
narrative_ontology:cs_reading_relation('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', divine_marriage_command__substitutionist_reading, coexists_with).
narrative_ontology:cs_axiom('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', foundational, coercion_visible_input_to_revelation).
narrative_ontology:cs_axiom_status(coercion_visible_input_to_revelation, holdable).
narrative_ontology:cs_axiom_grounding('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', coercion_visible_input_to_revelation, empirically_contingent).
narrative_ontology:cs_axiom('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', secondary, institutional_survival_grounds_doctrinal_shift).
narrative_ontology:cs_axiom_status(institutional_survival_grounds_doctrinal_shift, holdable).
narrative_ontology:cs_axiom_grounding('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', institutional_survival_grounds_doctrinal_shift, instrumental).
narrative_ontology:cs_reference_frame('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', divine_polygamy_command_authentic).
narrative_ontology:cs_drift_state('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', post_federal_coercion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9d5d9abc-69d2-41eb-82a1-0cdbaf568084', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, polygamist_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, reform_faction).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, continuationist_remnant).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, institutional_survival_necessity).
narrative_ontology:constraint_vindicates(divine_marriage_command__coercion_visibility_reading, revelation_responsive_to_external_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1890 Manifesto suspending polygamy practice in response to federal pressure (property seizure, disenfranchisement, imprisonment of church officers). Frames the suspension as doctrinal shift grounded in new revelation, preserving institutional legitimacy while capitulating to state coercion. Collects the benefit of institutional survival and resumed legal standing.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Members who view polygamy as a divine command are forced to choose: abandon the practice (violating what they understand as God's direct instruction), leave the institution, or face ecclesiastical discipline. Their identity as faithful practitioners is fused with polygamous marriage; exit means spiritual self-loss. The institutional leadership's theological reframing constructs their practice as disobedience rather than faithfulness.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, polygamist_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Applied sustained coercive pressure (property seizure under Morrill Anti-Bigamy Act, Edmunds Act criminal penalties, Edmunds-Tucker Act disenfranchisement and receivership) to force institutional abandonment of polygamy. Their exclusion from the religious authority conversation is the point: the federal law constrains the theological conversation without participating in it. If they were at the table, the coercion visibility would be immediate rather than mediated through the 'revelation' frame.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Members who viewed polygamy as a cultural accommodation rather than a doctrinal core. The Manifesto aligns institutional practice with their preferred theology. They benefit from the constraint because it selects for their interpretation as orthodox, while polygamist practitioners are reclassified as deviant.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, reform_faction, beneficiary,
    moderate, biographical, mobile, regional).

% Those who maintain that the 1890 Manifesto is a prudential suspension under duress, not a doctrinal rescission, and that polygamy remains divinely commanded. They are pushed into schism, apostasy charges, or secret practice. Their exclusion from authority interpretation—despite their claim to preserve the original doctrine—is maintained by institutional control over who counts as a legitimate interpreter.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, continuationist_remnant, excluded,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, continuationist_remnant, payer).

% Historians and scholars of religious authority who examine whether the Manifesto represents genuine theological development, pragmatic capitulation, or authoritative reinterpretation. They sit outside the institutional authority structure but observe it from the standpoint of institutional necessity and coercion visibility.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, academic_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the institution's theological commitments with federal law and political feasibility. Resolves the immediate collective-action problem: individual members cannot unilaterally decide whether polygamy is commanded; only the authority structure can reframe the doctrine. Without coordination, members face irresolvable conflict between understood divine command and legal impossibility.
% TRANSFER_FUNCTION: Transfers doctrinal authority from the original revelation-claim (polygamy is divinely commanded) to a reinterpreted revelation-claim (monogamy is now divinely commanded via the Manifesto). The cost to polygamist practitioners is the loss of their understanding of divine instruction; the benefit to institutional leadership is survival and resumed legitimacy with federal government and U.S. society.
% ABSENT_VOICES: Federal coercive architects (lawmakers, enforcement officers) are structurally excluded from the theological conversation itself. Continuationist members who reject the Manifesto's authority are pushed into schism and silenced within institutional authority structures. If these voices were present, the coercion mechanism would be visible as such rather than mediated through the revelation frame.
% DISAPPEARANCE_RATIONALE: If this constraint—the institutional reinterpretation of the divine marriage command as monogamous—disappeared, the institution would face immediate federal legal consequences (renewed property seizure, criminal prosecution, loss of legal standing). Alternative arrangements: the institution could capitulate explicitly to state law without the revelation frame; members could splinter into continuationist and reformist institutions; federal law could be repealed. The constraint itself is what holds the three-way negotiation (institutional theology, member identity, state power) in place.
% FOUNDING_PROBLEM: Federal coercion created an unsustainable conflict between institutional teaching (polygamy as divinely commanded), member practice, and legal survival. The institution's authority was collapsing under the legal and economic pressure of the Morrill Act, Edmunds Act, and Edmunds-Tucker Act.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary federal agents (Justice Department, territorial governors) explicitly confirm the coercive intent. Church historians and institutional records document the property seizure and disenfranchisement. Academic scholarship (Kathryn Daynes, Sarah Barringer Gordon, Jonathan Stapley, Jed Woodworth) external to the institution corroborates that the 1890 Manifesto was a response to federal pressure, not to an independent theological revelation.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_marriage_command__coercion_visibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__coercion_visibility_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__coercion_visibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__coercion_visibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68 at interval end) is moderately high because the institutional leadership reframes the identity and understanding of a significant segment of the membership. The 1890 Manifesto does not eliminate polygamy immediately—it suspends it and eventually criminalizes it within the institution, extracting from those for whom polygamy was constitutive of faith. The measured extractiveness accounts for the identity-lock: polygamist practitioners cannot exit without profound self-loss. Suppression (0.72) is stable across the interval because the suppression mechanism—institutional authority over theological interpretation, control of who counts as a legitimate voice—persists unchanged. Theater is initially high (0.72) and declines modestly (0.58 by interval end) as the constraint normalizes and the revelation frame becomes routine; the theater represents the proportion of institutional activity devoted to maintaining the authority reinterpretation (doctrinal clarification, testimony, pulpit messaging) rather than to coordinate genuine coordination. The measurement series tracks the transition from crisis (high theater, institutional legitimacy at risk) to stabilization (lower theater as the new normal consolidates). All metrics are authored on one shared time grid (every metric at every time point).
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's seat, the Manifesto is a divine revelation adjusting doctrine to changed circumstances—a genuine coordination function that preserves the institution and its core teachings. From the polygamist practitioner's seat, the Manifesto is a coerced reinterpretation that violates their understood divine instruction and fractures their faith identity. From the federal government's seat (excluded from the theological conversation), the coercion is explicit and the 'revelation' frame is transparent. From the academic observer's seat, the constraint reveals how institutional authority operates when external coercive pressure is applied: the authority structure adapts by reframing what counts as revelation. These divergent readings are not errors—they reflect the real structural asymmetries in the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership sits at the beneficiary end of the directionality spectrum: they collect the benefit of institutional survival, resumed legal standing, and political legitimacy. The polygamist practitioners sit at the target end: they lose their understanding of divine instruction, face identity dissolution or schism. The federal government is excluded from the theological authority structure but is the structural reason the constraint exists—their coercive pressure is the exogenous variable. The reform faction benefits from institutional validation of their preferred theology. The continuationist remnant are targets pushed out of the authority conversation. Directionality is asymmetric: the same constraint produces high d (target) for those who understood polygamy as commanded, and low d (beneficiary or near-neutral) for those who accepted monogamy or institutional authority as self-evidently prior.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy classification because the Manifesto addresses a real founding problem (federal coercion threatening institutional survival) that remains live through the interval. The institution's mandate—to maintain the faith community and its theological authority—is active, not atrophied. However, there is a potential mandatrophy trap: if the Manifesto's legitimacy depends on the federal coercion being real (that is, if the coercion visibility is the constraint's *actual* legitimacy ground), then as federal pressure diminishes, the Manifesto's authority becomes unstable. The constraint is stable as long as the founding problem (federal coercion) is perceived as live; if the founding problem becomes contested (did the coercion actually require doctrinal shift, or could the institution have resisted?), the constraint risks mandatrophy. This is captured in the conceptual omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_validity_input,
    'Is federal coercion a valid input to what counts as divine revelation within the institutional authority structure? Or must revelation be independent of exogenous pressure to be legitimate?',
    'Institutional theological statements examining the relationship between revelation, institutional pressure, and authority. Textual analysis of the Manifesto''s framing (does it claim independent revelation or pressure-responsive reinterpretation?). Comparative study: how the institution handles other doctrinal shifts when coercive pressure is absent.',
    'If coercion is a valid input, the institutional authority claim becomes contingent on political power rather than divine will, destabilizing the entire legitimacy structure. If coercion is not valid, the Manifesto''s authority is compromised because the coercive context is undeniable. Either way, the foundation of institutional theological authority is called into question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_validity_input, conceptual, 'Whether federal coercion can be a legitimate input to doctrinal reinterpretation without undermining institutional authority claims.').

omega_variable(
    mandatrophy_risk_trajectory,
    'Will the Manifesto''s legitimacy erode if federal coercive pressure diminishes or ceases to exist? Is the constraint''s persistence tied to ongoing federal enforcement, or has it become self-sustaining through institutional normalization?',
    'Historical comparison: as federal enforcement of the polygamy prohibition weakened (20th century), did institutional commitment to monogamy weaken correlatively, or did it stabilize independently? Contemporary survey of institutional justifications: do members now justify monogamy from within theology, or from institutional tradition?',
    'If legitimacy is tied to federal pressure, the constraint risks mandatrophy if pressure ceases (the institution keeps enforcing a rule whose founding justification has disappeared). If legitimacy has become self-sustaining, the constraint is stable even without external coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_risk_trajectory, empirical, 'Whether the Manifesto''s legitimacy is dependent on ongoing federal coercion or has become institutionally self-sustaining.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.72) structural (institutional authority apparatus, ex-communication, family/social pressure) or internalized (polygamist practitioners accept institutional authority as legitimately higher than their own understanding)?',
    'Post-institutional suppression trajectory: if practitioners who leave the institution maintain suppression (shame, internalized illegitimacy of polygamy), the suppression is internalized; if suppression is lifted upon exit, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the institutional apparatus alone—the target carries the suppression after exit. If structural, the constraint''s hold is looser and practitioners can recover agency outside the institutional frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of polygamist practice is structural or internalized.').

omega_variable(
    revelation_reinterpretation_vs_capitulation,
    'Does the institutional authority structure genuinely distinguish between revelation (doctrinal truth independent of circumstances) and capitulation (power-responsive reinterpretation)? Or is the distinction impossible to maintain when coercive pressure is acknowledged?',
    'Institutional theological writings on the nature of revelation. How the institution explains the Manifesto to members: as discovered truth, as prudential adaptation, or as revelation-under-pressure (a hybrid term). Sibling-reading corroboration: do the continuationist and substitutionist readings accept the same conception of revelation?',
    'If the distinction cannot be maintained, institutional theological authority is fundamentally unstable—every doctrinal claim becomes hostage to the possibility that it is coercion-responsive rather than divinely grounded. The institutional legitimacy structure becomes circular or self-referential.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revelation_reinterpretation_vs_capitulation, conceptual, 'Whether revelation and coercion-responsive reinterpretation can be coherently distinguished in institutional authority claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__coercion_visibility_reading, theater_ratio, 5, 0.68).
narrative_ontology:measurement_basis(divi_tr_t5, observed).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__coercion_visibility_reading, theater_ratio, 10, 0.64).
narrative_ontology:measurement_basis(divi_tr_t10, observed).
narrative_ontology:measurement(divi_tr_t15, divine_marriage_command__coercion_visibility_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement_basis(divi_tr_t15, observed).
narrative_ontology:measurement(divi_tr_t22, divine_marriage_command__coercion_visibility_reading, theater_ratio, 22, 0.59).
narrative_ontology:measurement_basis(divi_tr_t22, observed).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__coercion_visibility_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(divi_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(divi_be_t5, observed).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(divi_be_t10, observed).
narrative_ontology:measurement(divi_be_t15, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(divi_be_t15, observed).
narrative_ontology:measurement(divi_be_t22, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 22, 0.67).
narrative_ontology:measurement_basis(divi_be_t22, observed).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(divi_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 5, 0.69).
narrative_ontology:measurement_basis(divi_su_t5, observed).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(divi_su_t10, observed).
narrative_ontology:measurement(divi_su_t15, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(divi_su_t15, observed).
narrative_ontology:measurement(divi_su_t22, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 22, 0.72).
narrative_ontology:measurement_basis(divi_su_t22, observed).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(divi_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__coercion_visibility_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel spawns three structurally distinct constraint stories: (1) continuationist_reading—polygamy remains doctrinally valid; Manifesto is prudential suspension; (2) substitutionist_reading—monogamy is now doctrinally required; Manifesto is genuine revelation; (3) coercion_visibility_reading—Manifesto is coercion-responsive reinterpretation; legitimacy derives from institutional survival necessity. Each reading instantiates a different ε (how much the constraint extracts from its targets), different beneficiary/victim sets, and different type. They are linked by the kernel they share (the divine marriage command claim) and the institutional authority structure they contest. The coercion_visibility_reading's key structural claim is that the institutional authority admits non-revelatory grounds (federal pressure) as inputs to doctrinal reinterpretation, which creates potential legitimacy instability absent in the substitutionist reading (which claims pure revelation) and explicit in the continuationist reading (which claims the Manifesto is not binding doctrinally).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
