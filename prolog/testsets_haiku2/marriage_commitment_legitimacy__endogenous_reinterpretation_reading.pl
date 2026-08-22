% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Marriage Commitment Legitimacy — Endogenous Reinterpretation Reading
 *   domain: religious/institutional/political theology
 *
 * SUMMARY:
 *   Between 1852 and 1890, the Church of Jesus Christ of Latter-day Saints
 *   practiced plural marriage as a doctrinal commitment. Facing intense
 *   federal legal pressure—polygamy prosecutions, territorial restrictions,
 *   economic sanctions—the institution issued the 1890 Manifesto declaring an
 *   end to plural marriage. This constraint story instantiates ONE READING of
 *   that kernel: the endogenous-reinterpretation reading, which treats the
 *   Manifesto as genuine prophetic revelation commanding the reversal to
 *   preserve the Church for higher purposes. This reading does not deny
 *   federal pressure; rather, it frames federal pressure as the occasion for
 *   divine instruction, not its cause. The reversal becomes evidence of
 *   ongoing prophetic authority, and monogamy becomes a higher covenant
 *   stage, not doctrinal abandonment. The constraint's function is to
 *   maintain the coherence and legitimacy of this narrative—to coordinate
 *   membership belief around a theology of prophetic responsiveness rather
 *   than institutional capitulation.
 *
 * KEY AGENTS:
 *   - Institutional leadership: sets the agenda, issues the Manifesto, frames it as revelation, claims prophetic authority
 *   - Faithful membership: receives the reframing, maintains identity coherence by accepting the reading, bears the cost of abandoning plural marriage teaching
 *   - Dissenting faithful: would reject the reading, are structurally excluded from the reinterpretation process, experience suppression of their contrary interpretation
 *   - Federal government: applies exogenous pressure, but is analytically external to this reading's causal frame
 *   - Doctrinal beneficiaries (non-agent): prophetic succession claim, theological continuity narrative—vindicated by the constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.42).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Marriage Commitment Legitimacy — Endogenous Reinterpretation Reading").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious/institutional/political theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '597e6ef6-7c06-458c-aa92-a81f468e3e44').
narrative_ontology:cs_kernel_codification('597e6ef6-7c06-458c-aa92-a81f468e3e44', fixed_text).
narrative_ontology:cs_authority_grounding('597e6ef6-7c06-458c-aa92-a81f468e3e44', lineage).
narrative_ontology:cs_interpretation_layer_present('597e6ef6-7c06-458c-aa92-a81f468e3e44').
narrative_ontology:cs_reading_relation('597e6ef6-7c06-458c-aa92-a81f468e3e44', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('597e6ef6-7c06-458c-aa92-a81f468e3e44', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('597e6ef6-7c06-458c-aa92-a81f468e3e44', foundational, manifesto_expresses_divine_will).
narrative_ontology:cs_axiom_status(manifesto_expresses_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('597e6ef6-7c06-458c-aa92-a81f468e3e44', manifesto_expresses_divine_will, deontological).
narrative_ontology:cs_axiom('597e6ef6-7c06-458c-aa92-a81f468e3e44', secondary, prophetic_succession_legitimacy_preserved).
narrative_ontology:cs_axiom_status(prophetic_succession_legitimacy_preserved, holdable).
narrative_ontology:cs_axiom_grounding('597e6ef6-7c06-458c-aa92-a81f468e3e44', prophetic_succession_legitimacy_preserved, deontological).
narrative_ontology:cs_reference_frame('597e6ef6-7c06-458c-aa92-a81f468e3e44', prophetic_authority_doctrine).
narrative_ontology:cs_drift_state('597e6ef6-7c06-458c-aa92-a81f468e3e44', contemporary_secular_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('597e6ef6-7c06-458c-aa92-a81f468e3e44', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_structure).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, institutional_continuity_narrative).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, divine_succession_legitimacy).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_continuity_through_reframing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the Manifesto as prophetic utterance commanding the reversal of plural marriage doctrine. Frames the reversal not as capitulation to federal pressure but as divine instruction to preserve the Church's prophetic mission and spiritual authority. The reversal is presented as continuity with higher theological principles—a new covenant stage rather than doctrinal collapse. Leadership maintains that prophetic succession and the institution's integrity depend on recognizing and obeying this revelation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Receives theological reassurance that the Manifesto represents divine guidance, not institutional compromise. The reading preserves their narrative of belonging to a prophetically led Church even as practice changes. They also bear the cost of abandoning a doctrinal commitment they were taught was eternal and divinely established. Identity fusion with the faith community makes exit economically and psychologically costly.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, faithful_membership, payer).

% Applied legal and political pressure to end plural marriage—the exogenous catalyst. In this reading's framework, federal pressure is the occasion for divine revelation, not its cause. The government's position is analytically external; the theological reading does not attribute causation to coercion.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Would contest the reading's core claim—that the Manifesto represents genuine revelation rather than accommodating capitulation. Their voices are excluded from the institutional reinterpretation process itself. Those who cannot accept the reframing as authentic face schism or departure; their resistance is structurally suppressed by the institution's claim to prophetic authority.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dissenting_faithful, excluded,
    moderate, biographical, identity_locked, global).

% The institutional doctrine that prophetic authority flows through designated successors and that obedience to that authority is the test of faithfulness. This reading vindicates the succession claim by authorizing institutional leadership to reframe doctrine in response to revelation. The Manifesto becomes proof that succession works; the reversal is evidence of ongoing prophetic function.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_claim, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_claim).

% The framework that doctrine can evolve through revelation without losing identity. This reading treats monogamy as a higher covenant stage—not abandonment of prior teaching but fulfillment of a deeper principle. The narrative preserves the Church's self-understanding as theologically continuous and divinely guided, rather than compromised.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_continuity_narrative, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theological_continuity_narrative).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the institutional legitimacy of prophetic succession and theological authority by reframing doctrinal reversals as revealed truth rather than external coercion. The constraint coordinates membership belief around a coherent narrative: the Church is divinely led, its leaders receive ongoing revelation, obedience to revealed authority is the mark of faithfulness, and apparent compromises are actually higher theological principles.
% TRANSFER_FUNCTION: Moves narrative authority from federal government (the exogenous pressure) to divine agency (the endogenous cause). Transfers the meaning of the reversal from 'capitulation under force' to 'obedience to revelation.' What would otherwise appear as institutional defeat becomes evidence of prophetic function.
% ABSENT_VOICES: Dissenting faithful whose reading—that the Manifesto accommodates federal coercion rather than expressing genuine revelation—would contradict the reinterpretation. These voices are structurally excluded from the institutional process that declares what counts as legitimate interpretation. Secular historians and critical observers outside the faith community are also absent from the framework, which treats the theological reading as the sole legitimate account.
% DISAPPEARANCE_RATIONALE: From within this reading's framework, the constraint cannot disappear without unraveling the Church's claim to prophetic authority and theological continuity. Faithful who accept the reading depend on it to maintain coherent identity; institutional leadership depends on it to preserve succession legitimacy. From the exogenous-override reading's standpoint, the constraint is precisely the obstacle that prevents accurate reckoning with coercion. The world rearranges differently depending on which reading governs.
% FOUNDING_PROBLEM: How can a prophetically led institution respond to external pressure while maintaining claims to divine authority and internal theological coherence? How can reversal of foundational doctrine be reconciled with the claim that authority flows from ongoing revelation?
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests the founding problem remains live and that the Manifesto solves it by demonstrating prophetic responsiveness. Believers who accept the reinterpretation corroborate this reading. Dissenting faithful and external historians dispute that the problem is solved this way; they attest to a different problem—the problem of distinguishing genuine revelation from institutional accommodation. No corroboration exists from parties outside the institutional beneficiary set.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, contested).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at interval end) because the constraint's primary function is narrative coordination, not material transfer. The beneficiaries are institutional authority structures and theological doctrines, not discrete agents extracting rents. Suppression is moderate (0.42) because the constraint depends on excluding and suppressing dissenting interpretations—those who read the Manifesto as accommodative rather than prophetic are structurally barred from legitimate voice. Theater is present but not dominant (0.31): the constraint includes genuine theological reasoning and authentic prophetic rhetoric, but part of its operation is performance—maintaining the appearance of doctrinal coherence while practice has changed fundamentally. Accessibility collapse is moderate-to-high (0.65): once the institutional reading is internalized, alternatives become cognitively unavailable; members taught that obedience to prophetic authority is the test of faithfulness find the dissenting reading literally unthinkable. Resistance is moderate (0.52): substantial dissent arose from those who could not accept the reframing, leading to schism; however, the majority accepted the reading, so institutional resistance was overcome. The measurement series shows extractiveness and theater stable after the initial Manifesto period—the constraint settles into a steady coordination function with minimal drift, suggesting it achieves stable legitimation once accepted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (institutional leadership) and the faithful who accept the reading experience this constraint as genuine coordination—divine authority coordinating them around truth. Dissenting faithful and external historians experience it as suppression—forced reinterpretation masking institutional capitulation. The federal government observes it analytically but is not a party to the theological dispute. The engine computes per-seat classifications: institutional leadership sits near d=0.0 (full beneficiary of prophetic authority claims), faithful membership near d=0.5 (genuinely coordinated, but identity-locked and paying the cost of doctrinal reversal), dissenting faithful near d=1.0 (targeted by suppression of their reading), and federal government at d=analytical (external observer). This divergence is structural, not reconcilable—it follows from different causal attributions (endogenous revelation vs. exogenous coercion) that cannot coexist in a single framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: prophetic-authority-structure (collects legitimacy from being the reading's validator) and institutional-continuity-narrative (preserved by reframing doctrinal change as revelation). These are vindicated propositions, not agent beneficiaries in the ordinary sense. Among agent beneficiaries, institutional-leadership benefits most directly—the Manifesto confirms their prophetic authority and validates their succession. Faithful membership benefits narratively (coherent identity) but pays a cost (abandoning prior teaching); they sit near symmetric. Dissenting faithful and dissenting readers are the victims—their interpretation is suppressed, their voices excluded, their reading of events contradicted by institutional authority. The constraint's extractiveness is distributed: not a concentrated taking from one to give to another, but a coordination-and-suppression mechanism that benefits doctrinal legitimacy and institutional authority while suppressing alternative readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—how to maintain prophetic authority while reversing foundational doctrine—is arguably solved by this reading. The reinterpretation preserves the Church's claim to continuous revelation and prophetic succession. However, the mandatrophy question is whether the founding problem is the real problem or a cover story. If the real problem is 'how to accommodate federal pressure while preserving institutional authority,' then the theological solution is mandatrophic—it addresses an invented problem (the need for revelation) rather than the real one (institutional survival under legal threat). The constraint resolves mandatrophy only if one accepts that prophetic succession and theological continuity are problems the institution genuinely faces, independent of political exigencies. An observer who reads federal pressure as the true cause sees the constraint as mandatrophic theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_accommodation_distinguishability,
    'Is there a principled structural distinction between genuine prophetic revelation and institutional accommodation dressed in revelatory language? What observable difference would establish one over the other?',
    'Comparison of the temporal sequence and causal structure: if the Manifesto''s content and timing were predictable from federal pressure alone, the accommodation reading gains evidence; if the Manifesto precedes or independently contradicts federal demands, the revelation reading gains support. Post-hoc theological rationalization is compatible with both readings.',
    'If the distinction is unresolvable, the constraint operates as pure interpretive authority—whichever reading institutional leadership endorses becomes the operative truth for the membership. This would place the constraint closer to a snare than a rope, with theological legitimacy serving as cover for institutional power. If distinguishable, the constraint''s classification depends on which empirical facts obtain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_accommodation_distinguishability, conceptual, 'Whether genuine revelation and institutional accommodation are structurally distinguishable.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the suppression of dissenting voices (those who read the Manifesto as coerced accommodation) primarily structural—institutional sanctions, removal from roles, community exclusion—or internalized—the dissenter''s faith identity makes exit unthinkable, so suppression persists even without external enforcement?',
    'Observation of post-exit trajectories: do dissenting members who leave the faith community continue to experience suppression of their interpretation, or does external suppression cease? If internalized, suppression persists; if structural, it ends at the community boundary.',
    'If primarily internalized, the constraint''s effective suppression is higher than the measured structural component suggests, and the identity-locked exit becomes a more binding mechanism than institutional coercion alone. Reclassification would shift toward snare-like persistence even without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Whether suppression of dissenting theological readings is structural or internalized through identity fusion.').

omega_variable(
    reading_contestation_as_constraint_itself,
    'Is the endogenous-reinterpretation reading itself the constraint being analyzed, or is the constraint the institutional apparatus that enforces this reading as the only legitimate interpretation?',
    'Shift analytical focus: examining the constraint as a reading produces one classification (rope-like coordination around narrative legitimacy); examining it as enforcement of reading-monopoly produces another (snare-like suppression of competing accounts). The two analytical frames are not equivalent.',
    'If the constraint is the reading''s content, extractiveness stays low and suppression moderate. If the constraint is the enforced reading-monopoly, extractiveness and suppression both rise significantly. The two frames describe different constraints that happen to share institutional machinery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contestation_as_constraint_itself, conceptual, 'Ambiguity in the constraint''s referent: the reading itself vs. the enforcement of reading-monopoly.').

omega_variable(
    divine_authority_as_beneficiary,
    'Does the constraint benefit a non-agent entity (''divine authority,'' ''prophetic succession claim,'' ''theological continuity narrative'') that has no interests independent of how institutional members interpret it? Or does attributing beneficiary status to doctrines create a false agent?',
    'Distinction between genuine coordination (members coordinate to solve a problem they share) and ideological closure (the doctrine benefits by insulating itself from scrutiny, independent of member welfare). If member welfare and doctrine-preservation diverge, the doctrine is the beneficiary; if they align, the distinction collapses.',
    'If doctrines can be genuine beneficiaries, the constraint fits a category (vindicated-proposition-beneficiary) not fully captured by agent-based classification. If doctrines are never beneficiaries—only the agents who benefit from defending them are—then the constraint should identify the human institutional actors who benefit from maintaining the reinterpretation (leadership authority, successor legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_authority_as_beneficiary, conceptual, 'Whether institutional doctrines can be treated as beneficiaries or only as descriptions of what benefits institutional actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(marr_tr_t0, projected).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(marr_tr_t5, projected).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(marr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(marr_be_t0, projected).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.18).
narrative_ontology:measurement_basis(marr_be_t5, projected).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t50, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(marr_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(marr_su_t0, projected).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(marr_su_t5, projected).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t50, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(marr_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The marriage-commitment-legitimacy kernel decomposes into three structurally distinct constraints corresponding to three readings: endogenous-reinterpretation (divine revelation as cause), exogenous-override (federal coercion as cause), and hybrid-pragmatic (strategic adaptation as cause). These readings have different ε values (low, high, and moderate respectively), different beneficiary structures, and different persistence mechanisms. Each reading constitutes a separate constraint with its own classification; the three are linked via network.affects_constraints to show their family relationship and shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
