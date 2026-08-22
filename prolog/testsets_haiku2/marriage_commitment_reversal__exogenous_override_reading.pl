% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercion of LDS Plural Marriage Practice Suspension (Exogenous Override Reading)
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   Between 1890 and statehood in 1896, the LDS Church faced federal pressure
 *   to cease plural marriage practice as a condition of Utah territorial
 *   integration into the United States. The leadership issued the Manifesto
 *   (officially D&C Offical Declaration 1), publicly announcing the end of
 *   plural marriage practice, yet preserved Section 132 of the Doctrine and
 *   Covenants—the foundational doctrinal claim that plural marriage is
 *   eternal and divinely ordained—in the official canon without revision or
 *   renunciation. This reading examines the constraint as one of EXOGENOUS
 *   COERCION: the federal government extracts institutional compliance
 *   through threat and dispossession, while the LDS institution maintains
 *   doctrinal authority by never formally renouncing the underlying
 *   principle. The constraint is NOT a reinterpretation (sibling reading:
 *   endogenous_reinterpretation_reading) but an externally-imposed cessation
 *   that leaves the doctrine-practice gap structurally unresolved. This is
 *   the 'holding hostage' frame: practice is surrendered to preserve
 *   institutional survival, doctrine is preserved to maintain institutional
 *   coherence, and the contradiction between the two persists indefinitely.
 *
 * KEY AGENTS:
 *   - LDS institutional leadership: institutional power, trapped exit, generational time horizon — faces federal coercion; surrenders practice while preserving doctrine
 *   - Federal government: institutional power, arbitrage exit, national scope — imposes coercive conditions; extracts institutional autonomy and territorial control
 *   - Plural marriage practitioners: powerless, identity-locked, regional scope — absorb state violence; cannot exit without renouncing faith-constituted selfhood
 *   - Fundamentalist breakaways: powerless, identity-locked, regional scope — excluded from both federal law and LDS institutional legitimacy; embody the unresolved doctrine-practice gap
 *   - Federal judge: institutional observer — arbitrates the boundary between internal teaching and external compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.82).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercion of LDS Plural Marriage Practice Suspension (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '3942b0d9-2bac-4ebb-a8b1-40f456abd8d8').
narrative_ontology:cs_kernel_codification('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', fixed_text).
narrative_ontology:cs_authority_grounding('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', extraction).
narrative_ontology:cs_interpretation_layer_present('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8').
narrative_ontology:cs_reading_relation('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', foundational, federal_coercion_primary_cause).
narrative_ontology:cs_axiom_status(federal_coercion_primary_cause, holdable).
narrative_ontology:cs_axiom_grounding('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', federal_coercion_primary_cause, empirically_contingent).
narrative_ontology:cs_axiom('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', foundational, doctrine_preserved_despite_practice_reversal).
narrative_ontology:cs_axiom_status(doctrine_preserved_despite_practice_reversal, holdable).
narrative_ontology:cs_axiom_grounding('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', doctrine_preserved_despite_practice_reversal, deontological).
narrative_ontology:cs_reference_frame('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', doctrinal_authority_over_marriage_kinship).
narrative_ontology:cs_drift_state('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', post_manifesto_institutional_compliance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3942b0d9-2bac-4ebb-a8b1-40f456abd8d8', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_control).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, competing_protestant_establishment).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, utah_non_lds_settlers).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces federal threats: territorial exclusion, property seizure, loss of educational/legal standing, and prosecution of members. Retains internal doctrinal authority (Section 132 never revoked) but must suspend public practice of plural marriage to preserve institutional survival. Maintains interpretive control over doctrine while operationally complying with federal mandate. The constraint binds them to public abandonment while preserving the internal teaching.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership, payer,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership, agenda_setter).

% Imposes territorial and legal conditions on Utah statehood explicitly conditioning on cessation of plural marriage practice. Uses property seizure, legal prosecution, and denial of statehood as enforcement mechanisms. Does not require doctrinal renunciation—only behavioral compliance and public institutional abandonment of the practice. Extracts institutional sovereignty as the price of territorial integration.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Face criminal prosecution, property seizure, loss of legal marriage rights, social stigma, and family separation under the Morrill Anti-Bigamy Act and related federal statutes. Cannot exit the identity (formed by covenant and community) without renouncing faith-constituted selfhood. Must choose between practicing belief and avoiding state violence. The constraint extracts obedience through the threat of criminal sanctions and family dissolution.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practitioners, payer,
    powerless, biographical, identity_locked, regional).

% Benefits from LDS practice suppression as it eliminates a competitor's distinctive religious claim and integrates LDS into a Protestant-normed religious landscape. The constraint serves civilizational-norm vindication across the religious establishment without the establishment having to enforce it directly—the federal state does the coercive work.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, competing_protestant_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Access territorial resources and political participation previously restricted by LDS institutional dominance. The constraint's enforcement opens political space and property access to non-LDS settlers who would otherwise face institutional exclusion in Utah. Benefit from forced institutional retreat without bearing enforcement costs.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, utah_non_lds_settlers, beneficiary,
    moderate, generational, constrained, regional).

% Fundamentalist practitioners who reject the Manifesto and practice plural marriage in violation of the official institutional position. Are prosecuted under the same federal statutes LDS leadership formally renounced; operate outside both federal law and official LDS institutional authority. Would argue that the constraint is precisely the extraction of doctrinal betrayal masked as institutional survival, but are structurally excluded from legitimacy within either framework—criminal under federal law, apostate under LDS institutional law.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, doctrinal_continuity_preservers, excluded,
    powerless, biographical, identity_locked, regional).

% Adjudicates cases arising from plural marriage practice post-suspension, determining whether institutional compliance is sufficient or whether individual practitioners remain liable. Interprets the constraint's terms—what counts as practice cessation versus doctrinal preservation, what enforces the boundary between internal teaching and external compliance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_judge_arbiter, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enforces a single legal marriage regime at the territorial level: removes competing institutional authority over family structure, harmonizes Utah law with federal territorial jurisdiction, and establishes monogamy as the baseline for legal recognition and state benefits.
% TRANSFER_FUNCTION: Transfers institutional autonomy from LDS leadership to the federal government: LDS retains internal doctrinal authority but surrenders practice, property rights (initially), and capacity to structure kinship relations outside state recognition. Federal government gains territorial monopoly on marriage law and settles the civilizational-norm contest in favor of monogamy.
% ABSENT_VOICES: Plural marriage practitioners (those most affected by the constraint) are excluded from the negotiation; so are fundamentalist interpreters who hold Section 132 as superseding any later institutional decision. Indigenous peoples whose marriage practices were also subject to federal regulation are structurally erased from this story. The constraint's design presumes that only LDS institutional leadership and federal authority are the legitimate parties.
% DISAPPEARANCE_RATIONALE: If the constraint vanished (federal enforcement ceased), LDS institutional leadership would face internal pressure to either resume plural marriage practice (honoring Section 132) or explicitly renounce it (aligning doctrine with practice). The absence of federal coercion would force a genuine internal decision instead of the externally-imposed doctrine-practice gap. Federal territorial control of marriage law would be destabilized; competing territorial authorities might reassert quasi-autonomous marriage regimes. The religious landscape would reorganize around the question of whether Section 132 remained binding.
% FOUNDING_PROBLEM: Utah territorial governance faced a federal crisis: polygamy was read by federal and East Coast Protestant establishments as civilizationally inimical, a sign of failed assimilation; Utah's road to statehood was explicitly blocked until plural marriage ceased. The founding problem was framed as: 'How does a religious institution integrate into a federal territorial structure that violates its core doctrinal claims?'
% FOUNDING_PROBLEM_CORROBORATION: LDS institutional leadership attests the problem as live and foundational to survival (archival testimony: the Manifesto's language repeatedly invokes statehood as the conditio sine qua non). Federal government and competing religious establishments attest the problem as solved by practice cessation regardless of doctrinal preservation. Historians and doctrinal scholars outside the LDS institution attest the problem persists unresolved: the constraint suppresses practice but does not resolve the doctrinal claim, creating a structural ambiguity that fundamentalist splits and ongoing doctrinal debates have inhabited for 140+ years.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading assigns HIGH extractiveness (0.82 terminal) because the federal government, as the agenda-setter, extracts the institutional autonomy to define marriage kinship relations in LDS territory. The LDS leadership must cease a practice their doctrine authorizes, and they do so not by renouncing the doctrine but by declaring it inapplicable 'in this dispensation'—a formula that preserves the teaching while suspending its exercise. Suppression is very high (0.88) because the constraint depends on continuous federal enforcement: property seizure, criminal prosecution of practitioners, denial of statehood until compliance is demonstrated, and post-statehood continued prosecution of violations. The theater ratio is high (0.71) because the Manifesto itself is performative: it is not a doctrinal reconsideration but a public declaration of policy cessation. The underlying teaching persists unchanged; what changes is the public institutional posture. Accessibility collapse is very high (0.92) because practitioners who wish to honor their understanding of Section 132 have virtually no exit: remaining in the LDS institution means complying with the Manifesto; leaving the institution means losing community, authority structure, and spiritual legitimacy. Federal exit (leaving the territory) was possible in 1890 but increasingly costly as the constraint hardened. Resistance is moderate (0.58) because while practitioners resisted initially and fundamentalist groups split off to continue the practice, the LDS institutional hierarchy absorbed the constraint and marginalized internal dissent over the next several decades. The measurement series shows extractiveness and suppression hardening over the interval as federal pressure intensified and the constraint shifted from external threat to embedded institutional practice.
 *
 * PERSPECTIVAL GAP:
 *   The federal government and LDS institutional leadership occupy opposite structural positions and should compute radically different types from the engine. From the federal agenda-setter seat, this constraint is a coordination mechanism (establishing unified marriage law across the territory) that persists because the LDS institution complies with it, making it appear almost like a rope. From the plural marriage practitioner seat (the victim seat), this is unambiguously a snare: coercive threat, identity-locked exit, no genuine alternative, and the constraint persists only because the cost of resistance (prosecution, family dissolution) exceeds the cost of compliance. From the LDS institutional leadership seat, it is extractive (they lose autonomy) but instrumentally rational (they preserve institutional survival and, they believe, doctrinal truth by divorcing doctrine from practice). The engine should compute per-seat types that reflect these asymmetries. The authored claim (snare) reflects the victim-seat reading; the metrics describe the structural asymmetry that enables the snare to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is high for the federal government (d near 0.0, full beneficiary) because they extract institutional autonomy without bearing enforcement costs directly—the LDS institution becomes complicit in suppressing its own practitioners. Directionality is very high for plural marriage practitioners (d near 1.0, full target) because they absorb all enforcement: criminal prosecution, family separation, social stigma, and the permanent inability to practice what their doctrine teaches. LDS institutional leadership sits at moderate directionality (d ~0.5-0.6) because they both pay (lose practice authority, must suppress internal dissent, bear reputational cost) and benefit (preserve institutional survival, retain doctrinal teaching, position themselves as the legitimate interpreters of doctrine). The constraint extracts their autonomy while preserving their institutional existence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested between two incompatible verdicts: LDS leadership attests it is solved (plural marriage ceased, statehood achieved, institutional integration succeeded). Fundamentalist practitioners and scholarly historians outside LDS institutional control attest it is unresolved (the doctrine remains, practitioners continue, the contradiction persists). This is a classic setup for mandatrophy: the constraint was built to solve a real coordination problem (territorial integration requires unified marriage law), but the solution leaves the doctrinal foundation of the problem unaddressed. The constraint persists not because the problem is alive but because institutional and federal interests benefit from maintaining the doctrine-practice gap: the LDS institution benefits from claiming doctrinal fidelity while practicing federal compliance; the federal government benefits from LDS cooperation and compliance signaling without needing to resolve the doctrinal question; and both benefit from marginalizing fundamentalist practitioners as illegitimate. The theater ratio rising from 0.55 to 0.71 over the interval indicates that maintaining the constraint requires increasing performance and decreasing functional coordination—the constraint shifts from external enforcement (federal threat of statehood denial) toward embedded institutional theater (the Manifesto reaffirmed, Section 132 preserved in canon, the contradiction managed through interpretive formulas). This is the classic piton trajectory: the founding problem solving function decays while the institutional infrastructure preserves the constraint through performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_etiology,
    'Is the practice reversal primarily caused by external federal coercion (exogenous_override_reading) or by internal divine revelation reinterpreting divine will (endogenous_reinterpretation_reading), or is causality genuinely ambiguous?',
    'Historical analysis of: (1) Wilford Woodruff''s own statements about the revelation (Sept 23, 1889 vision) — did he describe it as responding to federal pressure or as independent divine communication? (2) Timing of doctrinal shift vs. federal pressure intensification — did revelation precede or follow federal escalation? (3) Counterfactual: would LDS leadership have reversed practice without federal coercion? (4) Internal doctrinal discourse — do LDS theologians emphasize divine revelation as the cause, or pragmatic institutional survival?',
    'If federal coercion is primary, the constraint is a snare (exogenous_override_reading — this reading) and the LDS institution is a victim of state extraction. If divine revelation is primary, the constraint is closer to a coordinated institutional decision (endogenous_reinterpretation_reading) and federal pressure is a secondary factor. If causality is genuinely ambiguous, the constraint instantiates an unresolvable pluralism where each reading claims legitimacy from its own epistemic frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_vs_revelation_etiology, empirical, 'Primary cause of practice reversal: external coercion or internal revelation? The question cuts between the sibling readings.').

omega_variable(
    doctrine_preservation_intention,
    'When LDS leadership preserved Section 132 in the canon without revision or formal renunciation, was this an intentional preservation of doctrinal claim (doctrine-practice gap as strategy), or an inadvertent institutional inconsistency?',
    'Historical analysis of contemporaneous doctrinal writings, institutional correspondence, and explicit statements from LDS leadership about whether Section 132 remained binding doctrine. Analysis of whether the doctrine-practice gap was discussed as a problem or accepted as a legitimate interpretive solution.',
    'If intentional, the doctrine-practice gap is a strategic choice to preserve both institutional survival and doctrinal fidelity—the constraint is a snare because it extracts practice while preserving doctrinal claim, creating an indefinite structural ambiguity. If inadvertent, the gap is an unresolved institutional problem that later generated fundamentalist splits and doctrinal confusion—the constraint is unstable because the inconsistency was never explicitly managed. If mixed (partially intentional, partially inadvertent), the constraint''s persistence is contingent on institutional agreement to manage ambiguity without resolving it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_preservation_intention, empirical, 'Was the doctrine-practice gap an intentional institutional strategy or an unresolved contradiction?').

omega_variable(
    section_132_binding_status,
    'After the Manifesto, does Section 132 remain binding doctrine within the LDS institution, or is it a superseded historical text?',
    'Current LDS institutional theology and official statements on whether Section 132 is binding divine doctrine or a historical claim that has been replaced by later revelation (the Manifesto, Official Declaration 1). Analysis of whether the LDS church teaches that plural marriage will be resumed in the afterlife (as Section 132 implies) or whether it is permanently renounced.',
    'If Section 132 remains binding, the constraint is an indefinite doctrine-practice gap that keeps the contradiction alive and generates ongoing legitimacy challenges from fundamentalist practitioners. If Section 132 is superseded, the constraint succeeds in resolving the problem through doctrinal revision (making this reading closer to endogenous_reinterpretation_reading). The ambiguity between these two verdicts is what persists structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_132_binding_status, conceptual, 'Binding status of Section 132 after the Manifesto: doctrine or history?').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of plural marriage practice primarily structural (federal law, prosecution, property seizure) or internalized (LDS institutional culture, shame, identity fusion with compliance)?',
    'Analysis of (1) post-suppression behavior of practitioners who left federal territory (did they resume practice where federal law did not reach?); (2) doctrinal discourse shifts (did LDS theology evolve to internalize monogamy as a normative value, not merely an institutional policy?); (3) apostate/exit testimony (what do people who leave LDS institutions say about the constraint''s coercive mechanism?); (4) fundamentalist continuation in geographically distant or isolated communities (does practice continue where structural suppression is less effective?).',
    'If primarily structural, the constraint depends on continuous federal enforcement and would collapse if federal coercion weakened. If substantially internalized, practitioners carry the suppression with them even if legal barriers disappear (post-exit suppression trajectory). If mixed, the constraint is reinforced by both mechanisms and would require both structural and cultural shifts to resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Suppression mechanism: structural law or internalized institutional culture?').

omega_variable(
    beneficiary_ambiguity,
    'Who is the actual beneficiary of this constraint: the federal government (territorial control and civilizational norm vindication), the competing Protestant establishment (elimination of a religious competitor), or the LDS institution itself (preservation through compliance)?',
    'Counterfactual analysis: (1) If the constraint did not exist, what would each potential beneficiary lose? (2) Historical analysis of who pressed for the constraint (federal government as enforcer, competing religious establishments as advocates, LDS institutional leadership as strategic adopters). (3) Post-constraint benefit distribution (who gained territory, political power, institutional standing, and resources after plural marriage ceased?).',
    'If federal government is primary beneficiary, the constraint is exogenous coercion (this reading). If competing establishments are primary, the constraint is ideological suppression of a religious competitor using federal power as instrument. If LDS institution is primary, the constraint is a strategic institutional choice to preserve existence (shifting toward endogenous_reinterpretation_reading). Mixed beneficiary structures would suggest the constraint is overdetermined—multiple parties benefit and all have incentive to maintain it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_ambiguity, empirical, 'Primary beneficiary: federal government, competing religious establishment, or LDS institutional self-preservation?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 5, 0.6).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 10, 0.63).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 20, 0.68).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 30, 0.7).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 40, 0.71).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 30, 0.81).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 5, 0.84).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 40, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__exogenous_override_reading, 0.15).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family decomposing the contested kernel 'marriage_commitment_reversal' (the Manifesto and its institutional consequences). The exogenous_override_reading emphasizes federal coercion as the primary cause; the endogenous_reinterpretation_reading emphasizes internal divine revelation; the practice_doctrine_gap reading emphasizes the unresolved structural ambiguity. Each reading has different ε, different beneficiary/victim structure, different founding-problem status, and different type signature. They are not the same constraint viewed from different angles—they are structurally distinct constraint readings that coexist as different parties' interpretations of the same contested kernel. The three stories are linked via network.affects_constraints because they share the same referent (the historical Manifesto event) and each reading's validity partially depends on refuting or displacing the others in the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
