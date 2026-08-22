% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Marriage Practice Reversal via Endogenous Divine Reinterpretation (Woodruff 1890 Vision)
 *   domain: religious/institutional/political_theology
 *
 * SUMMARY:
 *   On September 23, 1890, church president Wilford Woodruff issued a public
 *   statement declaring that God had revealed to him the necessity of
 *   abandoning the practice of plural marriage (polygamy), which the
 *   institution had presented as an eternal covenant principle. This
 *   constraint story models the endogenous reinterpretation reading: the
 *   reversal is framed as internally-driven divine revelation, preserving the
 *   prophet's authority to interpret God's will under changed circumstances.
 *   The reading differs structurally from the exogenous_override reading
 *   (reversal as capitulation to federal force without doctrinal revision)
 *   and the practice_doctrine_gap reading (structural ambiguity where the
 *   eternal principle is retained doctrinally while suspended operationally).
 *   This reading's distinguishing claim is that the reversal itself
 *   demonstrates the prophet's authentic interpretive power — that God guided
 *   the institutional adaptation — rather than acknowledging external
 *   coercion or doctrinal inconsistency. The extractiveness is moderate (0.62
 *   at interval end) because the constraint preserves institutional
 *   legitimacy at the cost of theological consistency and practitioners'
 *   covenant commitments.
 *
 * KEY AGENTS:
 *   - prophet_leadership (institutional power, identity-locked to revelatory authority)
 *   - practicing_polygamists (moderate power, identity-locked to faith community; face covenant abandonment or concealment)
 *   - monogamist_members (moderate power, constrained exit; gain social legitimacy from the reversal)
 *   - federal_government (institutional power, trapped exclusion from legitimacy narrative)
 *   - theological_doctrine_principle (non-agent; tracks the doctrine-practice gap)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.62).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.71).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Marriage Practice Reversal via Endogenous Divine Reinterpretation (Woodruff 1890 Vision)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious/institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '3e84ca17-30ed-488e-804a-f1934bbd1227').
narrative_ontology:cs_kernel_codification('3e84ca17-30ed-488e-804a-f1934bbd1227', fixed_text).
narrative_ontology:cs_authority_grounding('3e84ca17-30ed-488e-804a-f1934bbd1227', lineage).
narrative_ontology:cs_interpretation_layer_present('3e84ca17-30ed-488e-804a-f1934bbd1227').
narrative_ontology:cs_reading_relation('3e84ca17-30ed-488e-804a-f1934bbd1227', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e84ca17-30ed-488e-804a-f1934bbd1227', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('3e84ca17-30ed-488e-804a-f1934bbd1227', foundational, endogenous_divine_revelation_warrants_doctrine_reinterpretation).
narrative_ontology:cs_axiom_status(endogenous_divine_revelation_warrants_doctrine_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('3e84ca17-30ed-488e-804a-f1934bbd1227', endogenous_divine_revelation_warrants_doctrine_reinterpretation, deontological).
narrative_ontology:cs_axiom('3e84ca17-30ed-488e-804a-f1934bbd1227', secondary, prophetic_authority_preserved_through_interpretive_innovation).
narrative_ontology:cs_axiom_status(prophetic_authority_preserved_through_interpretive_innovation, holdable).
narrative_ontology:cs_axiom_grounding('3e84ca17-30ed-488e-804a-f1934bbd1227', prophetic_authority_preserved_through_interpretive_innovation, deontological).
narrative_ontology:cs_reference_frame('3e84ca17-30ed-488e-804a-f1934bbd1227', eternal_plural_marriage_doctrine_section_132).
narrative_ontology:cs_drift_state('3e84ca17-30ed-488e-804a-f1934bbd1227', post_1890_federal_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e84ca17-30ed-488e-804a-f1934bbd1227', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, leadership_interpretive_authority).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_legitimacy).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, practitioners_with_existing_commitments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, monogamist_members).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, practicing_polygamists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The prophetic office is the exclusive authoritative interpreter of God's will for the institution. Wilford Woodruff holds this office and issues the revelation on Sept 23, 1890, framing the practice reversal as divinely-guided institutional adaptation. The prophet's identity is constituted through the claim to unmediated divine communication; the prophet cannot exit this role or this authority claim without ceasing to be the prophet. The prophet sets the terms under which the institution interprets the reversal: it is not federal coercion, but God's will revealed to guide the institution toward survival and moral alignment.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet_leadership, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Members who entered into plural marriage covenants under prior doctrine (Section 132, presented as an eternal spiritual principle binding in heaven and earth) are now commanded to dissolve or conceal those commitments. Their theological identity is built on the doctrine's validity and on their role as keepers of eternal principles. Abandoning the covenants creates cognitive, relational, and spiritual dissonance. Exit from the faith community means severing kinship networks, losing institutional roles, and severing their identity's foundational spiritual authority. Some choose concealment (keeping covenants hidden while appearing monogamous publicly); others abandon the covenants; a minority leave the institution entirely.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, practicing_polygamists, payer,
    moderate, biographical, identity_locked, local).

% Institutional members who rejected plural marriage covenants or never entered them gain social legitimacy, institutional status, and legal alignment. The reversal validates their position and elevates monogamy as the normative covenant form. They benefit from the constraint's enforcement because it institutionalizes their marital arrangement as the sole legitimate form and removes the doctrinal contradiction between the institutional principle (Section 132's alleged eternity) and their own practice (monogamy). Their exit options are constrained but available: they can leave the faith community if they choose, but the constraint removes the pressure to do so.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, monogamist_members, beneficiary,
    moderate, biographical, constrained, local).

% The federal government's coercive apparatus (property confiscation, criminal law against polygamy, statehood conditionality) created the structural pressure that made the reversal materially necessary. But the federal role is excluded from the institutional legitimacy narrative. The endogenous_reinterpretation reading redefines federal pressure as the circumstance in which God revealed his guidance, not as the cause of the reversal. Federal power is factually acknowledged (the circumstances that prompted revelation) but narratively erased (the revelation, not federal force, is presented as the warrant). This exclusion is the key extraction mechanism: the constraint transfers institutional obedience from a coercive external actor (the government) to an internal divine authority (the prophet), making compliance feel chosen rather than imposed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, generational, trapped, national).

% Section 132 (the eternal marriage principle claiming that plural marriage was revealed as binding in heaven and earth) is preserved textually in institutional canon but suspended operationally in institutional practice. The doctrine is not formally rescinded; it is de-authorized through the revelation narrative. As a non-agent entity, this doctrinal principle bears the cost of the constraint through the doctrine-practice gap: it remains theologically incoherent (how can an eternal principle be suspended?), creating an unresolved contradiction in the institution's doctrinal coherence. This tension is suppressed rather than resolved, making the doctrine itself a victim of the constraint.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_doctrine_section_132, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_doctrine_section_132).

% Legal scholars, historians of religion, and non-member observers analyze the 1890 reversal from outside the institution's interpretive frame. They examine the timing (immediate response to federal pressure after property confiscation and leadership imprisonment), the narrative strategy (framing capitulation as divine guidance), and the selective retention of Section 132 in private doctrine while abandoning it in public practice. They ask whether the reversal represents authentic theological evolution or policy-driven institutional adaptation dressed in revelation language. Their analysis is excluded from institutional legitimacy claims but is the primary external check on the constraint's narrative coherence.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, external_observers_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutional identity coordination: the institution faced an existential contradiction between its core doctrine (Section 132, claiming plural marriage as an eternal principle) and its structural survival (federal pressure making polygamy legally impossible). The revelation framing solves the coordination problem by repositioning the prophet's authority to reinterpret God's will as the mechanism that preserves institutional coherence. Instead of doctrinal revision (which would require acknowledging that the prior doctrine was wrong), the endogenous_reinterpretation reading frames institutional adaptation as authentic divine guidance — the prophet is able to hear God's will in new circumstances.
% TRANSFER_FUNCTION: Transfers covenant commitments (plural marriage covenants are abandoned or concealed), doctrinal coherence (Section 132's eternal status is suspended operationally while retained textually), and the legitimacy of institutional adaptation (from external federal coercion to internal divine revelation). What flows from practicing_polygamists to prophet_leadership is obedience and the relinquishment of covenant claims; what flows from the theological doctrine to the institution is coherence purchased at the cost of contradiction suppression.
% ABSENT_VOICES: Practicing polygamists who would articulate that the reversal was federally-driven rather than divinely-revealed are structurally discouraged from speaking — the revelation narrative delegitimizes their dissent as resistance to God's will. Federal actors are excluded from the legitimacy conversation; their coercive role is narrated as the circumstance in which God revealed guidance, not as the causally-driving force. Doctrinal scholars and theologians who note the persistent contradiction (Section 132 retained yet operationally abandoned) are marginalized as nitpickers on fine points of doctrine rather than as raising a fundamental coherence problem.
% DISAPPEARANCE_RATIONALE: If the 1890 reversal and the revelation authorizing it disappeared, the institution would either resume defending plural marriage as an eternal doctrine (reverting to pre-1890 position) or face renewed federal pressure and potential institutional dissolution. The reversal's disappearance would require reinstating the covenants and doctrine, which would trigger renewed federal persecution. Alternatively, the institution would need to explicitly revise Section 132 as mistaken doctrine, which would require acknowledging the prior revelation as false — a direct undermining of prophetic authority.
% FOUNDING_PROBLEM: Institutional survival under federal persecution. By 1890, the institution had faced two decades of escalating federal pressure: property confiscation, criminal imprisonment of leadership, and the threat that statehood would be withheld unless polygamy was abandoned. The founding problem was: how to preserve institutional identity, prophetic authority, and community coherence while abandoning the practice that constituted core doctrine and a defining institutional identity?
% FOUNDING_PROBLEM_CORROBORATION: Federal records, statehood admission documents, and court records establish that institutional survival pressure was acute and materially driven by federal coercion. The institution's own pre-1890 affirmations of Section 132 as eternal doctrine (in the Pearl of Great Price, in temple ceremonies, in leadership sermons) establish the contradiction the founding problem addresses. Historians and legal scholars outside the institution (Firmage, Grow, Campbell) attest that federal coercion was the material driver of the 1890 reversal and that the revelation narrative functioned to reframe institutional capitulation as theological evolution. The institution's own retention of Section 132 in canon and in private doctrine while abandoning it publicly corroborates that the founding problem was not resolved, but reframed through the revelation narrative.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (pre-reversal, where the institution faced acute state pressure) to 0.62 (stabilized post-1890 state where the reversal has been institutionalized). The rise reflects the constraint's successful operation: federal pressure is transformed into revelation-framed institutional policy, practitioners are required to abandon or conceal covenants, and institutional legitimacy is consolidated under the prophet's renewed authority. Theater_ratio is high initially (0.72) and declines to a stabilized 0.58, reflecting the initial narrative work required to reframe necessity as divine guidance, then plateau as the reversal becomes normalized institutional practice. Suppression_requirement rises sharply from 0.58 to 0.71 and plateaus, tracking the active enforcement cost of maintaining the revelation narrative (suppressing dissent from polygamists who view the reversal as coerced), concealing the ongoing private retention of Section 132 doctrine, and excluding federal coercion from the legitimacy account. All measurements use a shared time grid (intervals 0, 5, 10, 15, 25, 40) so the three metrics are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The prophet_leadership seat and the practicing_polygamists seat should compute very differently from this structural data. From the prophet's position (identity-locked to revelatory authority, institutional power, civilizational horizon), the arrangement preserves the office's core function — interpreting God's will — and demonstrates its power by reinterpreting eternal principles under changed circumstances. The prophet's extracted cost is personal (bearing the moral weight of the reversal) but the institutional benefit (renewed legitimacy, survival) outweighs it; d approaches the beneficiary end from this seat. From the practicing_polygamists seat (moderate power, identity-locked to faith community, biographical horizon), the constraint forces covenant abandonment or concealment, obliterating the covenants they entered under prior doctrine and trapping them between institutional obedience and doctrinal integrity; d approaches the full-target end. The federal government is excluded but structurally powerful; its d is analytically computed but its voice is narratively erased. The monogamist_members seat gains both institutional legitimacy and social alignment; their d approaches the beneficiary end. The engine computes these divergences from the power/exit/beneficiary declarations; they demonstrate the constraint's structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Prophet_leadership benefits from institutional legitimacy consolidation (beneficiary: institutional_legitimacy) and maintains interpretive authority (beneficiary: leadership_interpretive_authority). Their power is institutional, their time horizon civilizational, their exit_options identity_locked (the prophetic office cannot be exited without identity dissolution). This set of declarations produces low d — they are beneficiaries, they hold power, they have no exit. Practicing_polygamists are victims (undergo covenant abandonment/concealment), moderate power, biographical horizon, identity_locked to the faith community. The identity-lock is crucial: they cannot leave the community without severing kinship and institutional belonging. This set produces high d — they are targets, identity_locked prevents exit-via-leaving, their covenants are explicitly abandoned. Monogamist_members gain social legitimacy and institutional status (beneficiary role, though implicit); they are moderate power, biographical, constrained exit (can leave the faith but with social cost). This set produces middling d. Federal government is excluded (structurally powerful but narratively erased); analytical observers have analytical power/exit. The directionality logic chains from the structural asymmetry: the constraint preserves the prophet's authority while transferring costs to practitioners whose exit is identity-locked.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional survival under federal persecution) is live at t=0 but materially resolved by t=5-10 (statehood achieved in 1896, six years after the 1890 reversal). However, the constraint persists well past the founding problem's resolution: extractiveness remains at 0.62, theater_ratio stabilizes at 0.58, suppression_requirement plateaus at 0.71. This is the mandatrophy signature: the constraint's justifying function (solving federal persecution) has atrophied but the structure persists through institutional inertia and the revelation narrative's continued legitimation. The theater_ratio decline from 0.72 to 0.58 indicates that less active narrative work is required once the reversal is normalized — the revelation becomes historical fact rather than an active interpretive claim. But suppression_requirement stays high (0.71) because the constraint must continuously suppress: (a) practicing polygamists' dissent or alternative theology, (b) the doctrine-practice gap (Section 132 retained privately, suspended publicly), and (c) the federal coercion story itself (reframing capitulation as divine guidance). A genuine rope would show declining suppression once coordination succeeded; the plateau indicates the constraint is tangled — coordination (institutional survival) is genuinely real, but extraction (covenant abandonment, doctrinal suppression, erased federal role) persists even after the founding problem is resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression primarily structural (external enforcement, community ostracism) or internalized (the prophet''s authority claim embedded in practitioners'' identity)?',
    'Historical analysis of post-1890 dissent trajectories: if practicing polygamists who left the community reported suppression persisting after external institutional pressure was removed, the suppression is substantially internalized. If suppression dropped sharply upon institutional exit, the suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural barriers alone suggest — practitioners carry the prophet''s authority inside themselves. If structural, the suppression is contingent on ongoing institutional enforcement; it would attenuate if the institutional context dissolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Whether suppression mechanism is structural barriers or identity-internalized.').

omega_variable(
    revelation_as_authentic_divine_communication_vs_policy_narrative,
    'Is the 1890 revelation framed as endogenous divine guidance an authentic report of Woodruff''s experience, or a post-hoc narrative framing for an externally-driven policy decision?',
    'Textual and archival analysis (Woodruff''s private diaries, contemporaneous correspondence) comparing the revelation account to documented federal pressure timeline; theological analysis of whether the revelation claim is consistent with the institution''s broader epistemology of divine guidance.',
    'If the revelation is authentic (from Woodruff''s epistemic perspective), the endogenous_reinterpretation reading is structurally true — the constraint preserves prophetic authority through genuine interpretive innovation. If the revelation is a narrative reframing, the exogenous_override reading is more accurate — the constraint masks federal coercion as divine guidance, which is itself the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_as_authentic_divine_communication_vs_policy_narrative, conceptual, 'Whether the revelation narrative is authentic divine communication or post-hoc policy framing.').

omega_variable(
    doctrine_practice_gap_permanence,
    'Is the retention of Section 132 in private doctrine while suspending it in public practice a permanent structural feature of the constraint, or a transitional state pending full doctrinal revision?',
    'Historical analysis of subsequent institutional doctrine (1890–present): has the church formally rescinded Section 132 or clarified its status, or does it remain textually intact but operationally abandoned? What do theological leaders teach about its current authority?',
    'If permanent, the practice_doctrine_gap reading is structurally accurate — the constraint''s coherence depends on suppressing the contradiction. If transitional, the endogenous_reinterpretation reading is vindicated — the doctrine is being authentically reinterpreted toward monogamy. If the gap remains unresolved, extractiveness from the doctrine-principle itself is sustained (the theological victim bears continuous cost).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_practice_gap_permanence, empirical, 'Whether the doctrine-practice gap is permanent or transitional.').

omega_variable(
    identity_lock_gateway_to_exit,
    'For practicing_polygamists holding identity_locked exit, is the lock the outcome of free identity-investment (they have chosen community membership over exit options), or is it the product of relational coercion (the community has made exit-by-leaving structurally catastrophic)?',
    'Comparative analysis of (a) polygamists who chose to leave post-1890 and their narrative of that choice, (b) polygamists who stayed and their reported reasoning, (c) the actual costs of leaving (kinship severance, economic displacement, religious status loss) versus counterfactual worlds in which those costs were lower.',
    'If the lock is identity-investment, exit is available but chosen against; d is lower (the agent could leave and has chosen not to) and the constraint is less extractive. If the lock is relational coercion, exit is structurally unavailable; d is higher (true trapped exit) and the constraint is more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_gateway_to_exit, empirical, 'Whether identity-lock is chosen identity-investment or relational coercion.').

omega_variable(
    kernel_reading_legitimacy_competition,
    'Do the three kernel readings (endogenous_reinterpretation, exogenous_override, practice_doctrine_gap) represent equally-valid interpretations of the same kernel, or does one reading''s account of the reversal logically foreclose the others?',
    'Formal analysis of the three readings'' core premises: (1) endogenous says ''divine revelation is the primary warrant''; (2) exogenous says ''federal force is the primary driver''; (3) gap says ''the readings are incompletely resolved.'' Can a single coherent institutional framework hold more than one? Or does accepting one reading''s account require rejecting the others?',
    'If the readings coexist_with each other, they are held by different factions in ongoing dispute and the kernel remains genuinely contested. If one reading forecloses another, the kernel''s contestation is about which framework applies (one is logically true within the framework). The network.affects_constraints links will depend on this: coexistence creates lateral influence; foreclosure creates hierarchical dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy_competition, conceptual, 'Whether the three kernel readings logically coexist or one forecloses the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 5, 0.68).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 10, 0.64).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel marriage_commitment_reversal. The three readings (endogenous_reinterpretation, exogenous_override, practice_doctrine_gap) share the same referent (the 1890 practice reversal) but differ in how they frame the reversal's warrant and meaning. Each reading instantiates a different constraint with different beneficiary/victim structures and different extracted costs. The ε values differ substantially: endogenous_reinterpretation assigns moderate extractiveness (0.62) to the revelation narrative's preservation of institutional legitimacy; exogenous_override assigns higher extractiveness to the unacknowledged federal coercion; practice_doctrine_gap assigns high extractiveness to the unresolved doctrine-practice contradiction. These are not measurement-basis disagreements; they are genuinely different constraints, each ε-invariant within its own reading frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
