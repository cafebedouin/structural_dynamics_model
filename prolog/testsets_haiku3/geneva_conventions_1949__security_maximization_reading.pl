% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_1949_security_max, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions 1949 — Security Maximization Reading
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The security-maximization reading of the 1949 Geneva Conventions claims
 *   that humanitarian protections are peacetime aspirations that must yield
 *   to operational necessity when a state faces asymmetric conflict from
 *   irregular forces. Under this reading, the state gains authority to
 *   classify adversaries as unlawful combatants (denying them POW status and
 *   habeas corpus), to employ coercive interrogation normalized as
 *   non-torture, to detain indefinitely without trial, to degrade civilian
 *   immunity via human-shields doctrine, and to accept collateral damage
 *   within security-necessity frameworks. This reading instantiates one horn
 *   of a three-way kernel contest: humanitarian-ceiling readings treat the
 *   Conventions as absolute minimums; conditional-reciprocity readings permit
 *   proportional degradation when adversaries violate; security-maximization
 *   readings treat irregularity as justifying suspension. Each reading
 *   produces a structurally distinct constraint with different victim sets
 *   and enforcement machinery. This story generates ONLY the
 *   security-maximization reading as a clean, ε-invariant constraint,
 *   addressing the standing arrangement under contest as this reading defines
 *   it.
 *
 * KEY AGENTS:
 *   - state_security_apparatus: institutional power, analytical exit, agenda-setter role — sets and enforces the interpretation
 *   - detained_irregular_combatants: powerless, trapped, payer role — stripped of legal protections via administrative reclassification
 *   - civilian_populations_in_conflict_zones: powerless, trapped, payer role — civilian immunity degraded via human-shields logic and collateral damage acceptance
 *   - military_commanders: organized power, constrained exit, beneficiary role — gain operational freedom and reduced legal liability
 *   - humanitarian_organizations: organized power, constrained exit, excluded role — would contest but are blocked from access and enforcement
 *   - international_courts: institutional power, constrained exit, excluded role — would adjudicate but are preempted by state sovereignty and non-cooperation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.89).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.92).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949 — Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'b8953caa-0efa-4936-8b29-d11ce3803fdc').
narrative_ontology:cs_kernel_codification('b8953caa-0efa-4936-8b29-d11ce3803fdc', fixed_text).
narrative_ontology:cs_authority_grounding('b8953caa-0efa-4936-8b29-d11ce3803fdc', extraction).
narrative_ontology:cs_interpretation_layer_present('b8953caa-0efa-4936-8b29-d11ce3803fdc').
narrative_ontology:cs_reading_relation('b8953caa-0efa-4936-8b29-d11ce3803fdc', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('b8953caa-0efa-4936-8b29-d11ce3803fdc', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('b8953caa-0efa-4936-8b29-d11ce3803fdc', foundational, humanitarian_protections_yield_to_necessity).
narrative_ontology:cs_axiom_status(humanitarian_protections_yield_to_necessity, holdable).
narrative_ontology:cs_axiom_grounding('b8953caa-0efa-4936-8b29-d11ce3803fdc', humanitarian_protections_yield_to_necessity, instrumental).
narrative_ontology:cs_axiom('b8953caa-0efa-4936-8b29-d11ce3803fdc', foundational, irregular_status_removes_legal_standing).
narrative_ontology:cs_axiom_status(irregular_status_removes_legal_standing, holdable).
narrative_ontology:cs_axiom_grounding('b8953caa-0efa-4936-8b29-d11ce3803fdc', irregular_status_removes_legal_standing, empirically_contingent).
narrative_ontology:cs_reference_frame('b8953caa-0efa-4936-8b29-d11ce3803fdc', state_security_necessity_primacy).
narrative_ontology:cs_drift_state('b8953caa-0efa-4936-8b29-d11ce3803fdc', post_2001_global_war_on_terror, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b8953caa-0efa-4936-8b29-d11ce3803fdc', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, prisoners_of_war_reclassified_unlawful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, military_commanders).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, state_survival_trumps_treaty_obligation).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, security_necessity_overrides_humanitarian_floor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Geneva Conventions through a security maximization lens: the state has inherent authority to suspend humanitarian protections when operational necessity demands it. Sets interrogation doctrine, detention classification systems, and combatant-status frameworks. Administers the constraint by classifying adversaries as 'unlawful combatants' to strip them of POW protections, by normalizing coercive interrogation as non-torture, and by authorizing indefinite detention without trial. Collects expanded operational freedom and diminished legal accountability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, state_security_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).

% Reclassified from prisoners of war (who retain habeas corpus, trial rights, and legal status) to 'unlawful combatants' or 'enemy combatants,' stripping them of Convention protections. Subject to indefinite detention without trial, coercive interrogation techniques normalized as non-torture, and denial of legal counsel. Exit from the constraint means escape or state discretionary release; the legal framework provides no recourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detained_irregular_combatants, payer,
    powerless, biographical, trapped, local).

% Civilian immunity under the Conventions is degraded through expanded application of the 'human shields' doctrine (civilians lose immunity if irregular forces operate near them) and collateral damage acceptance frameworks that permit civilian casualties when military advantage is deemed sufficient. Trapped in zones where the constraint's logic permits their targeting or displacement. No legal recourse against state characterization of their neutrality status.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones, payer,
    powerless, biographical, trapped, regional).

% Combatants who would qualify for POW status under the Conventions are administratively reclassified as unlawful combatants to deny them trial rights, release-at-end-of-hostilities guarantees, and humane treatment standards. Subjected to indefinite detention and interrogation coercion justified as operational security. The reclassification is administrative, not subject to independent review.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, prisoners_of_war_reclassified_unlawful, payer,
    powerless, biographical, trapped, local).

% Gain operational freedom: expanded authority to interrogate without legal constraint, to detain without trial, to target civilians under human-shields logic, and to authorize collateral damage within security-necessity frameworks. Reduced liability for decisions that would constitute war crimes under the humanitarian-ceiling reading. Exit requires adherence to stricter humanitarian standards, which commanders argue reduces operational effectiveness.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, military_commanders, beneficiary,
    organized, biographical, constrained, regional).

% Would monitor and report violations of humanitarian law protections. Structurally excluded from access to detention facilities, interrogation sites, and targeting decisions. Their testimony and documentation are treated as propaganda or selective reporting. They have standing to contest but lack enforcement mechanisms.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Would adjudicate war crimes and crimes against humanity. This reading's operational doctrine treats the security-maximization interpretation as a binding state prerogative that preempts court jurisdiction. International courts face state non-cooperation, immunity claims, and the argument that security necessity is a justified exception to law. Structurally sidelined.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts, excluded,
    institutional, generational, constrained, global).

% The target of the constraint: irregular forces are classified as unlawful, unprotected by the Conventions, and subject to unlimited state countermeasures. The constraint's logic denies them reciprocal protection by reframing them outside the legal order entirely. They have no seat at the interpretation table and no legal recourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, adversary_irregular_forces, excluded,
    moderate, biographical, trapped, regional).

% States that signed the 1949 Conventions and their Additional Protocols. This reading instantiates one state's unilateral interpretation. Other signatory states contest it, adopt competing readings, or maintain dual positions (humanitarian ceiling domestically, security maximization operationally). The collective observes the interpretive contest without enforcing a unified doctrine.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, treaty_signatories_collectively, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuinely present. The stated coordination function is mutual restraint among signatories to the Conventions — to establish humanitarian minimums that constrain all parties symmetrically. This reading inverts that: it treats the Conventions as inapplicable when one party operates irregularly, dissolving the reciprocal restraint mechanism rather than maintaining it.
% TRANSFER_FUNCTION: Moves legal accountability and operational constraint from the state security apparatus to detained combatants and civilians, and from international courts to state discretion. The state transfers unlimited interrogation authority, indefinite detention power, and targeting flexibility from itself to itself — it captures expanded operational freedom while stripping the traditional beneficiaries of the Conventions (prisoners, civilians) of protection.
% ABSENT_VOICES: Detained adversaries have no voice in the interpretation process. Humanitarian organizations are excluded by access restriction. International courts are excluded by state sovereignty claims and non-cooperation. The adversary irregular forces have no seat at any interpretation table and no legal standing to contest reclassification. Their exclusion is structural to the constraint: the security-maximization reading is authored by the state alone.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight and states reverted to the humanitarian-ceiling reading, interrogation authority would be constrained, indefinite detention would end, civilian immunity would be restored, and POW status would be granted to all regular combatants and most irregulars. International courts would reassert jurisdiction. The state would argue operational capacity is severely degraded; the humanitarian organizations would argue restraint becomes feasible. The contest is real: the constraint's disappearance would reorganize authority and legal accountability fundamentally.
% FOUNDING_PROBLEM: Asymmetric warfare: the 1949 Conventions were designed for state-vs-state conflict with clear combatant identification and reciprocal compliance. Irregular forces do not wear uniforms, hide among civilians, and do not comply with the Conventions. The security-maximization reading claims these conditions justify suspending humanitarian constraints to prevent catastrophic attacks on civilian populations and military personnel.
% FOUNDING_PROBLEM_CORROBORATION: State security officials and military strategists attest the problem is live and urgent: irregular forces exploit humanitarian law to hide among protected populations and conduct attacks without constraint, so humanitarian constraints on state response are operationally lethal. Humanitarian organizations, international court officials, and academic experts on law of war attest the problem is substantially mischaracterized: irregular forces do commit violations, but (a) framing the entire irregular-warfare context as justifying suspension of humanitarian law removes the very constraints that make states' own conduct reviewable, and (b) empirical evidence from conflicts shows humanitarian restraint and operational effectiveness are not opposed — states that maintain stricter humanitarian standards do not suffer systematic operational defeat. Legislative bodies in signatory states are divided; formal treaty monitoring bodies reject the security-maximization reading as inconsistent with the Conventions' text and object. No consensus outside the state security apparatus itself.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, contested).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.89) because the constraint strips legal protections from a powerless population (detained combatants and civilians) and concentrates operational authority in the state apparatus with minimal external review. The measurement trajectory shows extraction rising steeply in the first 15 years of the post-2001 security regime (0.72→0.85), then plateauing (0.85→0.89) as the interpretation becomes institutionalized and normalized. Suppression is even higher (0.92) because the constraint's persistence depends on actively preventing three forms of constraint: (1) international courts cannot access detention facilities or interrogation decisions, (2) humanitarian organizations cannot conduct independent monitoring, (3) detained combatants have no legal recourse to challenge reclassification. Theater-ratio rises from 0.45 to 0.68 over the interval: early justifications emphasize genuine operational security (theater ~45%), but as the regime matures, the functional interrogation activity remains flat while administrative apparatus (classification reviews, legal memoranda justifying non-torture status, security briefings) expands — the theater portion grows because more energy goes to performing justification than to extracting new operational intelligence. Accessibility collapse is moderate-high (0.78) because alternatives exist in principle (humanitarian-ceiling interpretation, conditional-reciprocity interpretation, international court authority) but are actively suppressed by state non-cooperation and sovereignty claims. Resistance is substantial (0.71) from humanitarian organizations, some state actors, and academic experts, but is structurally excluded from enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The state security apparatus and military commanders experience the constraint as enabling operational necessity — they see it as expanding legitimate authority to conduct asymmetric warfare. Detained combatants and civilians experience it as extractive deprivation of legal standing and vulnerability to coercive abuse. Humanitarian organizations and international courts experience it as a unilateral reinterpretation that overrides the treaty framework they are charged with monitoring. The engine computes these divergences from the structural data: the state's directionality is near-zero (full beneficiary), detained combatants' directionality is near 1.0 (full target), and international institutions' directionality is asymmetric (they bear costs of enforcing a framework the state rejects, but they do not collect extraction rents). The same constraint measured from each seat produces different effective extraction values.
 *
 * DIRECTIONALITY LOGIC:
 *   State security apparatus: beneficiary role, institutional power, analytical exit → d ≈ 0.1 (full beneficiary: collects operational freedom, bears none of the extraction costs, has perfect exit via unilateral reinterpretation). Military commanders: beneficiary role, organized power, constrained exit → d ≈ 0.15 (benefits operationally but carries residual liability in international forums; exit requires accepting tighter humanitarian constraints). Detained combatants: payer role, powerless, trapped → d ≈ 0.95 (full target: stripped of legal status, subjected to indefinite detention, no exit, identity-locked as 'unlawful combatant' by state classification). Civilians in conflict zones: payer role, powerless, trapped → d ≈ 0.92 (full target: made vulnerable through human-shields logic and collateral damage acceptance, no exit from the zone). Humanitarian organizations: excluded role, organized power, constrained exit → d ≈ 0.70 (structurally sidelined, bear enforcement costs, gain no rents; exit would mean abandoning monitoring mission). International courts: excluded role, institutional power, constrained exit → d ≈ 0.75 (preempted from adjudicating, carry reputational cost of appearing complicit or powerless, cannot exit treaty obligations).
 *
 * MANDATROPHY ANALYSIS:
 *   The security-maximization reading instantiates a clear mandatrophy: the founding mandate was mutual restraint through humanitarian minimums ('we all agree to constrain warfare so prisoners, civilians, and wounded are protected regardless of who wins'). The reading reinterprets this mandate to mean 'we constrain OUR response only when adversaries comply,' which dissolves the reciprocal restraint and converts the Conventions from a mutual-protection regime into a unilateral authorization for the state. The mandate has been inverted, not abandoned — the constraint still claims to be 'about' the Conventions, but the operational function has shifted from mutual restraint to unilateral exception-carving. Theater-ratio rising to 0.68 indicates substantial performative maintenance: legal memoranda justifying techniques as non-torture, security briefings asserting operational necessity, classification reviews claiming due process — the institutional apparatus is devoted increasingly to performing compliance with the reading's own framing rather than extracting new intelligence or capturing adversaries. This is textbook piton-trajectory behavior, but the high extractiveness and suppression keep it classified as snare rather than piton: a piton would have diffuse costs distributed across many actors with no concentrated beneficiary; here the benefits are concentrated in the state apparatus and the costs are concentrated in detained/civilian payers. The constraint persists because it serves the state's interests, not because of institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_necessity_definition,
    'What constitutes ''operational necessity'' that justifies suspending humanitarian protections? Is the threshold a genuine military exigency (immediate survival threat, catastrophic attack prevention) or a general efficiency claim (interrogation is effective, indefinite detention reduces recapture risk)?',
    'Empirical analysis of interrogation efficacy (coercive vs. non-coercive methods; do indefinitely detained combatants provide actionable intelligence or recidivism prevention?); case-by-case review of specific detention and interrogation decisions against stated operational objectives.',
    'If necessity is defined narrowly (genuine survival/catastrophic-attack threshold), the measured extractiveness is partially justified as coordination cost; if defined broadly (any efficiency gain), the extractiveness is pure rent-seeking. The classification could shift from snare toward tangled_rope under the narrow reading, or remain snare under the broad reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_definition, empirical, 'The definition of ''operational necessity'' determines whether the constraint is functional coordination or pure extraction.').

omega_variable(
    irregular_force_characteristics,
    'Does the label ''irregular forces'' correctly identify an operational category distinct from uniformed combatants, or does it serve primarily as a classification device to strip legal protections from politically disfavored adversaries?',
    'Systematic analysis of state classification decisions: are classification criteria applied consistently across conflicts, or are criteria applied asymmetrically to deny protections to particular political actors (e.g., non-state movements) while granting them to others?',
    'If irregularity is a genuine operational category, some constraints on state response are justified; if it is a reclassification device, the constraint is pure extraction masquerading as operational necessity. This omega directly addresses whether the constraint''s beneficiary structure (state apparatus gains authority, combatants lose status) is a response to real asymmetry or a unilateral power grab.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irregular_force_characteristics, empirical, 'Whether ''irregular forces'' is an operational category or a reclassification device for political advantage.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of detained combatants and international oversight structural (external barriers: physical isolation, legal immunity, non-cooperation) or internalized (detained combatants come to accept reclassification as legitimate, international bodies internalize state sovereignty claims as limitations on their own authority)?',
    'Post-release interviews with formerly detained combatants; analysis of institutional behavior by international courts and humanitarian bodies (do they accept state sovereignty claims as binding, or do they actively contest them?); tracking of suppression after release (do detained persons continue accepting reclassification narratives after exit from physical detention?).',
    'If suppression is primarily structural, it could in principle be removed by changing the state''s enforcement machinery; if internalized, the constraint''s persistence depends on belief systems that would persist even after external barriers were removed. High internalization would indicate the constraint has become self-enforcing through narrative capture and institutional acquiescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized — critical to understanding if the constraint persists by coercion or by consent to the reading.').

omega_variable(
    humanitarian_ceiling_vs_security_max_foreclosure,
    'Do the humanitarian-ceiling reading and the security-maximization reading logically foreclose one another, or do they coexist as genuinely competing interpretations that different parties hold simultaneously?',
    'Formal analysis of the axiomatic commitments: does accepting security-maximization''s core premise (humanitarian protections yield to operational necessity) logically entail rejecting humanitarian-ceiling''s core premise (humanitarian protections are absolute minimums)? Or can a party hold both by context-shifting (humanitarian ceiling applies to our conduct, security maximization applies when we face threats)?',
    'If they foreclose, the kernel itself is contested at the logical level and one reading must be overridden. If they coexist, the constraint persists as an interpretive plurality, with the security-maximization reading winning operationally through state enforcement capacity rather than logical resolution. This omega determines whether the kernel contest can be ''resolved'' at all or is structurally endemic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_ceiling_vs_security_max_foreclosure, conceptual, 'Whether competing Conventions readings are logically foreclosed or genuinely coexist.').

omega_variable(
    treaty_binding_force_under_reinterpretation,
    'When a signatory state unilaterally reinterprets a treaty to mean the opposite of its original humanitarian intent, does the treaty remain binding, or does unilateral reinterpretation constitute effective denunciation?',
    'Legal analysis by non-signatory experts and treaty monitoring bodies; examination of state practice (do other signatories treat the state''s reinterpretation as a valid reading or as breach?); analysis of dispute-settlement mechanisms (do treaty bodies treat reinterpretation as falling within state discretion or as breach subject to remedy?).',
    'If reinterpretation is a valid exercise of state treaty-signing authority, the constraint is binding on all signatories and the security-maximization reading is an authorized hermeneutic. If reinterpretation constitutes de facto breach, the state has exited the regime and the constraint applies only to other signatories. This omega addresses whether the constraint''s authority is legitimate or usurped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_binding_force_under_reinterpretation, conceptual, 'Whether unilateral reinterpretation of humanitarian treaty obligations is authorized or constitutes breach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t5, geneva_conventions_1949__security_maximization_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement_basis(gene_tr_t5, observed).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_1949__security_maximization_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(gene_tr_t10, observed).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__security_maximization_reading, theater_ratio, 15, 0.63).
narrative_ontology:measurement_basis(gene_tr_t15, observed).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.66).
narrative_ontology:measurement_basis(gene_tr_t20, observed).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_1949__security_maximization_reading, theater_ratio, 25, 0.67).
narrative_ontology:measurement_basis(gene_tr_t25, observed).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_1949__security_maximization_reading, theater_ratio, 30, 0.68).
narrative_ontology:measurement_basis(gene_tr_t30, observed).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_1949__security_maximization_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(gene_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t5, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement_basis(gene_be_t5, observed).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement_basis(gene_be_t10, observed).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement_basis(gene_be_t15, observed).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.87).
narrative_ontology:measurement_basis(gene_be_t20, observed).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement_basis(gene_be_t25, observed).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement_basis(gene_be_t30, observed).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 40, 0.89).
narrative_ontology:measurement_basis(gene_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.81).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t5, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 5, 0.84).
narrative_ontology:measurement_basis(gene_su_t5, observed).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement_basis(gene_su_t10, observed).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement_basis(gene_su_t15, observed).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.9).
narrative_ontology:measurement_basis(gene_su_t20, observed).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 25, 0.91).
narrative_ontology:measurement_basis(gene_su_t25, observed).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 30, 0.92).
narrative_ontology:measurement_basis(gene_su_t30, observed).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement_basis(gene_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__security_maximization_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, state_sovereignty_vs_international_law).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, irregular_warfare_classification_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Geneva Conventions 1949 kernel. Sibling readings (humanitarian_ceiling_reading, conditional_reciprocity_reading) are separate constraint stories with different ε values, victim sets, and enforcement machinery. All three instantiate the same treaty text but diverge on whether humanitarian protections are absolute (ceiling reading), reciprocal (conditional-reciprocity reading), or contingent on operational necessity (security-maximization reading). The three readings coexist across different state positions and international forums. Each should be generated as a separate constraint story and linked via network.affects_constraints to enable cross-reading comparison and contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
