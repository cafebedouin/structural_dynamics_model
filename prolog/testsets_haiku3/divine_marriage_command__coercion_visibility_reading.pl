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
 *   constraint_id: divine_marriage_command__coercion_visibility_reading
 *   human_readable: Divine Marriage Command (Coercion Visibility Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint models the coercion_visibility_reading of the divine
 *   marriage command kernel. The reading holds that the Manifesto (the
 *   institutional declaration rescinding plural marriage doctrine) is an
 *   acknowledged response to federal coercion, and that the institution's
 *   theological legitimacy derives from institutional survival necessity
 *   under state pressure rather than from immutable revelation. The reading
 *   closes the M-set gap by admitting that exogenous political pressure was a
 *   valid input to doctrinal change, and that the authority structure grounds
 *   legitimacy partly in survival strategy rather than purely in claimed
 *   revelation. This reading coexists with the continuationist reading
 *   (plural marriage remains doctrinally valid despite institutional
 *   suspension under duress) and the substitutionist reading (monogamy is now
 *   divinely commanded, Manifesto is new revelation). Each reading is a
 *   separate constraint story with its own ε and stakeholder structure.
 *
 * KEY AGENTS:
 *   - institutional_leadership: sets agenda, controls theological narrative, benefits from survival and legitimacy preservation
 *   - polygamist_members: powerless, identity-locked, bear the cost of renunciation
 *   - women_in_plural_marriage: trapped, doubly constrained by removal of arrangement and legal vulnerability
 *   - federal_government: excluded from theological authority but causal to the constraint
 *   - dissenting_members: excluded through schism, maintain continuationist counter-reading
 *   - theological_interpreters: analytical observers who can attest the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__coercion_visibility_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__coercion_visibility_reading, 0.71).
domain_priors:theater_ratio(divine_marriage_command__coercion_visibility_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(divine_marriage_command__coercion_visibility_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__coercion_visibility_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__coercion_visibility_reading, "Divine Marriage Command (Coercion Visibility Reading)").
narrative_ontology:topic_domain(divine_marriage_command__coercion_visibility_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__coercion_visibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__coercion_visibility_reading, '665b167b-a0d5-4dae-812e-5ea8a84dfdae').
narrative_ontology:cs_kernel_codification('665b167b-a0d5-4dae-812e-5ea8a84dfdae', formalized).
narrative_ontology:cs_authority_grounding('665b167b-a0d5-4dae-812e-5ea8a84dfdae', extraction).
narrative_ontology:cs_interpretation_layer_present('665b167b-a0d5-4dae-812e-5ea8a84dfdae').
narrative_ontology:cs_reading_relation('665b167b-a0d5-4dae-812e-5ea8a84dfdae', divine_marriage_command__continuationist_reading, coexists_with).
narrative_ontology:cs_reading_relation('665b167b-a0d5-4dae-812e-5ea8a84dfdae', divine_marriage_command__substitutionist_reading, influences).
narrative_ontology:cs_axiom('665b167b-a0d5-4dae-812e-5ea8a84dfdae', foundational, federal_coercion_valid_doctrinal_input).
narrative_ontology:cs_axiom_status(federal_coercion_valid_doctrinal_input, holdable).
narrative_ontology:cs_axiom_grounding('665b167b-a0d5-4dae-812e-5ea8a84dfdae', federal_coercion_valid_doctrinal_input, instrumental).
narrative_ontology:cs_axiom('665b167b-a0d5-4dae-812e-5ea8a84dfdae', secondary, revelation_contingent_on_survival_necessity).
narrative_ontology:cs_axiom_status(revelation_contingent_on_survival_necessity, holdable).
narrative_ontology:cs_axiom_grounding('665b167b-a0d5-4dae-812e-5ea8a84dfdae', revelation_contingent_on_survival_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('665b167b-a0d5-4dae-812e-5ea8a84dfdae', revelation_grounded_immutable_authority).
narrative_ontology:cs_drift_state('665b167b-a0d5-4dae-812e-5ea8a84dfdae', post_manifesto_federal_pressure_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('665b167b-a0d5-4dae-812e-5ea8a84dfdae', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(divine_marriage_command__coercion_visibility_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, polygamist_members).
narrative_ontology:constraint_victim(divine_marriage_command__coercion_visibility_reading, women_in_plural_marriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_marriage_command__coercion_visibility_reading, women_in_plural_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ecclesiastical hierarchy that authored and enforces the Manifesto doctrine. They frame the marriage command as divine direction contingent on circumstances; they control the theological narrative, administer the membership gate, and retain the authority to interpret what coercive pressure means for doctrine. They benefit from the Manifesto because it preserves institutional survival under federal pressure while maintaining a legitimacy framework that grounds authority in revelation rather than state accommodation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Those who accepted plural marriage as a binding divine command. They now face institutional pressure to dissolve existing marriages or face excommunication and social exclusion from the community. Exit from the religious community means severing family bonds, economic relationships, and identity. The constraint extracts the renunciation of marriage claims they understood to be divinely ordained.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, polygamist_members, payer,
    powerless, biographical, identity_locked, national).

% Women in plural marriages occupy a doubly constrained position. They bore children under a doctrine now officially rescinded; they carry economic and social vulnerability tied to the arrangement's legal status. Some experience the Manifesto as liberation from coercive plural marriage norms; others experience it as abandonment by institutional authority that commanded the arrangement. Either way, the constraint determines the terms of their family and property rights.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, women_in_plural_marriage, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(divine_marriage_command__coercion_visibility_reading, women_in_plural_marriage, beneficiary).

% The external coercive actor that applied prosecution, property seizure, and institutional pressure. The Manifesto represents the institution's capitulation to federal authority. The federal government is excluded from the theological authority structure but is the structural cause of the doctrinal shift. From the institution's perspective, admitting federal pressure as a valid ground for theological revision would undermine authority claims.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Members who rejected the Manifesto as theological betrayal and separated to form rival communities. They would argue that true revelation cannot be contingent on state persecution, and that institutional leadership abandoned divine command for political survival. Their voice is structurally excluded from the institutional authority structure because schism is the exit mechanism.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, dissenting_members, excluded,
    moderate, biographical, constrained, national).

% Scholars, theologians, and historians outside the institutional structure who analyze how revelation claims interact with political pressure. They document the constraint's operation and can attest whether the Manifesto represents genuine doctrinal shift, contingent accommodation, or institutional survival strategy dressed in theological language.
narrative_ontology:constraint_stakeholder(divine_marriage_command__coercion_visibility_reading, theological_interpreters, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__coercion_visibility_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(divine_marriage_command__coercion_visibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The marriage command (plural or monogamous, depending on reading) coordinates reproductive and kinship arrangements within the religious community under a shared normative framework grounded in claimed revelation rather than individual contract or civil law.
% TRANSFER_FUNCTION: Under this reading, the Manifesto transfers decision-making authority over marriage doctrine from claimed direct revelation to institutional leadership operating under federal coercion. It moves authority over binding life commitments from a revealed command to a survival-necessity accommodation, making legitimacy contingent on state pressure rather than immutable doctrine.
% ABSENT_VOICES: Dissenting members who maintained continuity with the original command are excluded through schism; their argument that true revelation cannot be rescinded by persecution is structurally silenced by institutional authority claiming interpretive monopoly. The federal government's coercive role is officially absent from the theological legitimacy frame, even though it is the structural cause of the shift.
% DISAPPEARANCE_RATIONALE: If the Manifesto and its enforcement vanished, the institution would either restore polygamy doctrine (continuationist path, affirmed by dissenting communities) or face schism reversal; the marriage arrangements of existing families would remain but lack institutional framing. The theological authority structure itself would face credibility crisis: had coercion validly altered doctrine, what prevents future federal pressure from altering it again? The constraint's disappearance would expose the legitimacy mechanism underlying the authority structure.
% FOUNDING_PROBLEM: Federal government's criminal prosecution, property seizure, and political marginalization of the institution made continuation of plural marriage doctrine politically unsustainable while maintaining institutional existence and social legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The institutional leadership and contemporary sympathetic historians attest the founding problem (federal pressure making continuity politically impossible). Dissenting community members and external theologians attest that the problem was manufactured by institutional capitulation, not inherent to revelation itself — they argue the real founding problem was whether revelation could legitimately be contingent on state power. Federal historical records confirm the prosecution and pressure; they do not adjudicate whether it 'forced' doctrinal revision or merely created political cost.
narrative_ontology:disappearance_verdict(divine_marriage_command__coercion_visibility_reading, contested).
narrative_ontology:founding_problem_status(divine_marriage_command__coercion_visibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__coercion_visibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 endpoint) because the constraint extracts renunciation of marriage commitments that members understood to be divinely ordained; the gain accrues to institutional leadership (political legitimacy, federal compliance, institutional survival). Suppression is high (0.71) because members who resist the Manifesto face excommunication, social exclusion, and identity rupture; their alternatives collapse once they grasp the constraint's terms. Theater is moderate-to-high (0.52 endpoint) because the Manifesto is presented as new revelation or contingent interpretation while actually responding to federal coercion — the performative frame (revelation-grounded authority) disguises the structural reality (survival-necessity accommodation). Accessibility_collapse is high (0.74) because understanding the constraint means grasping that doctrinal legitimacy is contingent on state pressure, which undermines the authority structure itself — the only accessible path forward is acceptance of the Manifesto frame or schism. Resistance is moderate (0.58) because dissenting members did resist and split off, but the institutional majority accommodated, limiting active resistance at any one time. The measurement series show extractiveness and theater rising sharply in the 0–15 interval (when the Manifesto was announced and enforced) and stabilizing thereafter; suppression also stabilizes but remains high because the constraint must be continuously enforced to prevent backsliding to the original command.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's seat, the Manifesto is a necessary accommodation to federal pressure that preserves institutional existence and authority — a tragic but legitimate exercise of interpretive authority under duress. From the polygamist members' seat, the same Manifesto is a betrayal of a command they understood to be binding, now revoked without theological justification, leaving them with dissolved marriages and damaged identity. From the women's seat, the Manifesto is ambiguous: some experience it as liberation from coercive norms; others as abandonment by an authority structure that commanded the arrangement and then rescinded it. The engine's per-seat classification will capture these divergences: from the leadership seat, the constraint may compute as a transient scaffold (temporary accommodation); from the member seats, as a snare (extraction enforced through identity-lock and exclusion).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is near the beneficiary end (d ≈ 0.15–0.25): they benefit from survival and legitimacy in the face of federal pressure. Polygamist members are near the target end (d ≈ 0.85–0.95): they are powerless, identity-locked (cannot exit without severing religious identity and community bonds), and bear the extraction directly. Women in plural marriage sit slightly lower than polygamist men (d ≈ 0.88–0.92) because they face compounded vulnerability — removal of marriage status and legal claims — even though some experience liberation from coercive plural norms. The federal government has no directionality in the constraint's perception (it is excluded from the internal theological frame) but is the structural cause of the extraction. Dissenting members have moderate d (≈ 0.65–0.75) because they resisted and exited but still bear the cost of schism and community rupture.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits incipient mandatrophy: the founding problem (federal pressure making plural marriage politically unsustainable) is contested — some parties argue it is real and inescapable; others argue it is manufactured by institutional capitulation rather than inherent to revelation. The constraint persists because the institution enforces the Manifesto, not because all parties agree on why the founding problem required the solution. A genuine Tangled Rope would have a coordination function (marriage arrangements) that all parties agreed required the institutional frame. This constraint has that coordination function, but the extraction component (renunciation under pressure) and the coordination component (kinship framing) are increasingly decoupled. If the founding problem is dead (i.e., federal pressure has lessened or members stopped expecting reversibility), but the constraint persists (institutional enforcement of the Manifesto continues), the constraint will reclassify toward Piton — theater-maintained form without live function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_validity_for_doctrine,
    'Is federal coercion a structurally valid input to doctrinal change, or does admitting it as valid undermine the entire authority structure''s claim to revelation-grounded legitimacy?',
    'Historical-comparative analysis: if multiple revelatory communities show doctrinal changes explicitly justified by external pressure, coercion-validity becomes normalized within revelation-based authority; if the institutional leadership''s own exegetical tradition rejects coercion as a valid input, admitting it represents a break from their own epistemic standards.',
    'If coercion is valid input, the constraint reclassifies as Tangled Rope with admitted asymmetric extraction (coordination + survival accommodation). If coercion is NOT valid input, the constraint reclassifies as Snare (pure extraction disguised as revelation), and the institutional leadership loses legitimacy claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_validity_for_doctrine, conceptual, 'Whether external political pressure can validly ground changes to doctrine claimed to be revealed.').

omega_variable(
    restoration_reversibility,
    'Can the institution revoke the Manifesto and restore plural marriage doctrine if federal pressure were to cease?',
    'Institutional statements of doctrinal permanence or contingency; historical precedent within the tradition for doctrinal reversal; federal law changes that would permit plural marriage.',
    'If reversibility is genuine, the constraint is Scaffold (temporary accommodation under pressure). If the Manifesto is now permanent, the constraint is Tangled Rope (permanent extraction under past pressure, now institutionalized). If the institution claims permanent reversal is impossible, the constraint moves toward Snare (permanent extraction from polygamists, now locked in place by own authority claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_reversibility, empirical, 'Whether the Manifesto''s doctrinal change is contingent on federal pressure or permanent.').

omega_variable(
    identity_lock_mechanism,
    'Is the measured suppression (0.71) structural (legal barriers, economic dependency on institutional community) or internalized (members believe the Manifesto''s theological frame and accept its validity)?',
    'Post-schism trajectories: if suppression persists in members who exit the institutional structure, it is internalized; if suppression dissipates, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the scalar suggests — members carry the constraint with them after institutional exit. The identity-lock exit option becomes even more restrictive. If structural, remedies that remove external barriers (legal plural marriage permission, economic alternatives to institutional community) would reduce suppression measurably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether suppression is structural (external barriers) or internalized (belief-based acceptance).').

omega_variable(
    revelation_vs_survival_priority,
    'Does the institutional leadership''s theological framework genuinely prioritize survival necessity as a valid ground for doctrinal change, or is this admission (closing the M-set gap) a post-hoc rationalization of a survival-driven decision?',
    'Comparison of institutional doctrine before and after federal pressure: if survival-necessity is invoked as a general principle in other contexts, it is systemic; if invoked only for this case, it is rationalization.',
    'If systemic, the authority structure operates under a hybrid model (revelation + survival pragmatism), which is internally coherent but less theologically pure than claimed. If rationalization, the constraint is Snare with a false frame (claimed revelation, actually survival extraction). The legitimacy crisis deepens either way, but in opposite directions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_survival_priority, conceptual, 'Whether survival necessity is a principled ground for doctrinal change or post-hoc rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__coercion_visibility_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__coercion_visibility_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(divi_tr_t5, divine_marriage_command__coercion_visibility_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__coercion_visibility_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(divi_tr_t15, divine_marriage_command__coercion_visibility_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(divi_tr_t25, divine_marriage_command__coercion_visibility_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__coercion_visibility_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divi_be_t5, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(divi_be_t15, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(divi_be_t25, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__coercion_visibility_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(divi_su_t5, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(divi_su_t15, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(divi_su_t25, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__coercion_visibility_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__coercion_visibility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_marriage_command__coercion_visibility_reading, 0.12).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__coercion_visibility_reading, divine_marriage_command__substitutionist_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel decomposes into three constraint stories (coercion_visibility_reading, continuationist_reading, substitutionist_reading), each with different ε, beneficiary structures, and authority-grounding claims. Each reading instantiates a different constraint: the coercion_visibility reading admits federal pressure as a valid causal input (M-set gap closed), which influences but does not foreclose the sibling readings. All three readings coexist as live institutional positions held by different factions: institutional leadership (coercion_visibility), dissenting communities (continuationist), and some modern exegetical movements (substitutionist). The sibling constraints are linked via network.affects_constraints to enable family analysis of how legitimacy crisis in one reading (e.g., if coercion-validity is denied) propagates to sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_marriage_command__coercion_visibility_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
