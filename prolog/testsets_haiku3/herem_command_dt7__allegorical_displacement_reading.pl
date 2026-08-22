% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem as Allegorical Internal Spiritual Warfare Against Vice
 *   domain: religious_ethics/hermeneutics
 *
 * SUMMARY:
 *   The allegorical displacement reading interprets the biblical herem
 *   command (Deuteronomy 7: destroy utterly the nations of Canaan) as an
 *   internal spiritual directive, not an ethnic-historical one. 'Nations'
 *   become typological placeholders for sin, temptation, and vicious
 *   impulses; 'conquest' becomes the practitioner's disciplinary struggle
 *   against vice. This reading resolves an acute ethical tension: the command
 *   appears to mandate ethnic destruction, which contradicts later moral
 *   teaching and universal human dignity. By relocating the referent entirely
 *   to the spiritual domain, the reading claims to render the command
 *   continuous with ethical monotheism. Extractiveness is low (0.18) because
 *   the framework distributes no material benefit to a beneficiary through
 *   coercion of victims — the 'victims' (vices) are abstractions, not human
 *   groups. Suppression is low (0.22) because the reading operates through
 *   interpretive authority and identity-locked commitment, not through force
 *   against external enemies. Theater is moderate-high (0.41) because the
 *   reading requires constant performative restatement of the allegorical
 *   meaning against the text's apparent literal sense — without that
 *   performance, readers revert to literal interpretation. The measurement
 *   series is stable across the interval: once the allegorical framework is
 *   adopted, extractiveness and suppression remain constant (the reading does
 *   not accumulate extraction over time; it maintains a steady low-extraction
 *   profile).
 *
 * KEY AGENTS:
 *   - righteous_practitioner_community: adopts the allegorical framework and constitutes their moral identity through the internalized discipline it frames
 *   - literal_ethnic_reading_proponents: interpret herem as ethnic-historical and are excluded by the allegorical move
 *   - contextual_supersession_proponents: treat herem as historically-bounded and superseded; coexist with but differ from allegorical reading
 *   - historical_critical_scholars: provide external evidence and challenge both literal and allegorical readings' claims about original sense
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.18).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.22).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Allegorical Internal Spiritual Warfare Against Vice").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "religious_ethics/hermeneutics").

domain_priors:requires_active_enforcement(herem_command_dt7__allegorical_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '62cfd70c-0d56-42b3-b65b-96af27657ac8').
narrative_ontology:cs_kernel_codification('62cfd70c-0d56-42b3-b65b-96af27657ac8', fixed_text).
narrative_ontology:cs_authority_grounding('62cfd70c-0d56-42b3-b65b-96af27657ac8', lineage).
narrative_ontology:cs_interpretation_layer_present('62cfd70c-0d56-42b3-b65b-96af27657ac8').
narrative_ontology:cs_reading_relation('62cfd70c-0d56-42b3-b65b-96af27657ac8', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('62cfd70c-0d56-42b3-b65b-96af27657ac8', herem_command_dt7__contextual_supersession_reading, influences).
narrative_ontology:cs_axiom('62cfd70c-0d56-42b3-b65b-96af27657ac8', foundational, herem_spiritual_typological_fulfillment).
narrative_ontology:cs_axiom_status(herem_spiritual_typological_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('62cfd70c-0d56-42b3-b65b-96af27657ac8', herem_spiritual_typological_fulfillment, deontological).
narrative_ontology:cs_axiom('62cfd70c-0d56-42b3-b65b-96af27657ac8', foundational, ethical_universalism_compatible_with_sacred_text).
narrative_ontology:cs_axiom_status(ethical_universalism_compatible_with_sacred_text, holdable).
narrative_ontology:cs_axiom_grounding('62cfd70c-0d56-42b3-b65b-96af27657ac8', ethical_universalism_compatible_with_sacred_text, deontological).
narrative_ontology:cs_reference_frame('62cfd70c-0d56-42b3-b65b-96af27657ac8', typological_spiritual_continuity).
narrative_ontology:cs_drift_state('62cfd70c-0d56-42b3-b65b-96af27657ac8', contemporary_moral_universalism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('62cfd70c-0d56-42b3-b65b-96af27657ac8', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, righteous_practitioner_community).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, typological_fulfillment_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__allegorical_displacement_reading, spiritual_internalization_hermeneutic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets herem as an internal disciplinary framework: the directive to 'destroy utterly' the nation's enemies is read as the practitioner's ongoing struggle against vice, temptation, and spiritual corruption within themselves and their community. Benefits from the reading by grounding moral self-discipline in sacred text and framing ethical struggle as divinely mandated warfare. The identity of a faithful practitioner is constituted through this struggle.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, righteous_practitioner_community, beneficiary,
    organized, generational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__allegorical_displacement_reading, righteous_practitioner_community, agenda_setter).

% Interpret the herem command as a historically-literal directive concerning actual ethnic boundaries and territorial conquest. They would argue the allegorical displacement misreads the text's original referent and dilutes its ethical gravity by relocating it to abstraction. Their voices are excluded from this reading's hermeneutical framework.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, literal_ethnic_reading_proponents, excluded,
    organized, generational, constrained, universal).

% Examine the textual, archaeological, and historical evidence for how herem was practiced and understood across different periods. They provide external corroboration or contestation of both the literal and allegorical readings' claims about the text's original sense and moral trajectory.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, historical_critical_scholars, observer,
    institutional, generational, analytical, global).

% Abstract vices (sin, temptation, spiritual corruption) are framed as the 'enemies' subject to herem. As non-agents, they do not negotiate or resist; they are the purported objects of the practitioner's disciplinary violence. The reading relocates all extractive force from ethnic relations to this internal struggle.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, sin_and_temptation_victims, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(herem_command_dt7__allegorical_displacement_reading, sin_and_temptation_victims).

% Argue that herem was a historically-bounded directive for Israel's settlement period, morally superseded by later prophetic teaching or Christian covenant. They are excluded from this reading's framework, which treats allegorical fulfillment as continuous, not superseded.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, contextual_supersession_proponents, excluded,
    organized, generational, constrained, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__allegorical_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__allegorical_displacement_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Frames internal moral discipline and the community's ethical struggle against vice as divinely mandated spiritual warfare, unifying practitioners around a common internal enemy (sin, temptation) and a shared disciplinary practice rooted in sacred text.
% TRANSFER_FUNCTION: Transfers interpretive authority from ethnic-historical referents to spiritual-typological referents: the meaning of 'destroy utterly' shifts from territorial conquest to the practitioner's destruction of vicious impulses within themselves. The 'nations' become internalized moral categories rather than external peoples.
% ABSENT_VOICES: Literal ethnic interpretation proponents and historical-critical scholars who would argue the text's original referent was concrete ethnic boundaries, not spiritual abstractions. They are excluded by the hermeneutical move itself — the reading constitutively relocates what 'nation' and 'destroy' mean.
% DISAPPEARANCE_RATIONALE: Proponents of the allegorical reading would argue that if this hermeneutical frame disappeared, the practitioner community loses a vital apparatus for integrating the command text into ethical practice — the text would either fall away as obsolete or revert to literal ethnic interpretation with implications its proponents reject. Critics would argue the disappearance of allegorical displacement would clarify that the text's referent was never abstract at all, forcing reckoning with its ethnic-historical content.
% FOUNDING_PROBLEM: How to render the herem command ethically intelligible and continuous with universal moral teaching in a framework where the command's literal ethnic sense is experienced as morally repugnant or superseded. The founding problem is not a practical coordination need but an interpretive crisis: the text seems to mandate ethnic destruction, which contradicts later moral development and universal human dignity claims.
% FOUNDING_PROBLEM_CORROBORATION: The interpretive tension is attested by multiple independent scholarly and theological traditions: Jewish allegorical interpreters (Philo, medieval Kabbalists), Christian typological readers (patristic and Reformation theologians), and modern religious ethicists all testify that the literal ethnic reading creates acute moral dissonance and requires hermeneutical displacement. The problem is live across traditions because the text remains authoritative and the ethical dissonance remains unresolved.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The allegorical reading's low extractiveness reflects its core hermeneutical move: by relocating the command entirely to the spiritual domain, it eliminates any human victim set that could bear the cost of extraction. Sin and temptation are not agents; they do not negotiate, resist, or suffer material loss. The suppression score (0.22) is low because the reading does not operate through coercion of external enemies — it operates through the internalized commitment of practitioners who identity-lock into the framework. The theater ratio (0.41) is elevated because the reading's credibility depends on constant performative restatement: the text must be read as allegorical, which requires interpretive labor and rhetorical insistence. Without that performance, the literal sense re-emerges. The measurement series is flat because the reading, once established in a tradition, maintains its character over time — there is no accumulation of extraction (the beneficiary is not collecting increasing rents) and no enforcement intensification (the constraint is stable as identity-locked commitment). Accessibility collapse is moderate (0.65) because once a practitioner adopts the allegorical framework, alternatives (literal reading, supersessionist reading) are not fully unavailable but are reframed as inferior readings — the collapse is identity-relative, not absolute. Resistance is moderate (0.58) because literal-interpretation proponents and historical-critical scholars actively contest the reading's validity, and the reading must continuously defend itself against the charge that it evacuates the text's ethical force.
 *
 * PERSPECTIVAL GAP:
 *   The righteous practitioner community (agenda-setter/beneficiary) experiences the constraint as a liberatory framework that integrates a morally repugnant text into ethical practice. Literal-ethnic reading proponents experience it as a hermeneutical evasion that denies the text's historical referent and drains it of force. Contextual supersession proponents experience it as an alternative to their own framework — both readings attempt to resolve the moral tension, but differently (supersession through historical transcendence, allegory through typological internalization). The engine should compute these perspectives differently: from the practitioner seat, the constraint is coordinating (unifying community around shared moral discipline); from the excluded literal seat, the constraint is suppressive (foreclosing their reading); from the observer seat (historical critics), the constraint is contestable (empirically vulnerable to textual evidence). The authored claim is rope (genuine coordination); the metrics describe low extraction and moderate theater, consistent with rope or scaffold framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is the righteous practitioner community, which derives identity and moral coherence from the allegorical framework. Their directionality (d) is low, near the beneficiary end, because they collect the benefit of ethical integration and reputational alignment with universal moral teaching. Literal-ethnic proponents are excluded by the hermeneutical move; they cannot operate within this reading's framework without accepting its typological premises. Historical-critical scholars are observers (analytical seat) — they neither collect nor pay, but their evidence can validate or undermine the reading's claim to textual fidelity. The abstract 'victims' (sin, temptation) are non-agents (agent: false) and do not participate in directionality computation. Because this reading has no human payer set and no coercive extraction mechanism against external groups, directionality does not require overrides — the beneficiary derivation alone captures the structural relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is acute: the text appears to mandate ethnic destruction, which contradicts the reading community's later moral commitments. This reading resolves the tension by claiming the command is not about ethnic destruction at all — it is about internal spiritual warfare. The resolution avoids mandatrophy (the founding problem is still live and addressed by the reading) but is fragile: it depends on the typological hermeneutic being accepted as a legitimate extension of meaning, not as an evacuation. If the hermeneutic is rejected as eisegesis, the founding problem remains unresolved, and the constraint becomes a theater-heavy cover story (extractiveness and suppression would rise, and type would shift toward snare). The measurements include an omega variable (typological_fulfillment_theological_validity) documenting this fragility. The reading claims to sustain the founding problem without succumbing to it; the measurement profile (stable low extraction, elevated theater) is consistent with that claim, but the omega notes that the claim is itself contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_identity,
    'Which kernel reading of the herem command is instantiated by the allegorical displacement interpretation, and what is the structural relationship to sibling readings?',
    'Textual analysis and hermeneutical genealogy: identify the interpreters and traditions that explicitly adopt allegorical displacement; classify their logical relationships to literal-ethnic and supersessionist readings across different confessional and scholarly traditions.',
    'This reading coexists with literal-ethnic readings (both are live in contemporary discourse) but logically forecloses the literal ethnic referent within its own framework — the ''nations'' cannot simultaneously be spiritual abstractions and ethnic peoples in a single reading. Influences supersessionist readings by offering an alternative path to ethical integration that does not require historical supersession.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'The identity and structural relations of this reading within the kernel contest over herem.').

omega_variable(
    typological_fulfillment_theological_validity,
    'Is the typological hermeneutic that treats herem''s ''nations'' as spiritual enemies a legitimate extension of the text''s meaning, or a displacement that evacuates the text''s ethical force?',
    'Hermeneutical and theological analysis: compare the allegorical reading''s justifications (textual typology, spiritual interpretation, continuity with liturgical practice) against charges that it amounts to eisegesis (reading-in rather than reading-out). Examine whether practitioners'' actual moral behavior is more aligned with allegorical understanding or with residual literal interpretation.',
    'If the displacement is judged a legitimate typological extension, the reading''s credibility as a coherent ethical framework is strengthened, and the low extractiveness score is justified. If judged as eisegesis, the theater ratio should rise (the apparent moral discipline masks unresolved ethnic content) and extractiveness should rise (the reading becomes a cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typological_fulfillment_theological_validity, conceptual, 'Whether the allegorical reading is a hermeneutical development or an evacuation of the text''s sense.').

omega_variable(
    internal_moral_struggle_externalization_risk,
    'Does the allegorical reading''s internalization of herem''s ''conquest'' successfully prevent its members from externalizing the framework onto actual ethnic or social enemies?',
    'Historical and sociological study: track whether communities adopting allegorical herem reading maintain strict internal-moral scope or whether the same language has been applied to justify real violence against groups coded as ''sinners'' or ''outsiders.'' Examine whether identity-locked commitment to the reading produces suppression of dissent within the community.',
    'If externalization is common, the suppression score should rise and extractiveness should rise — the reading would be misclassified as rope and should be reclassified as snare (using spiritual-internal framing as cover for exclusion/violence). If maintained as internal discipline, the reading''s low-extraction profile is valid.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_moral_struggle_externalization_risk, empirical, 'Whether the allegorical reading''s internal scope is stable or subject to re-externalization.').

omega_variable(
    textual_fulfillment_vs_supersession_boundary,
    'Where does the allegorical displacement reading locate itself relative to historical supersession? Does it treat the literal command as fulfilled (typologically completed) in Christ/community, or as transcended and obsolete?',
    'Textual analysis of how the reading''s proponents handle the command''s temporal status: is herem described as eternally operative in spiritual form, or as a past directive reinterpreted for new contexts? Compare against contextual supersession reading''s claim that herem is historically bounded and morally surpassed.',
    'If the reading treats fulfillment as eternal and operative, it claims continuity with both literal and supersessionist readings (forecloses neither). If it treats the literal command as obsolete though spiritually recapitulated, it influences the supersessionist reading but diverges from literal interpretation. The classification of relationships in cs_structure.reading_relations depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fulfillment_vs_supersession_boundary, conceptual, 'The temporal and eschatological status of herem in the allegorical reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t6, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 6, 0.39).
narrative_ontology:measurement_basis(here_tr_t6, observed).
narrative_ontology:measurement(here_tr_t12, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(here_tr_t12, observed).
narrative_ontology:measurement(here_tr_t18, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(here_tr_t18, observed).
narrative_ontology:measurement(here_tr_t24, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(here_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(here_be_t0, projected).
narrative_ontology:measurement(here_be_t6, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 6, 0.2).
narrative_ontology:measurement_basis(here_be_t6, projected).
narrative_ontology:measurement(here_be_t12, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement_basis(here_be_t12, projected).
narrative_ontology:measurement(here_be_t18, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 18, 0.18).
narrative_ontology:measurement_basis(here_be_t18, projected).
narrative_ontology:measurement(here_be_t24, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement_basis(here_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(here_su_t0, observed).
narrative_ontology:measurement(here_su_t6, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 6, 0.24).
narrative_ontology:measurement_basis(here_su_t6, observed).
narrative_ontology:measurement(here_su_t12, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement_basis(here_su_t12, observed).
narrative_ontology:measurement(here_su_t18, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 18, 0.22).
narrative_ontology:measurement_basis(here_su_t18, observed).
narrative_ontology:measurement(here_su_t24, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 24, 0.22).
narrative_ontology:measurement_basis(here_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__allegorical_displacement_reading, 0.12).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7__contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% The herem command kernel decomposes into three structurally distinct constraints, each instantiating a different reading. The allegorical-displacement reading relocates the constraint entirely to the spiritual domain, eliminating ethnic victim sets and human extraction mechanisms. The durable-separation reading treats herem as encoding timeless ethnic-identity preservation. The contextual-supersession reading treats herem as historically bounded and morally surpassed. These readings are linked via network.affects_constraints and share the same kernel_id; each has its own ε, victim/beneficiary structure, and classification. Sibling readings should be consulted via constraint-family queries (all readings of kernel_id = herem_command_dt7).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
