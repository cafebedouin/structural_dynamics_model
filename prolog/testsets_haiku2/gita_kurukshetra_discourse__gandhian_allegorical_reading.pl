% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gita Kurukshetra Discourse (Gandhian Allegorical Reading)
 *   domain: religious/philosophical/hermeneutical
 *
 * SUMMARY:
 *   The Gita Kurukshetra discourse, as interpreted through Gandhi's
 *   allegorical lens, presents the battlefield as a metaphor for the internal
 *   spiritual struggle between the forces of virtue (dharma) and vice
 *   (adharma) within the individual soul. This reading repudiates the text's
 *   apparent sanction for caste-based duty and righteous war by treating
 *   these literal elements as scaffolding for a deeper ethical teaching about
 *   non-violence (ahimsa) as the supreme principle. The reading transfers
 *   interpretive authority from the Brahminical scholarly class to the
 *   individual moral conscience, enabling modern reformers to claim Hindu
 *   textual tradition for anti-caste and non-violent movements. The reading
 *   is structurally a rope: it solves a genuine coordination problem (how to
 *   honor the Gita while rejecting caste and violence) with minimal
 *   extractive overhead and genuine benefit to the reader. The constraint's
 *   persistence depends on continued interpretive labor and institutional
 *   investment by the non-violence tradition, not on coercion—though the
 *   Brahminical scholarly establishment has incentives to suppress or
 *   delegitimize it.
 *
 * KEY AGENTS:
 *   - Individual moral conscience readers: gain hermeneutical freedom and direct access to the text's meaning without Brahminical mediation
 *   - Brahminical scholarly authority: loses monopoly on textual interpretation and institutional control over Hindu ethics
 *   - Victims of caste violence: vindicated by the reading's repudiation of caste hierarchy as divinely mandated
 *   - Gandhi and non-violence tradition: agenda-setter establishing the allegorical reading as the orthodox modern interpretation
 *   - Colonial observers: observe the contest; some appropriate the reading to justify liberal imperialism
 *   - Democratic mass movements: gain theological legitimation for non-violent resistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.18).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.12).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gita Kurukshetra Discourse (Gandhian Allegorical Reading)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious/philosophical/hermeneutical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'acc86f1c-bdd6-4add-849a-4039939a1577').
narrative_ontology:cs_kernel_codification('acc86f1c-bdd6-4add-849a-4039939a1577', fixed_text).
narrative_ontology:cs_authority_grounding('acc86f1c-bdd6-4add-849a-4039939a1577', lineage).
narrative_ontology:cs_interpretation_layer_present('acc86f1c-bdd6-4add-849a-4039939a1577').
narrative_ontology:cs_reading_relation('acc86f1c-bdd6-4add-849a-4039939a1577', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('acc86f1c-bdd6-4add-849a-4039939a1577', gita_kurukshetra_discourse__universalist_devotional_reading, influences).
narrative_ontology:cs_axiom('acc86f1c-bdd6-4add-849a-4039939a1577', foundational, ahimsa_supreme_principle).
narrative_ontology:cs_axiom_status(ahimsa_supreme_principle, holdable).
narrative_ontology:cs_axiom_grounding('acc86f1c-bdd6-4add-849a-4039939a1577', ahimsa_supreme_principle, deontological).
narrative_ontology:cs_axiom('acc86f1c-bdd6-4add-849a-4039939a1577', foundational, battlefield_metaphorical_internal_struggle).
narrative_ontology:cs_axiom_status(battlefield_metaphorical_internal_struggle, holdable).
narrative_ontology:cs_axiom_grounding('acc86f1c-bdd6-4add-849a-4039939a1577', battlefield_metaphorical_internal_struggle, conventional).
narrative_ontology:cs_axiom('acc86f1c-bdd6-4add-849a-4039939a1577', foundational, individual_conscience_interpretive_authority).
narrative_ontology:cs_axiom_status(individual_conscience_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('acc86f1c-bdd6-4add-849a-4039939a1577', individual_conscience_interpretive_authority, deontological).
narrative_ontology:cs_axiom('acc86f1c-bdd6-4add-849a-4039939a1577', foundational, caste_not_divinely_mandated).
narrative_ontology:cs_axiom_status(caste_not_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('acc86f1c-bdd6-4add-849a-4039939a1577', caste_not_divinely_mandated, deontological).
narrative_ontology:cs_reference_frame('acc86f1c-bdd6-4add-849a-4039939a1577', textual_meaning_accessible_to_conscience).
narrative_ontology:cs_drift_state('acc86f1c-bdd6-4add-849a-4039939a1577', contemporary_hindu_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('acc86f1c-bdd6-4add-849a-4039939a1577', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_conscience).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_reader).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, victims_of_caste_violence).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhi_and_nonviolence_tradition).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, democratic_mass_movements).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_scholarly_authority).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, advocates_of_literal_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains the hermeneutical freedom to read the Gita's violence as metaphorical rather than normative, and to interpret dharma as personal ethical responsibility rather than inherited caste duty. This reading licenses the reader to reject the text's apparent endorsement of violence and caste hierarchy without abandoning the text itself. Exit is available: the reader can adopt a different reading, but this reading presents the text as accessible to individual moral discernment rather than locked behind Brahminical authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_reader, beneficiary,
    moderate, biographical, mobile, universal).

% Loses exclusive interpretive authority over the Gita's meaning and application. The Gandhian reading contests the Brahminical claim that ritual interpretation of the text is the only legitimate path; it asserts that the text's true meaning is accessible to the individual conscience and that the orthodox reading's support for caste hierarchy is a historical accretion, not divine mandate. Exit is constrained by institutional and professional identity: abandoning interpretive authority would require ceding institutional standing.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_scholarly_authority, payer,
    institutional, generational, identity_locked, regional).

% Are vindicated by this reading's rejection of the Gita's apparent divine sanction for caste-based violence and ritualized hierarchy. The reading repudiates the orthodox interpretation that treats caste duty as dharmic and hence sacred. Their exit from caste-structured society is practically constrained, but the reading offers intellectual and moral grounds for resistance to structural violence by severing the text's theological endorsement of the system.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, victims_of_caste_violence, beneficiary,
    powerless, biographical, constrained, regional).

% Lose the Gita's apparent endorsement of righteous violence (dharmic war) as a legitimate expression of duty. Political and military actors who have cited the text to justify violence or conquest now face a reading that denies the text's literal validity for that purpose. Exit is constrained by prior institutional commitments to the literal reading's logic.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, advocates_of_literal_war, payer,
    powerful, biographical, constrained, regional).

% Sets the interpretive framework and gains legitimacy from the Gita's own text by demonstrating that the text's deepest meaning is consonant with ahimsa (non-violence). Gandhi's reading performs the institutional work of reclaiming Hindu textual authority for an ethical stance (non-violence) that is otherwise marginalized in orthodox Brahminical interpretation. The tradition has exit: it could abandon the Gita entirely and ground ahimsa in other textual or philosophical sources, but instead it invests in the allegorical reading to claim scriptural backing.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhi_and_nonviolence_tradition, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhi_and_nonviolence_tradition, beneficiary).

% Observe the contest between readings from a position of institutional distance. Colonial interpreters often read the Gita through the lens of civilization/savagery binaries and cited the literal reading to justify conquest and racial hierarchy. The Gandhian reading challenges that framing but also becomes subject to Orientalist appropriation (a text about Indian ethics becomes a vehicle for Western liberal values). Observers track how the reading is used politically and institutionally.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, colonial_and_orientalist_observers, observer,
    institutional, biographical, analytical, global).

% Gain a theological sanction for non-violent resistance to imperial and hierarchical authority. The Gandhian reading makes the Gita an intellectual and spiritual resource for mass mobilization around ahimsa and democratic equality. Exit is available: movements could ground resistance in other ideologies (secular nationalism, socialism, constitutional law), but the allegorical reading provides cultural-religious legitimation within Hindu traditions.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, democratic_mass_movements, beneficiary,
    organized, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhi_and_nonviolence_tradition).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical method for reading the Gita in a way that reconciles the text's apparent endorsement of violence and caste with the reader's moral intuition toward non-violence and equality. The reading coordinates the text's authority (it is scripture, not to be abandoned) with the reader's conscience (it cannot mean what the orthodox reading claims, because that would contradict the deeper ethical principles visible in other parts of the same text and in human moral experience).
% TRANSFER_FUNCTION: Transfers interpretive authority from the institutional Brahminical scholarly class to the individual moral conscience. What flows from the Brahminical interpreters to the Gandhian reader is the claim to legitimate textual meaning; what flows in return is the authority those interpreters lose to monopolize the text's application. The transfer is one of institutional power: the ability to say what the text means and what the text demands.
% ABSENT_VOICES: Scholars and communities who read the Gita as legitimating caste hierarchy and righteous war are structurally excluded from this reading's framing; they would object that the allegorical reading is a modern innovation that violates the text's plain meaning and that it is motivated by political convenience rather than textual fidelity. Dalit scholars and voices that experienced the Gita being used to justify violence against them are historically absent from the text's institutional interpretation (both orthodox and Gandhian), though this reading at least repudiates that use. Conservative Hindu establishments and nationalist movements that deployed the literal reading to justify violence are also excluded from the allegorical reading's epistemic community.
% DISAPPEARANCE_RATIONALE: If the Gandhian allegorical reading disappeared and only the orthodox literal reading remained unchallenged, Hindu intellectual and political traditions would lose a powerful framework for reconciling textual authority with non-violence and anti-caste commitments. Movements for democratic equality and non-violent resistance would lose a theological resource; the text would stand as a prima facie endorsement of caste and violence, and reformers would have to reject the text itself rather than reinterpret it. The institutional authority of the Brahminical scholarly class would be strengthened, and the text's legitimacy would be restored to those seeking divine sanction for violence or caste hierarchy. New readings might emerge to fill the gap, but the specific intellectual work the allegorical reading does to square the Gita with non-violence would need to be redone from other sources or abandoned.
% FOUNDING_PROBLEM: Early modern and modern Indian reformers faced a dilemma: the Gita is revered as scripture across Hindu traditions, but its literal reading appeared to endorse violence and caste-based duty in ways that conflicted with their commitment to non-violence and social equality. How could one remain faithful to Hindu textual tradition while rejecting its apparent ethical conclusions?
% FOUNDING_PROBLEM_CORROBORATION: Hindu scholars, philosophers, and activists outside the benefiting class of the Gandhian interpretation (e.g., B.R. Ambedkar, who rejected the allegorical reading as inadequate; contemporary Dalit scholars; textual historians; conservative Hindu establishments who defend the literal reading) confirm that this contradiction remains live in Hindu ethics and hermeneutics. The founding problem is not resolved; it is managed by the reading. Ambedkar's testimony, for instance, argues that the Gandhian allegorical reading is insufficient because it does not address the text's caste content directly and allows Brahminical interpreters to maintain their authority through a different framing. The persistence of competing readings across centuries of Hindu scholarship confirms the problem has not gone away.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).
:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.18 at interval end) because the reading does not concentrate benefits in any single institutional seat—it disperses hermeneutical power to the individual conscience and the non-violence tradition. Suppression is very low (0.12) because the reading persists through intellectual persuasion and institutional backing by Gandhi and later anti-caste movements, not through coercive enforcement of interpretation. Theater ratio is minimal (0.08) because the reading's function is real hermeneutical work, not performative maintenance. Accessibility collapse is low (0.25) because alternatives remain available—readers can adopt the orthodox literal reading, the universalist devotional reading, or reject the text entirely. Resistance is high (0.72) because the Brahminical scholarly establishment and those invested in the text's literal reading of dharmic war actively resist the allegorical interpretation. The measurement series shows a slight secular increase in extractiveness and suppression over the interval (t=0 to t=200), corresponding to the reading's gradual institutional entrenchment and the Brahminical response to contain and discredit it. The reading is relatively stable after t=150, when the Gandhian allegorical reading had become the dominant modern Hindu interpretation, though suppression efforts persist.
 *
 * PERSPECTIVAL GAP:
 *   The individual reader and the non-violence tradition experience the reading as liberatory and coordinating—it solves a genuine problem and benefits them structurally. The Brahminical scholarly establishment experiences it as an institutional threat: their authority to monopolize textual meaning is eroded, and their interpretation of dharma is challenged. The colonial observer may appropriate the allegorical reading to justify liberal imperialism ('India is learning non-violence and democracy from us'), which would constitute a different extractive use of the reading. The victims of caste violence gain intellectual grounds for resistance but do not gain material exit from caste structures—the reading's benefit to them is ideological, not material. The measurement of suppression_requirement captures the institutional resistance of the Brahminical class and conservative Hindu establishments against the spread of the allegorical reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The individual conscience reader is a beneficiary (d ≈ 0.2): gains interpretive freedom and hermeneutical access without bearing extractive costs. The Brahminical scholarly authority is a payer (d ≈ 0.8): loses interpretive monopoly and institutional control; their exit is identity-locked (institutional and professional identity fused with textual authority). The non-violence tradition is the agenda-setter and beneficiary: it establishes the reading's framework and benefits from it structurally. Democratic mass movements are beneficiaries (d ≈ 0.3): gain theological resource for non-violent resistance without bearing extractive costs. Victims of caste violence are beneficiaries (d ≈ 0.2): the reading vindicates their resistance to structural violence, though their practical exit from caste remains constrained. The colonial observer is neither beneficiary nor payer from the constraint itself—they may appropriate it to their own ends, but the reading's directionality does not include them structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy (the constraint's founding purpose persists and justifies its operation). The founding problem—reconciling the Gita's authority with non-violence and equality—remains live. The reading's function is not atrophied; interpretive labor is continuous, and the reading continues to coordinate the ethical intuitions of Hindu reformers with the textual tradition. Suppression requirements increase slightly over time, indicating that the constraint must be actively maintained against Brahminical resistance, but this is consistent with an ongoing coordination challenge, not a degraded function. The reading does not exhibit the characteristics of a piton (atrophied function, pure performance, theater_ratio >> 0.5). The mandatrophy issue is whether the reading is a modern imposition on the text or a legitimate recovery of the text's deeper meaning—but that is a conceptual/hermeneutical question, not a structural one about the constraint's function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_vs_literal_hermeneutics,
    'Is the allegorical reading a legitimate historical interpretation of the Gita, or a modern interpolation motivated by political convenience and colonial-era reformation?',
    'Textual-historical analysis: examine pre-modern commentaries on the Gita to determine whether allegorical interpretation of the battlefield has precedent in the tradition''s own history, or whether it emerges only with Gandhi and modern reformers.',
    'If the allegorical reading has deep textual precedent, it is a legitimate competing interpretation within the tradition; if it is a modern innovation, the reading''s claim to recover the text''s ''true meaning'' is undermined, though its ethical conclusions might still be sound. The type classification does not change, but the constraint''s legitimacy narrative is challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_vs_literal_hermeneutics, empirical, 'Historical originality of the allegorical reading within Hindu exegetical tradition').

omega_variable(
    brahminical_authority_identity_lock,
    'To what extent is the Brahminical scholarly class identity-locked into the orthodox literal reading, versus able to shift to the allegorical reading without institutional cost?',
    'Empirical observation of which scholars and institutions adopt the allegorical reading and what institutional consequences follow; whether Brahminical authorities can migrate to the new reading without ceding their interpretive authority or losing standing.',
    'If the identity-lock is severe (scholars cannot migrate without professional cost), the suppression requirement stays high and the reading remains a genuine institutional threat. If the identity-lock is weak (Brahminical authorities can adopt the allegorical reading and maintain their institutional position), suppression requirements should decline as institutional consensus shifts, and the reading transitions from contested to dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(brahminical_authority_identity_lock, empirical, 'Degree of institutional identity-lock among Brahminical scholars regarding the orthodox interpretation').

omega_variable(
    non_violence_tradition_motivations,
    'Does the non-violence tradition''s investment in the allegorical reading rest on a genuine conviction that the text can bear this interpretation, or is the reading a convenient vehicle for legitimating non-violence within Hindu tradition regardless of textual fidelity?',
    'Analysis of Gandhi''s own hermeneutical reasoning and later scholars'' defenses of the allegorical reading: do they argue for textual fidelity or for ethical priority? Do they acknowledge the text''s literal difficulties or attempt to resolve them?',
    'If the tradition is aware that it is imposing meaning on the text rather than recovering it, the reading''s extractiveness might increase (as it becomes more performative and less genuinely coordinating). If the tradition genuinely believes the text bears this interpretation, the extraction remains low and the coordination function is real. Either way, the reading''s structural function does not change, but the characterization of its epistemic status does.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_violence_tradition_motivations, conceptual, 'Epistemic status of the allegorical reading within the non-violence tradition''s own understanding').

omega_variable(
    kernel_vs_reading_distinction,
    'Which of the three readings (literal, universalist, allegorical) best instantiates the Gita''s own core claim about what the text is for?',
    'Detailed exegesis comparing how each reading handles the text''s most explicit and repeated claims (e.g., Krishna''s arguments to Arjuna, the text''s own framing of duty, the role of knowledge and action). No single reading may handle all claims equally well.',
    'This question is conceptual/hermeneutical and does not change the structural classification, but it bears on the reading''s long-term institutional stability and the likelihood of future readings emerging. If no reading handles all the text''s claims coherently, the contest among readings may be permanent rather than tending toward consensus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_distinction, conceptual, 'Textual integrity test: which reading best honors the text''s own internal structure and explicit claims').

omega_variable(
    brahminical_reading_as_mask,
    'To what extent does the orthodox literal reading mask Brahminical interests (preservation of caste hierarchy and institutional control) under the guise of textual fidelity?',
    'Genealogical analysis: trace how the literal reading became ''orthodox'' (who established it as such, when, under what institutional pressures); examine whether the reading was always stated the same way or evolved to justify institutional interests over time.',
    'If the literal reading is itself a modern invention designed to preserve Brahminical authority against reform, the distinction between ''true textual meaning'' and ''imposed reading'' becomes ambiguous for all three readings. The reading landscape becomes contested at a deeper level: the question is not which reading is correct, but who has the power to establish what counts as correct. This does not change the structural classification of the Gandhian reading as a rope, but it reframes the suppression analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_reading_as_mask, empirical, 'Historical genealogy of the ''orthodox literal reading'' as an institutional construction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(gita_tr_t25, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 100, 0.07).
narrative_ontology:measurement(gita_tr_t150, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 150, 0.08).
narrative_ontology:measurement(gita_tr_t200, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 200, 0.08).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(gita_be_t25, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 100, 0.16).
narrative_ontology:measurement(gita_be_t150, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 150, 0.17).
narrative_ontology:measurement(gita_be_t200, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 200, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(gita_su_t25, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 25, 0.09).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 100, 0.11).
narrative_ontology:measurement(gita_su_t150, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 150, 0.12).
narrative_ontology:measurement(gita_su_t200, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 200, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_authority_institutional_monopoly).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_hierarchy_theological_justification).

% DUAL FORMULATION NOTE:
% This constraint is part of the Gita Kurukshetra kernel family. The kernel is the text itself; the Gandhian allegorical reading is one of three major readings that instantiate distinct constraints from the same textual foundation. The other readings (orthodox_literal_reading, universalist_devotional_reading) are separate constraint stories with different beneficiaries, different extractiveness profiles, and different victim sets. The allegorical reading influences the orthodox literal reading by contesting its authority and creating institutional pressure to justify the literal reading on textual rather than purely traditional grounds. The allegorical reading influences the universalist devotional reading by both sharing the repudiation of caste hierarchy and competing for interpretive dominance within reformist Hindu traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
