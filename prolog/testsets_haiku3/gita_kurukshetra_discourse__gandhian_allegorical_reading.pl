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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Gandhian Allegorical Reading of Kurukshetra Discourse
 *   domain: religious/textual/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita is a contested kernel: a sacred text whose meaning is
 *   fought over by different interpretive communities. The Gandhian
 *   allegorical reading instantiates ONE constraint arising from that kernel.
 *   This reading interprets Kurukshetra as a metaphor for internal spiritual
 *   struggle, rejects caste-based dharma, elevates ahimsa (nonviolence) as
 *   supreme, and claims that interpretive authority rests with individual
 *   conscience rather than Brahminical institutional gatekeepers. The
 *   constraint emerges from this hermeneutical act: the reading establishes
 *   itself through textual exegesis, gains authority through alignment with
 *   nonviolent resistance movements, and competes with literal readings that
 *   ground caste hierarchy and righteous violence. The Gandhian reading is
 *   presented as ROPE (coordination around a shared interpretive framework
 *   that benefits nonviolent movements and caste-oppressed populations) while
 *   acknowledging that the orthodox scholars who bear the cost of losing
 *   institutional interpretive authority experience it differently. The claim
 *   and metrics are independent: claim rope, metrics describe low
 *   extractiveness with modest suppression requirement (the reading does not
 *   impose rents but does require defending the allegorical method against
 *   literalist challenge).
 *
 * KEY AGENTS:
 *   - Gandhian interpreters: propound and defend the allegorical reading, establish its authority through exegesis and political deployment
 *   - Brahminical orthodox scholars: maintain the literal reading, resist the allegorical reframing as heretical, invoke institutional authority and tradition
 *   - Caste-oppressed populations: experience the constraint as liberatory reframing (the reading enables their dignity claims) but are also trapped in the social structure the orthodox reading has long justified
 *   - Devotional universalists: offer an alternative reading (bhakti-centered, anti-caste but not explicitly antiviolent) that occupies different interpretive ground
 *   - Individual conscience interpreters: benefit from the reading's claim that authority vests in personal moral intuition rather than gatekeepers
 *   - Vedic literalists: excluded by the reading's foundational repudiation of literal scriptural hierarchy
 *   - Colonial administrators: external observers who encounter the reading through anticolonial mobilization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.18).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.42).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.31).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of Kurukshetra Discourse").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious/textual/ethical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '13151ae5-5492-4382-8382-6dca5177b2be').
narrative_ontology:cs_kernel_codification('13151ae5-5492-4382-8382-6dca5177b2be', fixed_text).
narrative_ontology:cs_authority_grounding('13151ae5-5492-4382-8382-6dca5177b2be', lineage).
narrative_ontology:cs_interpretation_layer_present('13151ae5-5492-4382-8382-6dca5177b2be').
narrative_ontology:cs_reading_relation('13151ae5-5492-4382-8382-6dca5177b2be', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('13151ae5-5492-4382-8382-6dca5177b2be', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('13151ae5-5492-4382-8382-6dca5177b2be', foundational, kurukshetra_metaphor_for_internal_struggle).
narrative_ontology:cs_axiom_status(kurukshetra_metaphor_for_internal_struggle, holdable).
narrative_ontology:cs_axiom_grounding('13151ae5-5492-4382-8382-6dca5177b2be', kurukshetra_metaphor_for_internal_struggle, deontological).
narrative_ontology:cs_axiom('13151ae5-5492-4382-8382-6dca5177b2be', foundational, ahimsa_supreme_dharma).
narrative_ontology:cs_axiom_status(ahimsa_supreme_dharma, holdable).
narrative_ontology:cs_axiom_grounding('13151ae5-5492-4382-8382-6dca5177b2be', ahimsa_supreme_dharma, deontological).
narrative_ontology:cs_axiom('13151ae5-5492-4382-8382-6dca5177b2be', secondary, individual_conscience_interpretive_authority).
narrative_ontology:cs_axiom_status(individual_conscience_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('13151ae5-5492-4382-8382-6dca5177b2be', individual_conscience_interpretive_authority, deontological).
narrative_ontology:cs_reference_frame('13151ae5-5492-4382-8382-6dca5177b2be', allegorical_internal_spiritual_struggle).
narrative_ontology:cs_drift_state('13151ae5-5492-4382-8382-6dca5177b2be', contemporary_religious_pluralism, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('13151ae5-5492-4382-8382-6dca5177b2be', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_conscience).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, nonviolent_resistance_movements).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_oppressed_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, devotional_universalists).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_interpreters).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_orthodox_scholars).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_oppressed_populations).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, internal_spiritual_struggle_primacy).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, conscience_based_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propound and defend the allegorical reading through exegesis, philosophical development, and political deployment in nonviolent movements. They establish the reading's authority by demonstrating its internal coherence, showing its applicability to contemporary moral crises, and mobilizing it for social change. They are not trapped in this interpretation; they can adopt or abandon it if ethical conviction shifts.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_interpreters, agenda_setter,
    organized, generational, mobile, global).

% Maintain and defend the literal reading as the authoritative interpretation. They invoke institutional authority grounded in Brahminical tradition, scriptural commentary chains, and educational gatekeeping. The Gandhian reading threatens their interpretive monopoly and the theological justification their position provides for caste hierarchy. Exiting the orthodox reading would require abandoning institutional identity and the authority it confers.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_orthodox_scholars, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_orthodox_scholars, agenda_setter).

% Experience the orthodox literal reading as theological justification for caste-based oppression and violence. They benefit from the Gandhian reading's explicit repudiation of caste hierarchy and elevation of ahimsa, which provides moral grounding for their dignity and resistance. However, they are also trapped: the reading offers textual and ethical reframing but does not automatically dismantle structural caste hierarchy. They cannot exit their structural position (caste is socially assigned, not chosen), though the reading may enable their resistance.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_oppressed_populations, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_oppressed_populations, payer).

% Propose an alternative reading centering bhakti (devotion) as path-independent and accessible to all regardless of caste. They reject caste-mandate readings but differ from Gandhians on whether the text authorizes any external violence. They occupy interpretive middle ground and can shift between readings or integrate multiple readings into their practice.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, devotional_universalists, beneficiary,
    moderate, biographical, mobile, global).

% Adopt the reading's claim that interpretive authority vests in individual moral conscience rather than institutional gatekeepers. They claim direct access to the text's meaning through personal moral intuition and lived experience. They benefit from the de-institutionalization of interpretation but face ongoing resistance from those who assert the necessity of trained exegesis. They can freely adopt or abandon this interpretive stance.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_interpreters, beneficiary,
    moderate, biographical, mobile, global).

% Argue that allegorical readings dilute the text's power and that its literal meaning about caste duty and righteous violence must be preserved. They are structurally excluded from the Gandhian reading's framework because that framework explicitly repudiates their foundational premise. No single commitment framework could coherently hold both the literal hierarchy mandate and the Gandhian metaphorical-internal-struggle interpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, vedic_literalist_movements, excluded,
    organized, generational, constrained, regional).

% Observe the constraint from outside the interpretive tradition. Initially dismissed the Gita entirely; later encountered it through the Gandhian reading during anticolonial mobilization. They become analytical observers tracking which readings enable resistance and which stabilize hierarchy. Their position is neither beneficiary nor payer but external analyst.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, colonial_administrators, observer,
    institutional, biographical, analytical, regional).

% Deploy the Gandhian reading as sacred textual authorization for ahimsa-based strategy. The reading provides them with a scriptural grounding for nonviolence that would otherwise rest only on philosophical or pragmatic arguments. They benefit from the reading's existence but could pursue nonviolent resistance without it (other ethical frameworks enable ahimsa). They have mobile exit: they can adopt alternative textual or secular grounds for their strategy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, nonviolent_resistance_movements, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared hermeneutical framework for interpreting a sacred text that has governed ethical action for centuries. The Gandhian allegorical reading solves the coordination problem of meaning-making: how can a text that has been read to justify caste and violence be read in a way that grounds nonviolent transformation and human equality? It offers a consistent method (allegory, internal struggle, ahimsa) that enables diverse interpreters to reach convergent conclusions about what the text teaches.
% TRANSFER_FUNCTION: Transfers interpretive authority from institutional Brahminical scholars to individual conscience and nonviolent resistance movements. Transfers moral legitimacy from caste-based hierarchy to egalitarian dignity. Transfers the text from justification of violence to prohibition of violence. The transfer is hermeneutical and political, not economic: authority over meaning, legitimacy in ethical claims, the text's deployment in public discourse.
% ABSENT_VOICES: Vedic literalist movements and caste-supremacist interpreters would object that the reading distorts the text's meaning and usurps their authority. Brahminical scholars trained in orthodox sampradaya would argue that the reading is heretical and that individual conscience cannot replace rigorous textual training. Some marginalized communities who understand the text through purely literal frameworks or non-Hindu ethical systems are absent from the Gandhian interpretive conversation. Colonial administrators and secular ethicists who view sacred texts with skepticism are also excluded by the reading's assumption that the Gita carries ethical authority.
% DISAPPEARANCE_RATIONALE: If the Gandhian allegorical reading disappeared, the dominant interpretation would likely revert to the orthodox literal reading, restoring theological justification for caste hierarchy and righteous violence. Nonviolent resistance movements would lose their primary textual grounding, though they would find other sources (secular ethics, other religious traditions, philosophical argument). Caste-oppressed populations would face renewed theological opposition to their dignity claims. However, the moral intuitions that ground the Gandhian reading (nonviolence, equality, individual conscience) would persist; they would simply find expression through other texts and frameworks. The Gita itself would remain contested and reinterpretable. The world would rearrange toward hierarchy, but moral resistance would not disappear.
% FOUNDING_PROBLEM: The Bhagavad Gita presents Arjuna facing a warrior-prince's moral crisis: must he fight a righteous war against kin, or renounce action? How can one act ethically in a morally ambiguous situation? The orthodox literal reading resolves this by saying caste duty (varnadharma) mandates the war and Krishna teaches acceptance of one's role. The Gandhian reading reframes the question entirely: the real problem is not external circumstances but internal moral clarity. Kurukshetra is the battlefield of the soul; Arjuna's struggle is with ego, attachment, and ignorance. The resolution is to act without ego-driven violence — to pursue the internal struggle rather than external warfare.
% FOUNDING_PROBLEM_CORROBORATION: Gandhi and his interpreters (Aurobindo, Vinoba Bhave, Sri Ananda Coomaraswamy, and modern scholars like Ashis Nandy) attest that the founding problem can be solved through allegorical reinterpretation. Independent scholars of comparative ethics, hermeneutics, and religious studies who are not invested in maintaining Brahminical authority (e.g., Western scholars of philosophy, postcolonial theorists) have analyzed the reading's internal coherence and its historical efficacy in grounding nonviolent resistance. However, orthodox Brahminical scholars and Hindu nationalist interpreters dispute this. They attest that the founding problem was already solved by the literal reading and that the Gandhian reading is a modern distortion. No neutral, position-independent corroboration exists; all corroboration comes from interpreters with stakes in the outcome (those committed to nonviolence and equality versus those committed to traditional hierarchy). The fact of corroboration from outside the Gandhian movement comes from secular and postcolonial scholars, not from within the orthodox Brahminical tradition.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.18 at interval end) because the Gandhian reading does not create a mechanism for collecting rents or resource transfer — it redistributes interpretive authority but not wealth. Over the 80-unit interval, extractiveness rises slightly (0.08 → 0.18) as some interpretive gatekeeping emerges among Gandhian scholars themselves (not everyone's conscience-based reading is equally heard; institutional authority reconstitutes itself at a different scale). Suppression declines from 0.55 to 0.42 because the reading's trajectory is toward greater acceptance and less need for active defense (modern education, postcolonial scholarship, global nonviolent movements have normalized the allegorical reading enough that suppression effort decreases). Theater ratio declines from 0.35 to 0.22: early in the reading's adoption, performative alignment (scholars invoking Gandhi without doing the exegetical work) was high; over time, the constraint becomes more genuinely hermeneutical and less performative. Accessibility collapse (0.31) is low because alternatives (literal reading, devotional reading, secular ethics) remain live and accessible to those who choose them — the Gandhian reading does not foreclose alternatives. Resistance (0.58) is substantial because the orthodox scholars and literalist movements actively oppose the reframing and the caste-hierarchy-supporting reading still commands institutional resources. The measurements on a unified time grid show the reading's gradual normalization over 80 years (roughly 1920–2000), with extractiveness rising slowly as gatekeeping re-emerges and suppression declining as the reading becomes institutionally established.
 *
 * PERSPECTIVAL GAP:
 *   The Brahminical scholars experience the reading as heretical usurpation that threatens their institutional authority and undermines the theological justification for caste hierarchy — from their position, low extractiveness is precisely the problem (the reading extracts no rents because it is not established enough to collect institutional power). The caste-oppressed populations experience the reading as liberatory (it provides moral grounding for their dignity claims and repudiates the caste-based dharma that has oppressed them). Gandhian interpreters experience it as truth-seeking and moral progress. The engine computes these per-seat divergences from the stakeholder power levels and exit options: institutional scholars have constrained exit (leaving the Brahminical tradition is costly), caste-oppressed are trapped (structural social assignment), Gandhians have mobile exit (they can adopt or drop the reading). The computed types will differ: the orthodox scholars may compute a snare-adjacent experience (they are defending an extractive hierarchy from reframing), while Gandhians compute rope (genuine coordination around nonviolence). The authored metrics describe the constraint structure as a whole; the engine's per-seat computation captures the perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual conscience (the abstract agent of personal moral authority) is beneficiary-coded: the reading elevates conscience as the source of interpretation, removing the gatekeeping function from institutions. Caste-oppressed populations are beneficiaries: the reading explicitly repudiates caste hierarchy and provides moral grounding for their resistance. Nonviolent resistance movements are beneficiaries: the reading gives them a sacred text authorization for ahimsa-based strategy. Brahminical orthodox scholars are payers: they lose institutional interpretive authority when the reading decentralizes meaning-making; their directionality is high (they bear the cost of lost gatekeeping power). The reading does not extract from them economically, but it redistributes power away from their institutional seat. The devotional universalists are neither clear beneficiaries nor payers — they coexist in parallel; their directionality is near 0.5 (symmetric, competitors not cooperators). The vedic literalists are excluded: the reading's foundational premise directly contradicts theirs (literal hierarchical mandate vs. metaphorical internal struggle). No override needed; the structural derivation from beneficiary/victim + exit captures the directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to interpret the Gita's moral crisis in a way that honors both sacred text and ethical principle) is LIVE for this reading. The reading does not face mandatrophy; it continues to be deployed for its original function: grounding nonviolent resistance and dignifying caste-oppressed populations. The threat to the reading's mandate comes not from obsolescence but from competing readings (the orthodox literal reading, the devotional reading) that offer different solutions to the same foundational problem. The Gandhian reading remains functional because nonviolent movements continue to exist and because the moral intuition (ahimsa, individual conscience, equality) that it instantiates remains culturally alive. If the founding problem were to die (if violence became universally accepted as legitimate, or if caste hierarchy were entirely abandoned), then the reading would face mandatrophy. But that is not the case. The constraint does not show signs of theatrical maintenance or extracted rents; it is a genuine coordination mechanism around a shared hermeneutical principle. Classification as rope is defensible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_interpretation_epistemology,
    'Is the allegorical reading a defensible hermeneutical method grounded in the text''s own complexity, or a retroactive imposition of modern nonviolent ethics onto an ancient warrior narrative?',
    'Textual analysis of the Gita''s own language (metaphorical density, self-referentiality, mystical teachings) compared against literal-reading textual claims; examination of earlier commentarial traditions to identify whether allegory was present before Gandhi or innovated by him.',
    'If allegory is textually grounded, the reading inherits authority from the text itself; if innovated by Gandhi, the reading''s authority rests on its ethical coherence and political efficacy, not on fidelity to the text. This changes how constraining the reading is (textually determined vs. ethically chosen).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_interpretation_epistemology, conceptual, 'Whether allegory is textually inherent or hermeneutically imposed.').

omega_variable(
    individual_conscience_authority_stability,
    'If interpretive authority vests in individual conscience rather than institutional gatekeepers, what prevents fragmentation into incompatible readings, and does the Gandhian reading itself become a new gatekeeper?',
    'Empirical observation of how the reading has actually been deployed: do nonviolent movements achieve consensus on interpretation, or do conscience-based readings splinter? Historical analysis of whether Gandhian interpreters have themselves gatekept the ''authentic'' allegorical reading.',
    'If the reading fragments, it loses coordination power (rope decays toward piton). If it re-gatekeeps, it recapitulates the institutional structure it repudiated (hidden extraction of interpretive authority). High fragmentation or re-gatekeeping would shift the type toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_conscience_authority_stability, empirical, 'Whether conscience-based authority is stable or re-institutionalizes.').

omega_variable(
    kernel_reading_vs_alternative_frameworks,
    'Is the Gandhian reading genuinely ABOUT the Gita, or is it a reframing that uses the Gita as a text through which to teach a separately-derived ethical system (nonviolence, equality)?',
    'Comparative textual analysis: extract the Gandhian reading''s core claims about what the Gita SAYS, and test whether an entirely secular teacher of nonviolence and equality would make different claims. If the claims are identical regardless of textual grounding, the reading is framework-external (the Gita is a vehicle, not the content).',
    'If the reading is framework-external, it is not constrained by the Gita''s actual meanings; it is constrained by Gandhian ethics using the Gita as legitimation. This would reclassify the constraint as snare (extraction of textual authority for external purposes) rather than rope (genuine coordination around interpretation). If the reading is framework-internal, the Gita genuinely constrains its meanings and it remains rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_vs_alternative_frameworks, conceptual, 'Whether the reading is about the Gita or uses the Gita for external ethical purposes.').

omega_variable(
    caste_oppressed_double_position,
    'Are caste-oppressed populations genuinely beneficiaries of the reading (it provides moral grounding for resistance), or are they also trapped in it (the reading offers only textual reinterpretation, not structural change in caste hierarchy)?',
    'Longitudinal empirical study of how caste-oppressed populations have deployed the Gandhian reading and whether it enabled structural change or only moral reframing. Analysis of whether the reading''s emphasis on allegorical internal struggle deflected attention from external structural change.',
    'If genuinely beneficiary, the reading is rope (serves their interests). If also trapping them in symbolic-only change while structure persists, it is snare or tangled rope (coordination story masks structural extraction). This would require re-authoring the victim set to include populations who adopted the reading but faced persistent caste violence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_oppressed_double_position, empirical, 'Whether the reading enables or constrains structural change for caste-oppressed populations.').

omega_variable(
    contested_kernel_reading_identity,
    'What makes the Gandhian reading a READING OF this kernel, as opposed to a separate ethical framework that simply invokes the Gita?',
    'Clarification of the reading''s own claim: does it assert ''the Gita actually teaches nonviolence when read allegorically'' (a claim about the text''s meaning) or ''we should interpret the Gita through nonviolence because it is ethically right'' (a claim about how to use the text)? The first is a reading; the second is an external framework.',
    'Affects how the constraint relates to the kernel: if reading, its ε is tied to the text''s actual referential content; if external, its ε is tied to how useful the text is for external purposes. This affects whether alternatives (other texts, secular ethics) would collapse the constraint or merely supplement it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_reading_identity, conceptual, 'Whether the reading instantiates a kernel interpretation or deploys the kernel for external purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(gita_tr_t0, projected).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(gita_tr_t10, observed).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(gita_tr_t20, observed).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(gita_tr_t40, observed).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement_basis(gita_tr_t60, observed).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement_basis(gita_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(gita_be_t0, projected).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement_basis(gita_be_t10, observed).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement_basis(gita_be_t20, observed).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement_basis(gita_be_t40, observed).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 60, 0.17).
narrative_ontology:measurement_basis(gita_be_t60, observed).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement_basis(gita_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(gita_su_t0, projected).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(gita_su_t10, observed).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(gita_su_t20, observed).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement_basis(gita_su_t40, observed).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement_basis(gita_su_t60, observed).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(gita_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% The Gandhian allegorical reading is one constraint instantiated from the contested kernel gita_kurukshetra_discourse. The orthodox literal reading and universalist devotional reading are sibling constraints with different ε values, different beneficiary/victim structures, and different authority-grounding claims. The three readings are NOT the same constraint viewed from different angles; they have different structural ε values because they make different claims about what the text says and what it permits. The kernel (the Gita itself) is contested precisely because no single reading captures its meaning to all parties. The three constraint files model this contest by making each reading its own constraint with its own stakeholder structure and its own cs_structure.reading_relations declarations. The network edges link them as competing instantiations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
