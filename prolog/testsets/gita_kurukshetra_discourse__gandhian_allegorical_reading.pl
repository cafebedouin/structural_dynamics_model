% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_gandhian_allegorical, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gandhian Allegorical Reading: Kurukshetra as Internal Spiritual Struggle
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Gita's Kurukshetra discourse is a contested kernel in Hindu textual
 *   tradition. The Gandhian allegorical reading interprets the battlefield as
 *   metaphor for internal spiritual struggle and repudiates physical violence
 *   as ethically defensible. It reframes Krishna's teaching as instruction in
 *   overcoming the violence of ego (ahamkara) and ignorance (avidya), not as
 *   justification for warfare. This reading shifts interpretive authority
 *   from Brahminical orthodox scholars to individual moral conscience and
 *   elevates ahimsa (non-harm) as the supreme ethical principle. It directly
 *   forecloses the orthodox literal reading's use of the text to legitimize
 *   caste hierarchy and righteous violence. It coexists with but differs from
 *   the universalist devotional reading, which emphasizes path-independent
 *   surrender (bhakti) over individual conscience and allegorical
 *   interpretation. The Gandhian reading is not a neutral hermeneutic
 *   technique but a politically charged reinterpretation that enables
 *   anti-caste and non-violence movements to claim the Gita's authority while
 *   rejecting the social hierarchies the orthodox reading defends.
 *
 * KEY AGENTS:
 *   - gandhian_moral_conscience_interpreters: organized scholars and activists who author and circulate the allegorical reading; have intellectual mobility and institutional capacity to publish and teach
 *   - brahminical_orthodox_scholars: institutional custodians of the orthodox literal reading; lose exclusive gate-keeping authority when the Gandhian reading circulates; their exit from the contest is constrained because their professional identity depends on defending orthodoxy
 *   - lower_caste_and_dalit_communities: powerless beneficiaries; the reading repudiates textual justification for their subordination but they have no direct authorial role in it
 *   - universal_ahimsa_advocates: organized beneficiaries who find in the reading a canonical textual foundation for non-violence
 *   - devotional_bhakti_tradition_followers: excluded because their reading privileges surrender to divine will over individual conscience and allegorical meaning-making
 *   - colonial_hindu_nationalism_interpreters: excluded because they may retain violence as an option for national defense while claiming allegorical framing
 *   - academic_comparative_religionists: analytical observers documenting the contest structure and material effects of competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.31).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.18).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading: Kurukshetra as Internal Spiritual Struggle").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'b86d2cfa-5832-46fa-b798-713ff823ab28').
narrative_ontology:cs_kernel_codification('b86d2cfa-5832-46fa-b798-713ff823ab28', fixed_text).
narrative_ontology:cs_authority_grounding('b86d2cfa-5832-46fa-b798-713ff823ab28', lineage).
narrative_ontology:cs_interpretation_layer_present('b86d2cfa-5832-46fa-b798-713ff823ab28').
narrative_ontology:cs_reading_relation('b86d2cfa-5832-46fa-b798-713ff823ab28', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('b86d2cfa-5832-46fa-b798-713ff823ab28', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('b86d2cfa-5832-46fa-b798-713ff823ab28', foundational, kurukshetra_battlefield_is_spiritual_metaphor).
narrative_ontology:cs_axiom_status(kurukshetra_battlefield_is_spiritual_metaphor, holdable).
narrative_ontology:cs_axiom_grounding('b86d2cfa-5832-46fa-b798-713ff823ab28', kurukshetra_battlefield_is_spiritual_metaphor, deontological).
narrative_ontology:cs_axiom('b86d2cfa-5832-46fa-b798-713ff823ab28', foundational, ahimsa_supreme_ethical_principle).
narrative_ontology:cs_axiom_status(ahimsa_supreme_ethical_principle, holdable).
narrative_ontology:cs_axiom_grounding('b86d2cfa-5832-46fa-b798-713ff823ab28', ahimsa_supreme_ethical_principle, deontological).
narrative_ontology:cs_axiom('b86d2cfa-5832-46fa-b798-713ff823ab28', foundational, caste_hierarchy_not_divinely_mandated).
narrative_ontology:cs_axiom_status(caste_hierarchy_not_divinely_mandated, holdable).
narrative_ontology:cs_axiom_grounding('b86d2cfa-5832-46fa-b798-713ff823ab28', caste_hierarchy_not_divinely_mandated, deontological).
narrative_ontology:cs_axiom('b86d2cfa-5832-46fa-b798-713ff823ab28', secondary, individual_moral_conscience_supreme_authority).
narrative_ontology:cs_axiom_status(individual_moral_conscience_supreme_authority, holdable).
narrative_ontology:cs_axiom_grounding('b86d2cfa-5832-46fa-b798-713ff823ab28', individual_moral_conscience_supreme_authority, deontological).
narrative_ontology:cs_reference_frame('b86d2cfa-5832-46fa-b798-713ff823ab28', gita_as_universal_ethical_teaching).
narrative_ontology:cs_drift_state('b86d2cfa-5832-46fa-b798-713ff823ab28', contemporary_hindu_intellectual_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b86d2cfa-5832-46fa-b798-713ff823ab28', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, moral_conscience_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_advocates).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, anti_caste_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, lower_caste_and_dalit_communities).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, universal_ahimsa_advocates).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_orthodox_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and activists who author and circulate the allegorical reading through publications, teaching, speeches, and political movements. They have capacity to publish in journals and books, teach in universities and movements, and mobilize practitioners around the reinterpretation. They do not depend on Brahminical institutional gatekeeping to author meaning; they can exit institutional constraints and reach audiences through independent movements (Gandhian non-cooperation, anti-caste organizing). Their situation is one of increasing institutional presence—they author the reading, defend it against criticism, and extend its implications to contemporary ethical problems.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_moral_conscience_interpreters, agenda_setter,
    organized, generational, mobile, national).

% Custodians of the orthodox literal reading within Sanskrit scholarship and Hindu religious hierarchy. They bear the cost of the Gandhian reading's circulation because it directly repudiates their interpretive authority and the social hierarchies (caste, justified violence) the literal reading validates. Their exit options are constrained because their institutional identity is constituted by defense of orthodoxy—leaving the orthodox position would dissolve their professional identity. They retain gate-keeping power in Sanskrit philology and Brahminical institutions but lose exclusive authority over the text's meaning when the Gandhian reading circulates widely.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_orthodox_scholars, payer,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_orthodox_scholars, excluded).

% Subject to caste hierarchy that the orthodox literal reading helps legitimize. They benefit from the Gandhian reading's repudiation of caste as divinely mandated, even though they do not author the reading or directly author its circulation. They are constrained in exit because their material position in caste society gives them no choice to abandon the text or its social consequences; the reading's circulation removes one class of textual justifications for their subordination. A directionality override (d=0.65) reflects their position as nominal beneficiaries but in a constrained exit context where even beneficiary status is structurally imposed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, lower_caste_and_dalit_communities, beneficiary,
    powerless, generational, constrained, continental).

% Peace movements, non-violence philosophers, and practitioners who gain from the Gandhian reading's framing of ahimsa as the Gita's supreme principle. The reading supplies textual authority within Hindu tradition for universal non-violence, which they can mobilize in their ethics and activism. They have exit options (they can frame non-violence on secular or other religious grounds) but benefit from Hindu canonical support. They coordinate around the reading because it enables them to claim Hindu tradition's authority for their ethical position.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, universal_ahimsa_advocates, beneficiary,
    organized, generational, mobile, global).

% Practitioners of the universalist devotional reading, which emphasizes path-independent surrender to Krishna (bhakti) accessible regardless of caste. They are excluded from the Gandhian reading's framing because that reading makes individual conscience and ahimsa supreme, while the devotional reading elevates surrender to divine will. They would object to the Gandhian reading's assertion that individual conscience is the ultimate authority; they argue divine surrender supersedes individual choice. They have constrained exit options because their religious identity is constituted through devotional practice within the Hindu tradition.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, devotional_bhakti_tradition_followers, excluded,
    moderate, generational, constrained, continental).

% Some nationalist rereadings of the Gita embrace the literal interpretation (righteous violence in defense of Hindu civilization) or use selective elements of allegory (internal struggle as metaphor for national defense) without committing to universal ahimsa or full repudiation of physical violence. They would object to the Gandhian reading's foreclosure of violence as an ethical option, even if they sometimes use allegorical language. They are excluded from the Gandhian reading's core because that reading forecloses violence, while nationalism may retain it as a possibility. They have mobile exit options (they can appeal to nationalist reading independently of any single interpretation).
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, colonial_hindu_nationalism_interpreters, excluded,
    organized, generational, mobile, national).

% Scholars analyzing the Gita's interpretations across traditions, documenting the three readings as live interpretive traditions with distinct empirical histories and institutional bases. They do not author or defend any reading but observe the structure of the contest, the textual strategies each uses, and the material effects of each reading on social organization and individual practice. They provide external corroboration of the founding problem's liveness (different communities genuinely stake ethical positions on the text's meaning).
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, academic_comparative_religionists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_moral_conscience_interpreters).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates anti-violence and anti-caste moral reasoning within Hindu textual tradition: practitioners seeking alignment between ahimsa (non-harm principle) and canonical authority find in the Gandhian reading a reinterpretation that permits both without rejecting the text. It solves the coordination problem 'how can I be faithful to Hindu tradition while rejecting caste hierarchy and committing to non-violence?' by offering a hermeneutic key that makes the text consistent with those commitments.
% TRANSFER_FUNCTION: Moves interpretive authority FROM Brahminical orthodox scholarship (institutional gatekeepers) TO individual moral conscience and universal ethical principles (accessible to all practitioners regardless of caste or institutional standing). The reading transfers the right to author meaning from credentialed Sanskrit scholars to any practitioner who can reason about the text's allegorical message. Material transfer is indirect: the reading does not extract wealth or labor directly, but it enables social movements (non-cooperation, civil disobedience) that redistribute institutional power and challenge caste-based resource allocation.
% ABSENT_VOICES: Orthodox Brahminical scholars who would defend the literal reading's coherence with contemporary caste practice; nationalist interpreters who might retain violence as an option while using selective allegorical framing; lower-caste practitioners who might resist the Gandhian framing as still-too-abstract or insufficiently material in addressing caste violence. The reading circulates primarily through organized intellectual and political movements (Gandhian non-violence, anti-caste activism) rather than through grassroots deliberation among all affected communities.
% DISAPPEARANCE_RATIONALE: If the Gandhian allegorical reading disappeared and the orthodox literal reading held exclusive authority, the Gita would remain a textual foundation for caste hierarchy and justified warfare; anti-caste movements and non-violence advocates would need to reject the text entirely rather than reinterpret it, shifting the authority structure and making caste hierarchy more vulnerable to challenge from outside Hindu tradition. The moral coordination it enables (fidelity to Hindu text + non-violence + anti-caste commitment) would dissolve; practitioners would face a renewed tension between textual authority and ethical conscience.
% FOUNDING_PROBLEM: The orthodox literal reading of the Gita was used to legitimize caste hierarchy as divinely mandated and to justify righteous violence in dharmic war. Anti-caste and non-violence advocates faced a dilemma: either reject Hindu tradition and textual authority to maintain ethical consistency, or remain within the tradition while accepting caste hierarchy and violence as divinely ordained. The Gandhian reading solves this by reframing the battlefield as metaphor for internal spiritual struggle, ahimsa as the supreme principle, and interpretive authority as individual conscience rather than Brahminical consensus.
% FOUNDING_PROBLEM_CORROBORATION: Hindu social reform movements, independence activists, and contemporary anti-caste scholars attest that caste hierarchy and violence justification remain live problems in the text and in society (documented in speeches by Gandhi, Ambedkar, and modern Dalit scholars). Non-violence advocates from outside Hindu tradition (peace philosophers, comparative ethicists) corroborate that the reframing enables moral consistency. Orthodox scholars and some nationalist interpreters attest that the literal reading remains authoritative in their communities, confirming the ongoing contest. Academic historians document the reading's emergence in late colonial and independence-era movements, confirming it solves a pressing historical problem.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).

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
 *   Extractiveness is low (0.31 at interval end) because the Gandhian reading does not concentrate material gains in a specific agent's pocket; it redistributes interpretive authority, not wealth directly. The measurement trajectory shows extractiveness rising from ~0.08 at t=0 (early circulation, low institutional reach) to ~0.31 by t=60, then stabilizing. This rise reflects the reading's growing institutional presence (universities, political movements, publishing) but also increasing defensive effort from orthodox scholars and institutional resistance from institutions invested in the literal reading. Suppression is low (0.18) and slightly declining over time because the reading does not depend on active coercion to persist; it relies on voluntary intellectual and political commitment. The reading's persistence does not require suppressing exits or alternatives—orthodox scholars remain free to defend the literal reading, and the two readings coexist in an active contestation. Theater ratio is very low (0.12) because the reading's function is not performative maintenance of an atrophied practice; it is live interpretive work that generates ongoing intellectual and political effects. Accessibility collapse is moderate (0.42) because the reading requires interpretive sophistication to understand the metaphor and its ethical implications; alternatives (literal reading, devotional reading) remain accessible and defended by powerful institutional actors. Resistance is high (0.71) because orthodox scholars, some nationalist interpreters, and defenders of caste hierarchy mount real intellectual and institutional resistance to the Gandhian reading's authority.
 *
 * PERSPECTIVAL GAP:
 *   The Gandhian allegorical interpreters and the Brahminical orthodox scholars compute from opposite directions. From the organizers' position, the reading enables moral coherence and serves justice by repudiating caste hierarchy and violence. From the orthodox institutional position, the reading is a hermeneutical violation that misreads the text and destabilizes their authority structure. Lower-caste communities benefit from the reading's existence even though they do not author it; they are targets of the orthodox reading's legitimation of hierarchy, so the Gandhian reading's circulation reduces one class of textual justifications for their subordination. The academic observer's position is distinct: they document the readings as live interpretive traditions with measurable institutional bases, textual strategies, and social effects, without adjudicating truth-value.
 *
 * DIRECTIONALITY LOGIC:
 *   The Gandhian interpreters are near the beneficiary end of directionality (d ~0.1–0.2) because they gain interpretive authority and capacity to mobilize movements around their reading, with relatively low suppression cost. Brahminical scholars are payers in the sense that they lose exclusive gate-keeping authority and face institutional erosion of their interpretive monopoly, but they retain institutional power in Sanskrit scholarship and religious hierarchy (d ~0.4–0.5, asymmetric because institutional power is not evenly distributed). Lower-caste communities have high d (~0.7–0.9) as targets of caste hierarchy, but they benefit from the reading's circulation even though they do not author it—this is precisely the asymmetry the tangled_rope (or rope with beneficiary-victim overlap) structure captures. Universal ahimsa advocates are beneficiaries (d ~0.15–0.25) because the reading provides textual authority for their ethical commitments. The reading's directionality emerges from the declared beneficiary/victim structure: ahimsa advocates and anti-caste movements are beneficiaries (they gain interpretive tools and authority for their positions), Brahminical scholars are payers (they lose exclusive authority), and the constraint itself is the reinterpretation that enables this redistribution.
 *
 * MANDATROPHY ANALYSIS:
 *   The Gandhian reading is not mandatrophy because it solves a live problem: the tension between fidelity to Hindu canonical authority and commitment to non-violence and anti-caste ethics. Its founding problem remains live in Hindu intellectual tradition and in the ongoing existence of caste hierarchy and violence justifications. The reading circulates because it enables practitioners to reconcile textual authority with moral conscience—a genuine coordination function. The claim/metric independence is maintained: claimed as rope (genuine coordination of anti-caste and non-violence moral reasoning) and measured as such (low extractiveness, low suppression, low theater ratio). The reading's authority does not rest on inertia or theatrical performance but on active intellectual work and political mobilization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_interpretation_vs_textual_fidelity,
    'Is the Gandhian allegorical reading a faithful interpretation of the Gita''s original meaning, or a modern reinterpretation that imposes contemporary ethical values onto an ancient text?',
    'Historical and philological analysis comparing the Gandhian reading to Sanskrit commentarial traditions, early textual scholarship, and attestations from contemporary practitioners about what the text ''meant'' to its original audiences and earliest interpreters.',
    'If the reading is faithful to original meaning (or to a live commentarial tradition), it gains authority within Hindu textual history and its ethics-based reframing is vindicated. If it is a modern imposition, the reading becomes a creative appropriation rather than a recovery of textual meaning—still politically and ethically valuable but with different status claims. The classification shifts from ''interpretation'' to ''reinterpretation as political intervention.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(allegorical_interpretation_vs_textual_fidelity, empirical, 'Whether the allegorical reading recovers original textual meaning or imposes modern ethics onto the text.').

omega_variable(
    individual_conscience_vs_textual_authority,
    'The reading shifts interpretive authority from Brahminical consensus to individual moral conscience. Is individual conscience a reliable basis for textual interpretation, or does it risk dissolving shared meaning into infinite subjectivity?',
    'Examine the actual variation in how individual practitioners using the Gandhian reading''s hermeneutic method arrive at ethical conclusions. If variance is wide, individual conscience may undermine coordination. If convergence is robust despite decentralized authority, conscience-based interpretation enables coordination without gate-keeping.',
    'High variance would suggest the reading''s claimed coordination function (enabling fidelity to text + anti-caste commitment + non-violence) may depend on implicit Gandhian-movement consensus rather than on the interpretive method itself. Low variance would support the reading''s claim that conscience-based interpretation is a reliable alternative to institutional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_conscience_vs_textual_authority, empirical, 'Whether individual moral conscience yields stable shared interpretation or dissolves into subjectivity.').

omega_variable(
    kernel_reading_vs_ordinary_interpretation,
    'Is the Gandhian reading one hermeneutic option among many within Hindu tradition, or does it represent a fundamental reformation of what the text means—a reading so transformative that it instantiates a different constraint from the orthodox literal reading?',
    'Compare the Gandhian reading''s structural reach: does it only reinterpret specific passages (local hermeneutic), or does it reframe the entire text''s meaning, ethical authority, and social implications (reading as systematic transformation)? Examine whether followers of the Gandhian reading can coherently also hold aspects of the orthodox literal reading, or whether the two are structurally incompatible.',
    'If local hermeneutic: the readings are variations within a single constraint structure, and the engine should model them as perspectives within the same story. If systematic transformation: they are genuinely different constraints with different ε values and beneficiary structures, and the two-file approach (separate stories linked via network) is structurally required. The current generation assumes the latter; the omega flags the ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_ordinary_interpretation, conceptual, 'Whether the Gandhian reading is a hermeneutic variant or a systematic constraint-level transformation.').

omega_variable(
    brahmininical_institutional_loss_vs_legitimate_authority_shift,
    'The reading displaces Brahminical scholars'' gate-keeping authority. Is this displacement a loss of legitimate expertise (Sanskrit philology, commentarial tradition), or is it the removal of an extractive institutional monopoly that had no legitimate basis for exclusive authority?',
    'Examine whether Brahminical interpretive expertise contributes essential value to understanding the text''s meaning and implications. Compare the accuracy and sophistication of Brahminical vs. Gandhian vs. other tradition commentaries. Assess whether the Brahminical monopoly on interpretation was defended on scholarly grounds or on caste-based institutional authority.',
    'If Brahminical expertise is legitimately specialized, the reading''s authority shift may sacrifice real knowledge for ideological consistency. If the monopoly was caste-enforced rather than merit-based, the shift is a democratic expansion. This resolves the question of whether the Gandhian reading is an extraction-removal (rope) or an extractive capture of authority (snare in disguise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahmininical_institutional_loss_vs_legitimate_authority_shift, empirical, 'Whether displaced Brahminical authority was legitimate expertise or extractive monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(gita_tr_t0, projected).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(gita_tr_t20, observed).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(gita_tr_t40, observed).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement_basis(gita_tr_t60, observed).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement_basis(gita_tr_t80, observed).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(gita_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(gita_be_t0, projected).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(gita_be_t20, observed).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement_basis(gita_be_t40, observed).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement_basis(gita_be_t60, observed).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 80, 0.31).
narrative_ontology:measurement_basis(gita_be_t80, observed).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(gita_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(gita_su_t0, projected).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(gita_su_t20, observed).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement_basis(gita_su_t40, observed).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 60, 0.18).
narrative_ontology:measurement_basis(gita_su_t60, observed).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement_basis(gita_su_t80, observed).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement_basis(gita_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_authority_gate).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_principle_elevation_across_traditions).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, anti_caste_textual_legitimation_removal).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel gita_kurukshetra_discourse. The kernel is the fixed text; readings diverge on interpretation (literal vs. allegorical), ethical conclusions (violence justified vs. repudiated), and legitimate authority (Brahminical scholars vs. individual conscience). Each reading is a separate constraint story with its own beneficiary/victim structure and type classification. The Gandhian allegorical reading forecloses the orthodox literal reading within Hindu ethical frameworks but coexists with the universalist devotional reading, which emphasizes devotion over ethics and does not directly address violence or caste. All readings are linked via network.affects_constraints to indicate shared kernel and mutual influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
