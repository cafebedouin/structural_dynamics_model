% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Gandhian Allegorical Reading of the Kurukshetra Discourse (Bhagavad Gita)
 *   domain: religious/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This story instantiates the Gandhian allegorical reading of the Bhagavad
 *   Gita's Kurukshetra discourse: Arjuna's battlefield dilemma is read as an
 *   allegory for the individual moral conscience's struggle against inner
 *   forces of attachment, fear, and violence, and Krishna's counsel to
 *   'fight' is read as counsel to struggle spiritually, not to kill
 *   physically. This reading emerged with particular force in the
 *   early-to-mid twentieth century as the ethical engine of Gandhi's
 *   satyagraha movement and remains a live devotional and political reading
 *   today. It is one of three sibling readings of the same kernel text — the
 *   orthodox literal reading (which holds the battlefield is a real war and
 *   caste-based duty is divinely mandated) and the universalist devotional
 *   reading (which reads dharma as surrender to divine will independent of
 *   caste or allegory) are separate constraints, not alternate framings
 *   folded into this one. This story's epsilon is authored strictly for the
 *   allegorical reading's own operation as a contested interpretive
 *   arrangement — the displacement of orthodox interpretive authority and the
 *   withdrawal of scriptural warrant from those who relied on the literal
 *   reading — not for the nonviolent ethic it endorses, which would trivially
 *   read as near-zero extraction.
 *
 * KEY AGENTS:
 *   - satyagraha_movement_leadership: primary agenda_setter and beneficiary (organized/mobile) — derives mass movement legitimacy from the reading
 *   - orthodox_brahminical_commentators: primary payer (institutional/constrained) — loses exclusive interpretive gatekeeping
 *   - readers_seeking_scriptural_warrant_for_martial_duty: secondary payer (powerless/constrained) — loses scriptural anchor for occupational identity
 *   - textual_hermeneutics_scholars: analytical observer (analytical/analytical) — documents the historical contest without adjudicating it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.58).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.42).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of the Kurukshetra Discourse (Bhagavad Gita)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '4c18fefe-574a-460b-a7e3-60dae2643026').
narrative_ontology:cs_kernel_codification('4c18fefe-574a-460b-a7e3-60dae2643026', fixed_text).
narrative_ontology:cs_authority_grounding('4c18fefe-574a-460b-a7e3-60dae2643026', practice).
narrative_ontology:cs_interpretation_layer_present('4c18fefe-574a-460b-a7e3-60dae2643026').
narrative_ontology:cs_reading_relation('4c18fefe-574a-460b-a7e3-60dae2643026', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('4c18fefe-574a-460b-a7e3-60dae2643026', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('4c18fefe-574a-460b-a7e3-60dae2643026', foundational, violence_in_text_is_exclusively_figurative_inner_struggle).
narrative_ontology:cs_axiom_status(violence_in_text_is_exclusively_figurative_inner_struggle, holdable).
narrative_ontology:cs_axiom_grounding('4c18fefe-574a-460b-a7e3-60dae2643026', violence_in_text_is_exclusively_figurative_inner_struggle, conventional).
narrative_ontology:cs_axiom('4c18fefe-574a-460b-a7e3-60dae2643026', foundational, individual_conscience_supersedes_lineage_authority_as_interpretive_ground).
narrative_ontology:cs_axiom_status(individual_conscience_supersedes_lineage_authority_as_interpretive_ground, holdable).
narrative_ontology:cs_axiom_grounding('4c18fefe-574a-460b-a7e3-60dae2643026', individual_conscience_supersedes_lineage_authority_as_interpretive_ground, deontological).
narrative_ontology:cs_axiom('4c18fefe-574a-460b-a7e3-60dae2643026', secondary, caste_duty_language_is_not_divine_mandate).
narrative_ontology:cs_axiom_status(caste_duty_language_is_not_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('4c18fefe-574a-460b-a7e3-60dae2643026', caste_duty_language_is_not_divine_mandate, deontological).
narrative_ontology:cs_reference_frame('4c18fefe-574a-460b-a7e3-60dae2643026', vedantic_lineage_commentary_tradition).
narrative_ontology:cs_drift_state('4c18fefe-574a-460b-a7e3-60dae2643026', nationalist_independence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c18fefe-574a-460b-a7e3-60dae2643026', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, satyagraha_movement_leadership).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, reformist_hindu_laity).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, anti_caste_ethical_reformers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_brahminical_commentators).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_temple_authorities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, readers_seeking_scriptural_warrant_for_martial_duty).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, internal_moral_struggle_as_true_referent_of_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the Kurukshetra battlefield as the field of the human heart (kurukshetra dharmakshetra as inner terrain) and Arjuna's dilemma as the individual conscience confronting attachment, fear, and violence within itself. Uses this reading to derive a doctrine of nonviolent resistance and moral self-discipline, and actively promotes it as the correct key to the whole text in political and pedagogical settings. Gains legitimacy and mass moral authority for a nonviolent independence movement by claiming this reading is what the text has always meant.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, satyagraha_movement_leadership, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, satyagraha_movement_leadership, beneficiary).

% Adopts the allegorical frame to practice the Gita devotionally without endorsing caste duty or literal violence; gains a scripture that can be read as personal ethical instruction rather than as a manual for war or social hierarchy. Can freely choose among available readings depending on community and teacher.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, reformist_hindu_laity, beneficiary,
    moderate, biographical, mobile, regional).

% Uses the allegorical reading's removal of literal caste mandate to argue the text does not sanction the varna system as divinely fixed duty. Benefits directly from the reading's structural delta but remains constrained by the continuing social weight of orthodox interpretation in many communities and institutions.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, anti_caste_ethical_reformers, beneficiary,
    moderate, generational, constrained, national).

% Has historically held interpretive authority over the text through lineage-transmitted commentary (Shankara, Ramanuja, and subsequent acharya traditions) grounding svadharma in literal caste-based duty and legitimating righteous war. The allegorical reading displaces this authority onto individual conscience, eroding the commentators' traditional gatekeeping role and their institutional relevance as the exclusive adjudicators of the text's meaning. Cannot simply exit the interpretive contest without losing the basis of their institutional standing.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_brahminical_commentators, payer,
    institutional, civilizational, constrained, national).

% Administers ritual and pedagogical transmission of the text within temple and matha structures premised on the historicity of the battlefield event and the divine sanction of Krishna's counsel to fight. The allegorical reading undercuts the literal historicity claim that underwrites some of their ritual and calendrical practices, though it does not eliminate their institutional base entirely.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_temple_authorities, payer,
    institutional, generational, constrained, regional).

% Historically drew on the literal reading of dharmic war to justify participation in caste-linked martial or occupational duty as spiritually mandated. Under the allegorical reading, this warrant is withdrawn — their occupational identity loses its scriptural anchor, and they must locate ethical justification elsewhere, without the institutional resources to contest the reinterpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, readers_seeking_scriptural_warrant_for_martial_duty, payer,
    powerless, biographical, constrained, national).

% Neither authors nor is bound by the interpretive contest, but has structural interest in which reading prevails: the allegorical reading, deployed as the moral engine of satyagraha, directly threatens colonial administrative legitimacy while a literal-warrior reading would pose no such threat. Excluded from the hermeneutic conversation itself even though the outcome materially affects state authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, colonial_and_postcolonial_state_authorities, excluded,
    institutional, biographical, analytical, national).

% Analyzes the historical layering of the text, the plausibility of allegorical versus literal readings given the Mahabharata's narrative frame, and the political circumstances under which each reading gained prominence. Does not adjudicate which reading is theologically correct but documents how interpretive authority has shifted historically.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, textual_hermeneutics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared ethical vocabulary that lets a mass, religiously diverse movement coordinate around nonviolent resistance by locating the text's authority in individual conscience rather than in caste-differentiated duty — solving the real problem of how to mobilize devout Hindus toward nonviolence without repudiating their scripture.
% TRANSFER_FUNCTION: Moves interpretive authority and the moral capital attached to it from hereditary Brahminical commentators and literalist temple institutions to lay reformers, political leadership, and the individual reader's conscience; moves scriptural warrant for martial caste duty away from those who relied on it.
% ABSENT_VOICES: Practicing warriors and martial-caste communities whose occupational and spiritual identity depended on the literal reading are not consulted in the reinterpretation and have no seat in the hermeneutic contest; orthodox commentators experience the shift as an imposed loss of authority rather than a debate they were invited to win or lose.
% DISAPPEARANCE_RATIONALE: If the allegorical reading disappeared as a live interpretive option, the ethical grounding many nonviolent-resistance movements drew from the Gita would lose its scriptural anchor, forcing either a return to literalist readings that sanction righteous violence or a search for entirely non-Gita ethical warrants — the moral architecture of movements built on 'ahimsa is the true teaching of the Gita' would need reconstruction.
% FOUNDING_PROBLEM: How can a scripture whose plain narrative frame is a call to righteous war, addressed to a warrior on a literal battlefield, be reconciled with a commitment to universal nonviolence and the rejection of caste-mandated duty as divinely fixed?
% FOUNDING_PROBLEM_CORROBORATION: Gandhi and his interpretive successors attest the problem is solved by recognizing the battlefield as always-already allegorical. Independent historians of religion and philologists outside both the Gandhian and orthodox camps attest that the text's own internal evidence (explicit caste-duty language, the literal narrative frame within the Mahabharata) makes the allegorical reading a deliberate ethical reconstruction rather than a recovery of original intent — corroboration exists but does not resolve in the allegorical reading's favor as a matter of textual history.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the reading's function as a genuine redistribution of interpretive and moral authority: it is not merely additive (a new reading alongside the old) but actively displaces the orthodox commentators' claim to be the sole legitimate voice on the text's meaning, and it withdraws scriptural warrant from those whose martial or caste-linked identity depended on the literal frame. Suppression (0.42) is moderate rather than low because the reading has been actively promoted and taught as authoritative — pedagogically, politically, and through Gandhi's own widely disseminated commentary (Anasaktiyoga) — rather than merely offered as one option among equals; this is real interpretive pressure, though it falls well short of coercive enforcement. Theater ratio (0.30) captures that some of the reading's public deployment (particularly in nationalist political rhetoric) performs moral seriousness that outstrips close textual engagement with the Sanskrit. Accessibility collapse (0.35) is moderate-low: rival readings remain fully available and widely practiced — this reading has never achieved anything like monopoly. Resistance (0.55) is substantial: orthodox commentators and traditionalist institutions have actively contested the allegorical reading in print and pedagogy since its rise to prominence.
 *
 * PERSPECTIVAL GAP:
 *   From the satyagraha leadership's seat, the allegorical reading is simply correct scriptural interpretation, recovering the text's true ethical core from centuries of literalist distortion — no extraction is visible from this seat. From the orthodox commentators' seat, the same reading is an imposition that discards fourteen centuries of grounded commentary tradition in favor of a modern ethical agenda read backward into the text. The engine computes these as different seat-classifications from the same structural data; neither seat's self-perception settles which is descriptively accurate.
 *
 * DIRECTIONALITY LOGIC:
 *   Satyagraha leadership and reformist beneficiaries sit near the beneficiary end of directionality: they derive mobilizational and moral capital directly from the reading's ascendance and hold organized power with mobile exit (they can draw on multiple textual and philosophical traditions). Orthodox commentators and literalist temple authorities sit near the target end: institutional power does not translate into exit here because their authority is specifically constituted by the literal reading's correctness — abandoning the contest concedes the very thing that makes them authoritative. Readers seeking martial scriptural warrant are the most fully targeted: powerless, constrained, and their loss (withdrawal of scriptural warrant) is not offset by any compensating gain within this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling scripture with nonviolent ethics — remains genuinely contested rather than dead or purely solved: philological evidence supports reading the caste-duty language as sincerely meant rather than allegorical from the outset, yet the moral-conscience reading has independently demonstrated real coordinative power (mobilizing mass nonviolent resistance) regardless of its historical-textual pedigree. Classifying this as tangled_rope rather than snare or rope avoids two errors: treating the reading as pure extraction (which would ignore its genuine, historically demonstrated coordination function for nonviolent ethics) and treating it as costless coordination (which would ignore that its ascendance required and still requires actively displacing a rival interpretive authority with real institutional stakeholders who bear a real cost).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_reading_originalism_vs_reconstruction,
    'Is the allegorical reading a recovery of a meaning latent in the text from composition, or a modern ethical reconstruction retrojected onto a text whose plain sense is martial and caste-affirming?',
    'Philological and historical-critical analysis of the Bhagavad Gita''s composition layers within the Mahabharata, comparison with contemporaneous dharma literature''s treatment of caste duty, and textual-critical assessment of whether the battlefield frame narrative is separable from the philosophical dialogue without violence to the text''s structure.',
    'If the allegorical reading is shown to be a modern reconstruction rather than a recovered original sense, its interpretive authority claim weakens relative to lineage-based orthodox commentary, which would affect how the displacement of orthodox authority (the extraction this story measures) should be morally evaluated even though it would not change the structural fact of displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegorical_reading_originalism_vs_reconstruction, empirical, 'Whether the allegorical reading recovers original textual meaning or reconstructs it for modern ethical purposes.').

omega_variable(
    committer_kernel_disagreement_locus,
    'Where exactly does the gandhian_allegorical_reading structurally diverge from its sibling readings, and is the divergence located primarily in the ontological status of the battlefield (literal vs. figurative), the locus of interpretive authority (lineage vs. conscience), or the status of caste (mandate vs. dissolved constraint)?',
    'Comparative analysis of the three sibling constraint stories'' beneficiary/victim structures and axiom sets to identify whether the readings differ primarily on one axis or are jointly entangled across all three.',
    'If the divergence is primarily about caste-mandate status, this reading''s victim set (orthodox commentators, martial-duty readers) is the most consequential structural marker; if primarily about the battlefield''s literal status, the universalist_devotional_reading may share more structural ground with this reading than with the orthodox_literal_reading, altering the reading_relations classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_locus, conceptual, 'Locating the precise structural axis of disagreement among the three sibling readings of the kernel.').

omega_variable(
    gandhi_own_reading_stability,
    'Did Gandhi''s own allegorical reading remain stable across his life and writings (Anasaktiyoga, Young India commentary, later speeches), or did it shift in ways that make ''the Gandhian allegorical reading'' itself an idealized composite rather than a single stable interpretive position?',
    'Close textual comparison of Gandhi''s Gita commentary across different periods of his public life and correspondence.',
    'If Gandhi''s own reading shifted substantially, this story''s claimed_type and metrics describe a somewhat idealized stabilization of a real but more variable historical phenomenon, which would suggest decomposing this story further into period-specific sub-readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gandhi_own_reading_stability, empirical, 'Whether the named reading corresponds to one stable historical position or an idealized composite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 1900, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(gita_tr_t1917, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(gita_tr_t1934, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(gita_tr_t1950, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1950, 0.24).
narrative_ontology:measurement(gita_tr_t1970, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1970, 0.27).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 2000, 0.3).

% Extraction over time
narrative_ontology:measurement(gita_be_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(gita_be_t1917, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1917, 0.38).
narrative_ontology:measurement(gita_be_t1934, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1934, 0.47).
narrative_ontology:measurement(gita_be_t1950, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1950, 0.52).
narrative_ontology:measurement(gita_be_t1970, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 2000, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gita_kurukshetra_discourse__gandhian_allegorical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposed from the single natural-language label 'the Gita's Kurukshetra discourse' per the epsilon-invariance principle. The orthodox_literal_reading treats the battlefield as historically real and caste duty as divinely mandated (high extraction from a different victim class: those excluded from martial/priestly caste roles by birth); the universalist_devotional_reading treats dharma as path-independent devotional surrender, dissolving caste as a spiritual barrier through a different mechanism than this reading's allegorization of violence. Each reading has a distinct epsilon, distinct beneficiary/victim structure, and distinct claimed_type; they are linked here rather than merged because measuring the same text under different observables (literal-historical vs. allegorical-ethical vs. devotional-universalist) produces genuinely different extraction profiles, which is the definitional signal for decomposition rather than a single parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
