% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Gandhian Allegorical Reading of the Kurukshetra Discourse (Bhagavad Gita)
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading among three declared readings of the
 *   Kurukshetra discourse kernel in the Bhagavad Gita: the Gandhian
 *   allegorical reading, which treats the battlefield as a symbolic staging
 *   ground for internal moral struggle rather than a literal war, repudiates
 *   physical violence as the text's sanctioned method, and relocates
 *   interpretive authority from Brahminical commentarial lineages to
 *   individual conscience. This is not a synthesis of the sibling readings
 *   (orthodox_literal_reading, universalist_devotional_reading) and does not
 *   describe their contest internally — it is a single, ε-stable constraint
 *   describing how this reading operates: who it coordinates, who it
 *   displaces, and at what enforcement cost, principally in the anticolonial
 *   Indian political-religious context circa 1900-1948.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.28).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.42).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of the Kurukshetra Discourse (Bhagavad Gita)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '681014ca-02db-4e21-a8fe-31f2df59c25d').
narrative_ontology:cs_kernel_codification('681014ca-02db-4e21-a8fe-31f2df59c25d', fixed_text).
narrative_ontology:cs_authority_grounding('681014ca-02db-4e21-a8fe-31f2df59c25d', practice).
narrative_ontology:cs_interpretation_layer_present('681014ca-02db-4e21-a8fe-31f2df59c25d').
narrative_ontology:cs_reading_relation('681014ca-02db-4e21-a8fe-31f2df59c25d', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('681014ca-02db-4e21-a8fe-31f2df59c25d', gita_kurukshetra_discourse__universalist_devotional_reading, influences).
narrative_ontology:cs_axiom('681014ca-02db-4e21-a8fe-31f2df59c25d', foundational, ahimsa_supersedes_svadharma_as_supreme_duty).
narrative_ontology:cs_axiom_status(ahimsa_supersedes_svadharma_as_supreme_duty, holdable).
narrative_ontology:cs_axiom_grounding('681014ca-02db-4e21-a8fe-31f2df59c25d', ahimsa_supersedes_svadharma_as_supreme_duty, deontological).
narrative_ontology:cs_axiom('681014ca-02db-4e21-a8fe-31f2df59c25d', foundational, individual_conscience_is_valid_interpretive_authority).
narrative_ontology:cs_axiom_status(individual_conscience_is_valid_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('681014ca-02db-4e21-a8fe-31f2df59c25d', individual_conscience_is_valid_interpretive_authority, conventional).
narrative_ontology:cs_axiom('681014ca-02db-4e21-a8fe-31f2df59c25d', secondary, battlefield_narrative_is_symbolic_not_historical).
narrative_ontology:cs_axiom_status(battlefield_narrative_is_symbolic_not_historical, holdable).
narrative_ontology:cs_axiom_grounding('681014ca-02db-4e21-a8fe-31f2df59c25d', battlefield_narrative_is_symbolic_not_historical, conventional).
narrative_ontology:cs_reference_frame('681014ca-02db-4e21-a8fe-31f2df59c25d', premodern_brahminical_commentarial_synthesis).
narrative_ontology:cs_drift_state('681014ca-02db-4e21-a8fe-31f2df59c25d', post_1920_anticolonial_mobilization, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('681014ca-02db-4e21-a8fe-31f2df59c25d', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, nonviolent_resistance_movements).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, reform_minded_lay_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, anticolonial_political_organizers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_commentarial_authorities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_ritual_specialists).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_hierarchy_beneficiaries_within_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, reform_minded_lay_practitioners).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_as_valid_interpretive_seat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draw on the allegorical reading to recruit the Gita as scriptural warrant for satyagraha and civil disobedience, converting a war narrative into a training manual for internal moral struggle. They gain legitimacy and a canonical text without needing to justify literal violence.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, nonviolent_resistance_movements, beneficiary,
    organized, generational, mobile, national).

% Adopt the allegorical reading to practice the text without deferring to Brahminical intermediaries or accepting caste-based duty as binding. They gain direct interpretive access but must contend with community pushback for departing from received commentary.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, reform_minded_lay_practitioners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, reform_minded_lay_practitioners, payer).

% Use the reframed text to build a mass movement around disciplined nonviolent action, converting a scripture historically read as endorsing righteous war into a foundation for anti-colonial struggle without arms.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, anticolonial_political_organizers, beneficiary,
    organized, generational, mobile, national).

% Their traditional monopoly on adjudicating the text's meaning is bypassed; the allegorical reading routes interpretive authority to individual conscience instead of trained scholarly lineage, eroding their institutional standing and the fee/status structures built on commentarial gatekeeping.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_commentarial_authorities, payer,
    institutional, civilizational, constrained, national).

% Depend on literal readings of caste-based duty (svadharma) to justify their ritual and social role; the allegorical reading strips the divine mandate for caste hierarchy, undermining the textual basis for their position without offering them a comparable replacement role.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, literalist_ritual_specialists, payer,
    powerful, generational, constrained, regional).

% Historically relied on Krishna's counsel to Arjuna as scriptural legitimation for hereditary caste duty and its attendant privileges; the allegorical reading dissolves the divine warrant, removing a load-bearing textual justification for their social position.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_hierarchy_beneficiaries_within_tradition, payer,
    powerful, civilizational, constrained, national).

% Have historically borne the structural violence of caste duty framed as sacred obligation; largely absent from the scholarly and political rooms where the allegorical vs. literal debate is conducted, even though their material situation is the thing the reading contest is ultimately about.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, lower_caste_and_subjugated_communities, excluded,
    powerless, generational, trapped, national).

% Study the allegorical reading as a documented 19th-20th century hermeneutic innovation, tracing its textual warrants, its political utility, and its departure from received commentarial tradition without adjudicating which reading is theologically correct.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive frame that lets a mass political and spiritual movement act in concert under a single canonical authority (the Gita) while committing to nonviolence, resolving the coordination problem of grounding collective moral action in a text otherwise read as endorsing war and caste duty.
% TRANSFER_FUNCTION: Moves interpretive authority away from hereditary commentarial lineages and ritual specialists toward individual conscience and movement leadership; moves the text's legitimating force away from caste-duty justification and toward ahimsa-based political mobilization.
% ABSENT_VOICES: Lower-caste and subjugated communities whose lived experience of structural and literal violence is the substantive stake in the reading contest are largely absent from the scholarly and reformist debate about how to read the text; their material outcomes are argued over on their behalf rather than by them.
% DISAPPEARANCE_RATIONALE: If the allegorical reading were withdrawn as a live interpretive option, the textual scaffolding for organized nonviolent resistance movements grounded in the Gita would lose its scriptural anchor, orthodox commentarial and caste-hierarchy authorities would regain uncontested interpretive ground, and reform-minded practitioners would lose a sanctioned route to practicing the text without caste deference.
% FOUNDING_PROBLEM: Reconciling reverence for a canonical scripture that narrates and appears to sanction righteous violence and caste-based duty with an ethical-political commitment to nonviolence and social equality, at a moment (early 20th century anticolonial struggle) when the text's authority was needed but its literal content was in tension with the movement's method.
% FOUNDING_PROBLEM_CORROBORATION: Gandhi and the movements he shaped attest the allegorical reading resolves a genuine and still-live tension between scriptural reverence and nonviolent commitment. Independent historians of religion and orthodox commentators outside the movement attest the reading is a documented 20th-century innovation departing from the text's dominant premodern reception, in which literal battlefield narrative and caste duty were read as intended rather than symbolic — corroboration for the founding-problem's continued life comes chiefly from within the beneficiary tradition itself, with outside scholarship treating it as a contested retrofit rather than a rediscovery.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-low (0.28) because the reading's primary function is genuinely coordinative — it lets a mass nonviolent movement act under shared scriptural authority — but it also displaces institutional actors (commentarial authorities, ritual specialists, caste-hierarchy beneficiaries) whose textual warrant is dissolved, which is a real cost to a real party, not merely a side effect. Suppression is moderate (0.42) reflecting the active argumentative and institutional effort required to displace centuries of literalist commentarial reading — this is not a costless reinterpretation but one requiring sustained rhetorical and political enforcement (sermons, political writing, movement discipline) to hold against orthodox pushback. Accessibility collapse is moderate (0.35): the literal and universalist-devotional readings remain fully available and practiced by other communities; this reading has not foreclosed them in the wider tradition, only within the movements that adopted it. Resistance is comparatively high (0.55) because orthodox commentarial authorities and caste-hierarchy beneficiaries have actively contested this reading since its emergence, not passively accepted displacement.
 *
 * DIRECTIONALITY LOGIC:
 *   Nonviolent resistance movements, anticolonial organizers, and reform-minded lay practitioners are beneficiaries: they gain a canonical warrant for a method (nonviolent struggle, direct scriptural access) that the literal and caste-duty readings would not license. Orthodox commentarial authorities, literalist ritual specialists, and caste-hierarchy beneficiaries are victims of the reading's displacement effect: their institutional and social position depended in part on the literal, caste-affirming reading remaining canonical, and its erosion is a real structural cost to them, distinct from mere disagreement. Lower-caste and subjugated communities are named as excluded rather than beneficiary or victim in the stakeholder set proper, because although the expected structural delta names them as victims of the constraint the allegorical reading displaces (structural violence of caste and literal war), within THIS reading's own operation they remain largely unrepresented voices whose interests are argued over by others.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling scriptural reverence with a nonviolent ethical-political program — remains genuinely contested rather than resolved or dead: it retains real force within the tradition of use (Gandhian and post-Gandhian movements) even as outside scholarship treats it as a documented innovation rather than a rediscovery of original meaning. This prevents mislabeling the reading as pure invention-for-extraction (a snare) — it does perform real coordination work for the movements that hold it — while also preventing the reverse error of treating it as a costless, victimless clarification: it genuinely displaces institutional actors whose position depended on the sibling literal reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegory_as_genuine_hermeneutic_vs_political_retrofit,
    'Is the allegorical reading a genuine recovery of a symbolic dimension present in the text''s original composition and reception, or a 20th-century political retrofit that repurposes the text''s authority for a program (organized nonviolent resistance) the text''s dominant premodern readers would not have recognized?',
    'Comparative textual-historical analysis of premodern commentarial traditions (Shankara, Ramanuja, Madhva) for allegorical vs. literal treatment of the battlefield narrative, cross-referenced against the documented emergence and political utility of the allegorical reading in Gandhi''s own writings and its reception.',
    'If premodern commentarial traditions show substantial allegorical precedent, this reading''s claim to legitimate continuity with tradition strengthens and its coordination function is better grounded; if the allegorical reading is shown to be a novel 20th-century construction with minimal premodern precedent, its authority rests more heavily on its political utility than on textual fidelity, and its extraction from orthodox authorities looks less like correction and more like appropriation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegory_as_genuine_hermeneutic_vs_political_retrofit, empirical, 'Whether the allegorical reading recovers genuine textual precedent or retrofits the text for political ends.').

omega_variable(
    kernel_reading_relation_to_siblings,
    'Given three declared readings of the same kernel (gandhian_allegorical, orthodox_literal, universalist_devotional), does the allegorical reading''s repudiation of caste-mandate and literal violence logically foreclose the orthodox_literal_reading within a single coherent framework, or can both remain live positions held by different interpretive communities without contradiction at the level of practice?',
    'Examine whether any single interpretive community or institution holds both readings simultaneously without internal contradiction (e.g., a movement that treats the battlefield as symbolic for personal ethics while also treating caste duty as literally binding) versus communities that treat the two premises as mutually exclusive commitments.',
    'If no coherent framework can hold both premises together, the relation should be reclassified from coexists_with toward forecloses; if hybrid holdings are documented, coexists_with is the more accurate structural relation, which is the choice made in this story''s cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_to_siblings, conceptual, 'Whether the allegorical and orthodox-literal readings can coexist within a single framework or are mutually exclusive.').

omega_variable(
    beneficiary_of_caste_delegitimation,
    'Does the delegitimation of caste-mandate readings via the allegorical frame produce concrete material benefit for lower-caste communities, or does it operate primarily at the level of elite reformist and nationalist discourse without translating into structural change for those historically subjected to caste-based structural violence?',
    'Track material and legal outcomes for lower-caste communities across the interval (land rights, temple access, political representation) and assess correlation with the spread of allegorical vs. literal readings among elite reformist circles.',
    'If material benefit is weak, the lower_caste_and_subjugated_communities stakeholder should remain excluded rather than beneficiary, as authored; if strong correlation is found, that group''s role classification and the constraint''s stated coordination function would need revision toward direct beneficiary status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_of_caste_delegitimation, empirical, 'Whether caste-mandate delegitimation via allegory translates into material benefit for lower-caste communities or remains elite discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 1900, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(gita_tr_t1909, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1909, 0.15).
narrative_ontology:measurement(gita_tr_t1919, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(gita_tr_t1930, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1930, 0.24).
narrative_ontology:measurement(gita_tr_t1939, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1939, 0.28).
narrative_ontology:measurement(gita_tr_t1948, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 1948, 0.3).

% Extraction over time
narrative_ontology:measurement(gita_be_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(gita_be_t1909, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1909, 0.18).
narrative_ontology:measurement(gita_be_t1919, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1919, 0.22).
narrative_ontology:measurement(gita_be_t1930, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1930, 0.25).
narrative_ontology:measurement(gita_be_t1939, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1939, 0.27).
narrative_ontology:measurement(gita_be_t1948, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 1948, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t1900, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement(gita_su_t1909, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1909, 0.33).
narrative_ontology:measurement(gita_su_t1919, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1919, 0.36).
narrative_ontology:measurement(gita_su_t1930, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1930, 0.39).
narrative_ontology:measurement(gita_su_t1939, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1939, 0.41).
narrative_ontology:measurement(gita_su_t1948, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 1948, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.1).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of a three-story kernel family (gita_kurukshetra_discourse). orthodox_literal_reading grounds caste-based duty and literal righteous war in divine mandate — the reading this story's coordination function actively displaces. universalist_devotional_reading shares this story's rejection of caste as spiritually determinative but grounds that rejection in path-independent bhakti rather than in ahimsa-as-supreme-principle and allegorical reinterpretation of violence; the two readings exert mutual influence (both erode caste-mandate legitimacy) without fully converging. Each story carries its own ε, stakeholders, and classification; none averages or synthesizes across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
