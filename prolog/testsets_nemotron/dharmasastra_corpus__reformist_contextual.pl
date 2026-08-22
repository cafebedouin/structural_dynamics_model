% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Reformist Contextual Reading of Dharmasastra Authority
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The reformist contextual reading of the Dharmasastra corpus attempts to
 *   separate an ethical core (dharma as righteous conduct, universalizable
 *   moral principles) from historically conditioned social prescriptions
 *   (varna/jati hierarchy, gender roles, ritual purity laws). It emerged in
 *   the 19th century as a response to colonial and missionary critique, and
 *   persists today as the dominant interpretive framework for progressive
 *   Hindu institutions and diaspora communities. The constraint is the
 *   interpretive regime itself: the set of hermeneutic rules, institutional
 *   authorities, and social practices that determine which textual
 *   prescriptions are binding and which are contextual. This reading claims
 *   to be a rope (genuine coordination: preserving tradition while enabling
 *   reform), but its metrics reveal substantial extraction (symbolic
 *   hierarchy persists, marginalized groups pay dignitary costs) and active
 *   enforcement (institutional gatekeeping against both orthodox and
 *   abolitionist challengers) — a tangled rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.45).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.38).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Reformist Contextual Reading of Dharmasastra Authority").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'b7da638c-9cdb-48a7-addb-355d47f1e628').
narrative_ontology:cs_kernel_codification('b7da638c-9cdb-48a7-addb-355d47f1e628', fixed_text).
narrative_ontology:cs_authority_grounding('b7da638c-9cdb-48a7-addb-355d47f1e628', lineage).
narrative_ontology:cs_interpretation_layer_present('b7da638c-9cdb-48a7-addb-355d47f1e628').
narrative_ontology:cs_reading_relation('b7da638c-9cdb-48a7-addb-355d47f1e628', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('b7da638c-9cdb-48a7-addb-355d47f1e628', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('b7da638c-9cdb-48a7-addb-355d47f1e628', foundational, dharma_as_righteous_conduct_universalizable).
narrative_ontology:cs_axiom_status(dharma_as_righteous_conduct_universalizable, holdable).
narrative_ontology:cs_axiom_grounding('b7da638c-9cdb-48a7-addb-355d47f1e628', dharma_as_righteous_conduct_universalizable, deontological).
narrative_ontology:cs_axiom('b7da638c-9cdb-48a7-addb-355d47f1e628', foundational, social_prescriptions_historically_conditioned).
narrative_ontology:cs_axiom_status(social_prescriptions_historically_conditioned, holdable).
narrative_ontology:cs_axiom_grounding('b7da638c-9cdb-48a7-addb-355d47f1e628', social_prescriptions_historically_conditioned, empirically_contingent).
narrative_ontology:cs_reference_frame('b7da638c-9cdb-48a7-addb-355d47f1e628', smriti_tradition_as_living_authority).
narrative_ontology:cs_drift_state('b7da638c-9cdb-48a7-addb-355d47f1e628', post_ambedkar_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b7da638c-9cdb-48a7-addb-355d47f1e628', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_scholars).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, progressive_hindu_institutions).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, modern_practitioners_seeking_continuity).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, marginalized_castes_under_symbolic_hierarchy).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_under_patriarchal_residues).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, dharma_as_righteous_conduct).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, textual_authority_preserved_through_reinterpretation).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__reformist_contextual, historical_conditionality_of_social_prescriptions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produce commentaries and institutional frameworks that reinterpret caste prescriptions as historical artifacts while preserving the Dharmasastra's ethical authority. They administer the interpretive apparatus (academic chairs, monastic lineages, NGO networks) that determines which passages are binding and which are contextual. Their professional standing and funding depend on the reading's viability as a middle path.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_scholars, agenda_setter,
    institutional, generational, mobile, global).

% Temples, ashrams, and educational trusts that adopt the reformist reading to retain legitimacy with modern constituencies (diaspora, urban middle class, interfaith dialogue). They gain donor support and regulatory goodwill by demonstrating caste reform while keeping textual tradition intact. Exit means losing the tradition's authority entirely — a cost they are structured to avoid.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, progressive_hindu_institutions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__reformist_contextual, progressive_hindu_institutions, agenda_setter).

% Lay adherents who want to practice Hinduism without endorsing caste oppression. The reformist reading gives them a vocabulary (dharma as righteous conduct, varna as spiritual aptitude) that lets them stay inside the tradition. Their exit options are limited: secularism severs cultural roots; orthodox communities enforce caste norms; other traditions require conversion. They pay with cognitive dissonance when symbolic hierarchy resurfaces in rituals.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, modern_practitioners_seeking_continuity, beneficiary,
    moderate, biographical, constrained, global).

% Dalit and Bahujan communities who remain subject to the symbolic force of varna hierarchy even when its legal enforcement is rejected. The reformist reading reinterprets rather than repudiates the textual architecture of their subordination — their ancestors are still described in the same texts as born from the feet of Purusha. They cannot exit the textual world that names them because that world constitutes the cultural grammar of their society. Resistance is met with 'we already reformed' dismissal.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, marginalized_castes_under_symbolic_hierarchy, payer,
    powerless, generational, identity_locked, national).

% Women who find that reformist reinterpretation of caste prescriptions leaves patriarchal family law (marriage, inheritance, ritual purity) largely intact. The same hermeneutic that historicizes varna treats gender prescriptions as essential to dharma. Their identity is fused to the textual tradition through family, ritual, and community — exit means alienation from kinship networks and cultural memory. They bear the cost of a hierarchy that the reading claims to have softened.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_under_patriarchal_residues, payer,
    powerless, biographical, identity_locked, national).

% Traditional pandits, matha heads, and organizations (e.g., VHP, RSS-affiliated trusts) who hold that Dharmasastra is shruti-smriti continuum requiring literal observance. They are structurally excluded from the reformist reading's interpretive circle — their premise (eternal revealed truth) is the very thing the reading historicizes. They control parallel institutions (temples, schools, certification bodies) and deploy state power where aligned. Their exclusion is not marginalization; it is the condition of the reformist reading's coherence.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_authorities, excluded,
    institutional, civilizational, trapped, global).

% Dalit-Bahujan intellectual and political formations (Ambedkarite organizations, Buddhist converts, constitutional rights groups) who argue the Dharmasastra corpus has no legitimate authority and must be wholly abandoned. They are excluded because the reformist reading's project — saving the text — is precisely what they reject. Their exit is mobile: they have built alternative epistemologies (Buddhism, constitutionalism, oral traditions) and institutional power (reservations, legislative presence). They do not need the reformist reading's permission.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_ambedkarite_voices, excluded,
    organized, generational, mobile, national).

% Indian judiciary adjudicating religious freedom vs. equality claims (Articles 25-28 vs. 14-17). They observe the reformist reading as a contestant in the 'essential practices' test: does the reading represent genuine Hinduism, or a strategic reinterpretation? Their rulings shape which reading gains state recognition and thus material resources. They do not collect from the constraint; they regulate its public instantiation.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, secular_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutic bridge allowing communities to maintain textual continuity and cultural identity while repudiating the caste hierarchy that the texts explicitly prescribe. Solves the coordination problem of 'how to remain Hindu without endorsing Manusmriti's varna-jati system' by designating an ethical core (dharma as righteous conduct) that survives historical critique.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from orthodox literalists to reformist scholars and progressive institutions. Moves the cost of symbolic hierarchy onto marginalized castes and women who remain subject to its residual force in ritual, family law, and social imagination. The extraction is not monetary but epistemic and dignitary: the right to define what the tradition *is* accrues to the reformists; the cost of what the tradition *does* falls on those it still subordinates.
% ABSENT_VOICES: The abolitionist Ambedkarite position — that no reinterpretation can legitimate a corpus whose founding move is the theological encoding of caste — is structurally excluded from the reformist reading's framework. They are not in the room because the room is defined by the project of saving the text. Their objection (the text itself is the weapon) would dissolve the reading's premise. Also absent: the vast majority of rural, non-literate Dalit communities who lack access to the Sanskrit-English interpretive apparatus but live the hierarchy's daily enforcement.
% DISAPPEARANCE_RATIONALE: If the reformist reading vanished overnight, the middle ground between orthodox literalism and abolitionist rejection would collapse. Progressive institutions would lose their hermeneutic license to operate within the tradition; practitioners seeking continuity would face a binary choice (orthodoxy or exit); marginalized castes would lose the 'we reformed' shield that currently deflects some critique; courts would lose a key contestant in essential-practices jurisprudence. The textual authority would polarize into two incommensurable readings.
% FOUNDING_PROBLEM: Late colonial and early postcolonial Hindu reformers (Rammohan Roy, Dayananda Saraswati, Gandhi, Ambedkar's early interlocutors) needed to defend Hindu tradition against missionary and colonial critique that held up Manusmriti as proof of Hinduism's inherent degeneracy. The founding problem was: how to retain the authority of the Dharmasastra corpus while disavowing its most indefensible prescriptions (caste hierarchy, sati, child marriage, women's subordination) without surrendering the claim that the texts are divinely sourced or eternally valid.
% FOUNDING_PROBLEM_CORROBORATION: Reformist lineages (Arya Samaj, Brahmo Samaj, Gandhi's Harijan Sevak Sangh, modern academic Hindu studies) attest the problem is live: colonial critique persists in new forms (human rights discourse, caste census demands), so the hermeneutic bridge remains necessary. Ambedkarite and Marxist historians (corroborating from outside the beneficiary set) attest the problem was strategically constructed: the 'colonial critique' narrative obscures that indigenous anti-caste traditions (Buddhism, Bhakti, Nayanar/Sant lineages) already rejected the texts centuries before colonialism. The reformist reading's founding problem is itself a historical construction that naturalizes a particular elite response to modernity.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is medium: the reading extracts interpretive authority and cultural capital from marginalized groups by monopolizing the 'legitimate Hindu reform' position, while the hierarchy it claims to historicize continues to operate symbolically. Suppression (0.38) is moderate: the reading does not legally enforce caste, but it deploys institutional power (academic, religious, state-recognized) to marginalize abolitionist readings and police the boundaries of acceptable reinterpretation. Theater ratio (0.42) is elevated: a significant portion of reformist activity (conferences, publications, interfaith statements) performs the *appearance* of caste rejection while the symbolic architecture remains intact in ritual, marriage markets, and temple governance. Accessibility collapse (0.52) is partial: alternatives exist (abolitionism, secularism, conversion) but are culturally costly. Resistance (0.55) is significant: from orthodox literalists (who reject historicization), abolitionists (who reject the corpus), and marginalized communities (who experience the reading as co-optation).
 *
 * PERSPECTIVAL GAP:
 *   From the reformist scholar's seat, the constraint is a rope: it coordinates a community around a viable ethical tradition. From the marginalized caste seat, it is a snare: the coordination story is cover for preserving the textual architecture of their subordination. From the orthodox seat, it is a corruption of revelation. The engine will compute these divergences from the structural data — the claimed_type (tangled_rope) is my assessment that the constraint *structurally* has both coordination and extraction, not that any single seat experiences it that way.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and progressive institutions are structural beneficiaries (d near 0.15): they collect interpretive authority, funding, and legitimacy. Modern practitioners are near-symmetric beneficiaries/payers (d ~0.45): they gain cultural continuity but pay cognitive dissonance costs. Marginalized castes and women are targets (d ~0.85): they bear the residual symbolic hierarchy with identity-locked exit. Orthodox literalists are excluded but powerful — their exclusion is the reading's boundary condition. Abolitionists are excluded but mobile — they have alternative frameworks. Courts are analytical observers. The identity_locked exit for marginalized groups reflects that their subordination is constituted *by* the textual world the reading preserves; they cannot exit the grammar that names them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defending Hindu tradition against colonial degeneracy discourse) is contested: reformists say it persists; abolitionists say it was a strategic construction that obscures indigenous anti-caste traditions. The mandate has partially outlived its function — colonial critique has mutated, but the reading's hermeneutic now serves new functions (diaspora identity, interfaith legitimacy, academic institutionalization). The constraint persists not because the founding problem is live, but because the interpretive apparatus has become self-sustaining: careers, institutions, and funding streams depend on the reading's viability. This is mandatrophy — the mandate atrophied, the structure remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_hierarchy_persistence,
    'Does the reformist reading''s symbolic retention of varna terminology (as ''spiritual stages'' or ''psychological types'') functionally sustain caste hierarchy, or does it genuinely defang the textual architecture of subordination?',
    'Longitudinal sociolinguistic study of how reformist communities actually use varna language in marriage, ritual, and self-identification vs. orthodox communities. If usage patterns converge, the symbolic retention is functional hierarchy.',
    'If symbolic retention sustains hierarchy, the reading''s extraction is higher than measured — the dignitary cost to marginalized groups persists through the very vocabulary of ''reform.'' If it genuinely defangs, the reading approaches rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symbolic_hierarchy_persistence, empirical, 'Whether reinterpreted varna language operates as hierarchy in practice.').

omega_variable(
    committer_frame_underdetermination,
    'Is the reformist reading''s claim to preserve ''the ethical core'' a defensible textual operation, or a projection that reads modern liberal values back into a corpus whose internal logic is irreducibly hierarchical?',
    'Philological comparison: does the Dharmasastra corpus contain a coherent, extractable ''ethical core'' (dharma as righteous conduct) that is logically independent of its varna-jati prescriptions, or are the two mutually constituting? Expert consensus from text-historical scholars outside the reformist lineage.',
    'If the ethical core is a projection, the reading''s coordination function is illusory — it coordinates around a unity the texts do not contain. The constraint would reclassify toward snare (coordination story as cover). If the core is textually defensible, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_underdetermination, conceptual, 'Whether the reformist reading''s central hermeneutic move is textually warranted or a modern projection.').

omega_variable(
    institutional_capture_of_reform,
    'To what extent have progressive Hindu institutions captured the reformist reading to serve their own legitimacy and funding needs, rather than the interests of the marginalized communities the reading claims to serve?',
    'Trace funding flows, board compositions, and policy positions of major reformist institutions (ashrams, trusts, academic centers) against the stated demands of Dalit-Bahujan and women''s organizations. Divergence indicates capture.',
    'If capture is extensive, the beneficiary structure shifts: progressive institutions become the primary extractors, using the reading to harvest legitimacy resources while marginalized groups remain payers. The reading''s claimed coordination function (serving the marginalized) would be falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_reform, empirical, 'Whether reformist institutions serve marginalized communities or their own perpetuation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 1820, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1820, dharmasastra_corpus__reformist_contextual, theater_ratio, 1820, 0.55).
narrative_ontology:measurement(dhar_tr_t1860, dharmasastra_corpus__reformist_contextual, theater_ratio, 1860, 0.5).
narrative_ontology:measurement(dhar_tr_t1900, dharmasastra_corpus__reformist_contextual, theater_ratio, 1900, 0.48).
narrative_ontology:measurement(dhar_tr_t1950, dharmasastra_corpus__reformist_contextual, theater_ratio, 1950, 0.45).
narrative_ontology:measurement(dhar_tr_t1975, dharmasastra_corpus__reformist_contextual, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__reformist_contextual, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(dhar_tr_t2025, dharmasastra_corpus__reformist_contextual, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1820, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1820, 0.65).
narrative_ontology:measurement(dhar_be_t1860, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1860, 0.58).
narrative_ontology:measurement(dhar_be_t1900, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement(dhar_be_t1950, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement(dhar_be_t1975, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1975, 0.44).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(dhar_be_t2025, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1820, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1820, 0.7).
narrative_ontology:measurement(dhar_su_t1860, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1860, 0.62).
narrative_ontology:measurement(dhar_su_t1900, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(dhar_su_t1950, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1950, 0.35).
narrative_ontology:measurement(dhar_su_t1975, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(dhar_su_t2025, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__reformist_contextual, 0.12).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, hindu_personal_law_codification).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, temple_entry_movements).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, caste_census_politics).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel decomposes into three constraint stories: (1) orthodox_literalist — Mountain claim (eternal truth), negligible extraction from its own seat, high suppression of dissent; (2) reformist_contextual (this story) — Tangled Rope: genuine coordination function (cultural continuity without caste endorsement) + asymmetric extraction (marginalized groups bear symbolic hierarchy); (3) abolitionist_rejection — Snare from orthodox/reformist seats (attacks their authority), Mountain from its own seat (liberation as natural law). The ε values differ radically: orthodox claims ε≈0 for itself; reformist authors ε=0.45; abolitionist authors ε≈0.8 for the corpus-as-standing-arrangement. They are not the same constraint viewed differently — they are structurally distinct constraints instantiated from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, institutional, 0.15).
constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
