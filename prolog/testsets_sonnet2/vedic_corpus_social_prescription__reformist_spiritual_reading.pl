% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__reformist_spiritual_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Reformist Spiritual Reading of the Vedic Corpus (Non-Prescriptive Cosmology)
 *   domain: religious_studies/hermeneutics/social_stratification
 *
 * SUMMARY:
 *   This story instantiates the reformist spiritual reading of the Vedic
 *   corpus: the claim that the texts, properly understood, describe spiritual
 *   unity (atman-brahman identity) and metaphorical cosmology, and carry no
 *   binding prescriptive social content. This is one of three structurally
 *   distinct readings of a single contested kernel — the same corpus is read
 *   by the orthodox varna reading as literally prescribing caste hierarchy,
 *   and by the colonial orientalist reading as constituting a unified
 *   codifiable legal system for administrative governance. Per the
 *   ε-invariance principle, these are not the same constraint viewed three
 *   ways; they have different beneficiary/victim structures and different
 *   extraction profiles, so they are authored as three separate constraint
 *   stories linked through the network layer. This file's ε is authored for
 *   the reformist reading's OWN referent: the standing interpretive practice
 *   of reading the corpus as non-prescriptive, as that reading's proponents
 *   themselves operate it — not for the varna hierarchy it rejects (which
 *   would artificially inflate this reading's ε) and not for some averaged or
 *   contested composite.
 *
 * KEY AGENTS:
 *   - reformist_spiritual_practitioners: beneficiary, cross-caste devotional practice
 *   - cross_caste_devotional_movements: organized beneficiary, coordination function
 *   - modern_universalist_hindu_organizations: institutional beneficiary and agenda-setter for the reading's public promotion
 *   - orthodox_varna_traditionalists: excluded, competing interpretive claimants (not bound by this reading)
 *   - textual_historians_and_indologists: analytical observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of the Vedic Corpus (Non-Prescriptive Cosmology)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious_studies/hermeneutics/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '4c78ac72-848a-4892-b4a6-de6a6e696113').
narrative_ontology:cs_kernel_codification('4c78ac72-848a-4892-b4a6-de6a6e696113', distributed).
narrative_ontology:cs_authority_grounding('4c78ac72-848a-4892-b4a6-de6a6e696113', distributed).
narrative_ontology:cs_reading_relation('4c78ac72-848a-4892-b4a6-de6a6e696113', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('4c78ac72-848a-4892-b4a6-de6a6e696113', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('4c78ac72-848a-4892-b4a6-de6a6e696113', foundational, vedic_content_is_exclusively_metaphorical_spiritual).
narrative_ontology:cs_axiom_status(vedic_content_is_exclusively_metaphorical_spiritual, holdable).
narrative_ontology:cs_axiom_grounding('4c78ac72-848a-4892-b4a6-de6a6e696113', vedic_content_is_exclusively_metaphorical_spiritual, conventional).
narrative_ontology:cs_axiom('4c78ac72-848a-4892-b4a6-de6a6e696113', foundational, atman_brahman_identity_is_the_corpus_core_teaching).
narrative_ontology:cs_axiom_status(atman_brahman_identity_is_the_corpus_core_teaching, holdable).
narrative_ontology:cs_axiom_grounding('4c78ac72-848a-4892-b4a6-de6a6e696113', atman_brahman_identity_is_the_corpus_core_teaching, deontological).
narrative_ontology:cs_axiom('4c78ac72-848a-4892-b4a6-de6a6e696113', secondary, varna_references_are_corruption_or_later_interpolation).
narrative_ontology:cs_axiom_status(varna_references_are_corruption_or_later_interpolation, holdable).
narrative_ontology:cs_axiom_grounding('4c78ac72-848a-4892-b4a6-de6a6e696113', varna_references_are_corruption_or_later_interpolation, empirically_contingent).
narrative_ontology:cs_reference_frame('4c78ac72-848a-4892-b4a6-de6a6e696113', vedantic_universalist_hermeneutic).
narrative_ontology:cs_drift_state('4c78ac72-848a-4892-b4a6-de6a6e696113', contemporary_diaspora_reform_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4c78ac72-848a-4892-b4a6-de6a6e696113', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_spiritual_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, cross_caste_devotional_movements).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, modern_universalist_hindu_organizations).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_texts_are_metaphorical_not_legislative).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, atman_brahman_unity_is_the_core_teaching).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read the Vedas, particularly the Upanishadic strata, as describing the identity of atman and brahman and treat cosmological and social passages (including the Purusha Sukta) as metaphor rather than statute. They practice across caste lines and draw on this reading to justify inclusive ritual participation. They pay no cost through this reading; if it were unavailable, they would simply adopt a different hermeneutic or a different textual tradition.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Bhakti-lineage and neo-Vedantic organizations that use the non-prescriptive reading to admit members regardless of birth-caste into study and ritual life, citing the Vedic corpus as compatible with (or actively supportive of) that inclusion. The reading lowers coordination costs for building a multi-caste congregation; it does not extract resources from any excluded party because the reading itself excludes no one.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, cross_caste_devotional_movements, beneficiary,
    organized, generational, mobile, national).

% Institutions (reform societies, diaspora temples, comparative-religion-facing organizations) that promote this reading in publications, translations, and public apologetics, presenting Vedic religion as a universalist spiritual philosophy compatible with modern egalitarian norms. They administer curricula and public messaging built on this reading and benefit reputationally and in membership growth from it.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, modern_universalist_hindu_organizations, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__reformist_spiritual_reading, modern_universalist_hindu_organizations, agenda_setter).

% Hold that the same corpus prescribes a divinely ordained social order and regard the reformist reading as a modern erasure of textual content they consider literal and binding. They are not coordinated by this reading at all — it does not govern or bind them, and they object to its claim of interpretive authority over shared texts, but the reformist reading exerts no coercive force on them; it simply competes for interpretive space.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_traditionalists, excluded,
    organized, civilizational, constrained, national).

% Study the stratified composition of the Vedic corpus (Samhita, Brahmana, Aranyaka, Upanishad layers) and can assess whether the metaphorical-unity reading is textually well-supported for the corpus as a whole, well-supported only for later strata, or a retrospective imposition. Their findings can validate, qualify, or complicate any of the three kernel readings without being party to the social stakes of any of them.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, textual_historians_and_indologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared textual-spiritual vocabulary (atman-brahman unity, cosmic interconnection) that lets practitioners from different caste backgrounds, and different reform lineages, coordinate around a common devotional and philosophical framework without needing to resolve or enforce social hierarchy claims.
% TRANSFER_FUNCTION: Moves essentially nothing coercively: no tribute, labor, or status is extracted from any party through this reading's operation. What it does move is interpretive authority and legitimacy — toward reform organizations and away from hierarchy-based claims on the same texts — but this is a contest over meaning, not a resource transfer with victims.
% ABSENT_VOICES: Orthodox varna traditionalists would strongly object to being told the texts they read as prescriptive are 'merely metaphorical,' and colonial-era codifiers (now largely historical rather than living parties) would object that the corpus was never meant to be read as free-floating philosophy detached from administrable social law. Both are present as siblings in the kernel contest rather than absent from this story, but neither is coordinated or bound by this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live interpretive option, cross-caste devotional and universalist organizations would lose a primary textual warrant for inclusive practice and would need to source legitimacy elsewhere (other scriptures, purely ethical arguments, or reformist commentarial traditions outside the Vedic corpus itself) — a real reorganization, even though no one is currently extracted from by the reading's presence.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century reform movements (Brahmo Samaj, Arya Samaj currents, later neo-Vedanta) needed a way to retain reverence for the Vedic corpus as sacred and authoritative while rejecting caste hierarchy and ritual exclusion as morally untenable; the metaphorical/non-prescriptive reading resolved that tension by relocating the texts' authority to spiritual philosophy alone.
% FOUNDING_PROBLEM_CORROBORATION: Comparative-religion scholars and historians of the reform movements (outside both the reformist organizations and the orthodox lineages) corroborate that the tension between textual reverence and caste critique was a real and continuing motivation, documented in the movements' own polemics and in independent intellectual histories of nineteenth-century Hindu reform; the same historians note the reading is a genuine hermeneutic choice, not the only textually available one.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).
:- end_tests(vedic_corpus_social_prescription__reformist_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.08) because the reading itself imposes no cost, tribute, or exclusion on any party — it is a hermeneutic stance that expands rather than restricts who can participate in Vedic-derived practice. Suppression is low (0.12): no one is coerced into holding this reading, and its spread is through persuasion, translation, and institutional promotion, not enforcement. Theater ratio is modest and slowly rising (0.10 to 0.15) reflecting some performative distance between popular universalist presentations of 'Vedic philosophy' and the more heterogeneous, stratified textual reality documented by historians — a small but real gap between the promoted reading and the corpus's full compositional history. Resistance is comparatively high (0.55) not because the reading extracts from anyone, but because it is actively contested by orthodox traditionalists who dispute its textual accuracy; this is intellectual/religious resistance to a claim, not resistance mounted by victims of an extractive mechanism. Accessibility collapse is low (0.2): alternative readings of the same texts remain fully available and are, in fact, actively practiced by other communities (the sibling readings) — nothing about this reading forecloses access to those alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reformist practitioners, devotional movements, universalist organizations) sit near the full-beneficiary end of directionality: the reading was constructed to serve their coordination and inclusion goals and imposes no reciprocal cost on them. There is no victim group declared for this constraint — the schema does not require one for rope, and none exists structurally: excluding orthodox traditionalists from this reading's authority claim is not extraction, since they suffer no cost from a reading that does not bind them and that they can simply reject. The excluded stakeholder here signals absence of voice in the reading's own promotional discourse, not victimhood of an extractive mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling reverence for sacred texts with rejection of caste hierarchy) remains live wherever reform communities continue to build inclusive practice on Vedic textual warrant, so this is not a case of mandate outliving function. The classification as rope rather than tangled_rope or snare depends on the absence of any coercive mechanism enforcing the reading on outsiders — it coordinates willing adherents without extracting from anyone who declines it, which is exactly the coordination/extraction distinction the framework is built to preserve. Were this reading found to be actively suppressing textual scholarship on the corpus's stratified, historically prescriptive layers (i.e., functioning as institutional censorship of inconvenient textual history), that would push it toward tangled_rope; the story as authored finds no such active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_stratification_support,
    'Is the non-prescriptive, purely metaphorical reading well-supported across the entire Vedic corpus, or only for later strata (principally the Upanishads), with earlier strata (Samhita, Brahmana) containing genuinely prescriptive social and ritual content that this reading must reinterpret or set aside?',
    'Independent philological and historical-critical analysis of the corpus''s compositional layers, compared against how each of the three kernel readings characterizes the corpus as a whole versus specific strata.',
    'If the metaphorical reading is well-supported only for a subset of the corpus, this reading''s claim to describe ''the Vedic texts'' generally would be textually narrower than its promotional presentation suggests, without changing its low-extraction, non-coercive structural profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_stratification_support, empirical, 'Whether non-prescriptive reading holds for the full corpus or only later strata.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one of three readings of the shared kernel vedic_corpus_social_prescription: the orthodox_varna_reading (texts literally and divinely prescribe caste hierarchy), the colonial_orientalist_reading (texts constitute a unified codifiable legal system for administrative governance), and this reformist_spiritual_reading (texts are non-prescriptive metaphorical cosmology). A sibling reading adopting the orthodox premise would treat the same corpus as actively extractive (caste-based victim set); a sibling adopting the colonial-administrative premise would treat it as a constructed legal-governance instrument serving colonial administrative interests. Which reading a given community or scholar adopts is not resolved by the texts alone but by interpretive tradition, institutional position, and reform commitments.',
    'No single empirical test resolves the kernel; adjudication is via hermeneutic and historical argument. Textual-critical scholarship (see textual_stratification_support) can narrow but not close the interpretive gap, since the disagreement concerns how prescriptive content should be weighted, not solely what content exists.',
    'The reading determines the constraint''s entire structural profile: victim set (none here vs. caste-based victims in orthodox_varna_reading vs. colonized-subject victims in colonial_orientalist_reading), extractiveness (low here vs. high in the other two), and claimed type (rope here vs. tangled_rope or snare in the siblings).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'This story is one reading (reformist_spiritual_reading) of the shared kernel; siblings are separate constraint files with different ε and victim structures.').

omega_variable(
    promotional_versus_scholarly_gap,
    'Does the rising theater_ratio reflect a growing gap between how universalist organizations popularly present ''Vedic philosophy'' to lay and diaspora audiences versus how the corpus is actually treated in serious internal scholarly and philological work within those same reform traditions?',
    'Comparative content analysis of public-facing reform materials versus internal scholarly/commentarial literature over the measured interval.',
    'A widening gap would suggest the reading is drifting toward simplified apologetics for lay consumption even as its own tradition''s serious scholarship remains more nuanced — a mild Goodhart-style drift worth tracking, though not enough on current evidence to change the rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(promotional_versus_scholarly_gap, empirical, 'Possible drift between popular promotional framing and internal scholarly nuance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement(vedi_tr_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 120, 0.13).
narrative_ontology:measurement(vedi_tr_t160, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 160, 0.14).
narrative_ontology:measurement(vedi_tr_t200, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 200, 0.15).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 80, 0.07).
narrative_ontology:measurement(vedi_be_t120, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 120, 0.08).
narrative_ontology:measurement(vedi_be_t160, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 160, 0.08).
narrative_ontology:measurement(vedi_be_t200, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 200, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Three constraint files decompose the single natural-language label 'the Vedic corpus's social content' per the ε-invariance principle: orthodox_varna_reading (high ε, caste-based victim set, tangled_rope/snare-leaning), colonial_orientalist_reading (high ε, colonized-administrative victim set, tangled_rope/snare-leaning), and this reformist_spiritual_reading (low ε, no victim set, rope). Each reading is authored as its own constraint with its own stakeholder set and its own ε, assessed by that reading's own lights on the standing arrangement it describes. They are linked here via affects_constraints because they compete for interpretive authority over the same textual kernel and each reading's institutional success shifts the resource and legitimacy environment the others operate in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
