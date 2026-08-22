% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Reformist Spiritual Reading of the Vedic Corpus (No Prescriptive Social Content)
 *   domain: religious/hermeneutic/social
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   vedic_corpus_social_prescription: the reformist spiritual reading, on
 *   which the Vedic corpus teaches spiritual unity and metaphorical cosmology
 *   and binds no social order — varna references being allegory, cosmological
 *   symbolism, or late accretion. The standing arrangement under contest (and
 *   therefore the epsilon referent) is the reformist hermeneutic regime
 *   itself: the denominations, curricula, translations, and citation
 *   practices through which this reading governs how the corpus is taught and
 *   invoked. Assessed by the reading's own lights, that arrangement extracts
 *   little: participation is voluntary, exit is cheap for laity, and the
 *   reading's social payload is egalitarian. The colloquial label 'what do
 *   the Vedas say about caste' decomposes into three structurally distinct
 *   constraints — this reading, the orthodox literal-varna reading, and the
 *   colonial orientalist reading — linked via network.affects_constraints per
 *   the epsilon-invariance principle; the siblings carry the
 *   hierarchy-enforcement and administrative-codification structures
 *   respectively and are NOT described inside this file. Claim and metrics
 *   are independent authored facts: the reading is CLAIMED as rope, and the
 *   metrics describe low-but-nonzero extraction with a slow upward drift
 *   driven by growing apologetic citation rather than by any internal
 *   extractive mechanism.
 *
 * KEY AGENTS:
 *   - - reformist_movement_leaders: Agenda-setting seat (organized/identity_locked) — administers the reading, trains teachers, decides allegorical-versus-literal treatment; institutional livelihood fused with the reading
 *   - - reformist_practitioners: Primary beneficiary seat (moderate/mobile) — adopts the reading for practice and identity; low-cost exit to orthodox or secular life
 *   - - dalit_anticaste_movements: Beneficiary seat with constrained exit (powerless/generational) — deploys the reading as scriptural warrant for equality
 *   - - caste_apologetic_public_intellectuals: Costless-beneficiary seat (powerful/mobile) — cites the reading rhetorically without maintaining it
 *   - - orthodox_pandits: Excluded seat (organized/identity_locked) — holds the rival literal reading; objects from outside the conversation
 *   - - academic_indologists: Analytical observer (institutional/analytical) — tests the reading's textual claims against the full corpus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.2).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.08).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of the Vedic Corpus (No Prescriptive Social Content)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious/hermeneutic/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, '6eb592a9-b825-4959-9a7e-b40e8d8f1e4d').
narrative_ontology:cs_kernel_codification('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', fixed_text).
narrative_ontology:cs_authority_grounding('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', lineage).
narrative_ontology:cs_interpretation_layer_present('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d').
narrative_ontology:cs_reading_relation('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', foundational, vedic_corpus_non_prescriptive).
narrative_ontology:cs_axiom_status(vedic_corpus_non_prescriptive, holdable).
narrative_ontology:cs_axiom_grounding('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', vedic_corpus_non_prescriptive, empirically_contingent).
narrative_ontology:cs_axiom('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', secondary, varna_passages_allegorical_or_late).
narrative_ontology:cs_axiom_status(varna_passages_allegorical_or_late, holdable).
narrative_ontology:cs_axiom_grounding('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', varna_passages_allegorical_or_late, empirically_contingent).
narrative_ontology:cs_reference_frame('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', pristine_revelatory_unity).
narrative_ontology:cs_drift_state('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', contemporary_philological_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6eb592a9-b825-4959-9a7e-b40e8d8f1e4d', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_movement_leaders).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_practitioners).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, dalit_anticaste_movements).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, caste_apologetic_public_intellectuals).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, rigvedic_monism_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, universal_spiritual_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Found and administer reformist denominations built on the spiritual-unity reading: they train teachers, publish vernacular translations, set curricula for study circles, and decide which passages are taught as allegory. Their standing, livelihood, and life's work are bound to the reading's authority; stepping away would mean repudiating their own institution and community.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_movement_leaders, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_movement_leaders, beneficiary).

% Attend satsangs, recite Sanskrit verses, and practice meditation under teachers who present the Vedas as universal spiritual science. They gain a scriptural identity that sits comfortably with modern professional and civic life. Moving to an orthodox congregation or into secular life carries modest cost, mainly frayed friendships and family friction.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Organize against caste discrimination and cite the spiritual-unity reading as evidence that the oldest scriptural layer does not ordain hierarchy — in pamphlets, court argument, and conversion ceremonies. Daily exposure to caste practice does not change with the reading's fortunes, and access to reformist platforms runs through institutions they do not control.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, dalit_anticaste_movements, beneficiary,
    powerless, generational, constrained, national).

% Write columns, give talks, and press culture-war arguments that caste is not intrinsic to Hindu scripture, often adding that census practice hardened it. The reading supplies their central citation. They contribute nothing to maintaining the interpretive tradition and can stop citing it whenever the argument stops paying.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, caste_apologetic_public_intellectuals, beneficiary,
    powerful, biographical, mobile, global).

% Hold hereditary lineages of Vedic recitation and commentarial authority centered on the same corpus read literally, with the Purusha Sukta and the Dharmashastra as anchors. They are not part of the reformist conversation; their objections circulate as polemic from outside, and their standing depends on the transmission the reformist reading bypasses.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_pandits, excluded,
    organized, generational, identity_locked, national).

% Date manuscript strata, edit critical editions, and test claims about the corpus against its full extent, including the Brahmanas and later Vedic prose. Both reformist and orthodox camps recruit their findings; the scholars themselves hold no stake in which reading prevails.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, academic_indologists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_movement_leaders).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared hermeneutic across dispersed reformist communities: one common account of what the Vedas are (spiritual-universalist, non-hierarchical) that lets teachers, laity, and converts align belief and practice without priestly mediation, and supplies scriptural warrant for egalitarian social commitments.
% TRANSFER_FUNCTION: Moves interpretive authority from hereditary pandit lineages to vernacular teachers and lay practitioners, and moves scriptural legitimacy toward anti-hierarchical social claims. Little material wealth moves; what flows is authority, legitimacy, and membership.
% ABSENT_VOICES: Orthodox pandits would object that the reading severs shruti from its commentarial transmission and allegorizes inconvenient passages; they sit outside the reformist conversation entirely, in mathas and traditional schools. Dalit critics in the Ambedkarite line would also object — that the reading sanitizes scripture without redistributing anything on the ground; they sit in political discourse adjacent to, but not inside, the reformist institutions that host the reading.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the reformist denominations lose their founding charter and institutional coherence, anticaste scriptural argument loses one of its principal resources, diaspora universalist identity loses its anchor text-reading, and the apologetic citation economy loses its central exhibit. The orthodox reading would face no internal rival within the reformist space, and the shape of Indian religious modernity would reorganize around the remaining two readings.
% FOUNDING_PROBLEM: The collision between scriptural authority and modernity: how to affirm the Vedas as revelation while rejecting caste hierarchy and answering missionary and orientalist critique — a problem crystallized in the nineteenth-century reform movements (Brahmo Samaj, then Arya Samaj) that needed the oldest scriptural layer to be egalitarian for the tradition to survive modern scrutiny.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: colonial-era administrative records and missionary debate transcripts document the problem as contemporaries framed it; university indology programs independently confirm both the universalist strands and the prescriptive-adjacent passages that make the reconciliation nontrivial; Ambedkar's writings constitute a hostile witness attesting that the problem was live and remained unsolved to his satisfaction; and academic histories of the Arya Samaj and Brahmo Samaj, written by scholars with no confessional stake, attest the founding problem's reality and continuing vitality.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is low (0.20 at interval end): the reading extracts interpretive conformity and modest institutional support from adherents, decoupled from any material transfer, and its drift upward tracks the growth of costless apologetic citation rather than any deepening internal mechanism. Suppression is very low (0.08): the reading maintains itself through voluntary affiliation, publishing, and teaching; it excludes no one from anything material, and orthodox communities operate alongside it untouched. Theater ratio (0.35) is honest about a real performative layer — 'back to the pure Vedas' rhetoric, heritage performance in diaspora contexts, and equality language that sometimes outruns practice — while the functional core (teaching, translation, communal practice) remains genuine. Accessibility_collapse is low (0.30): accepting this reading does not collapse alternatives, since orthodox and secular options remain fully available; the reading competes socially even though, within its own logical framework, it forecloses the literal reading. Resistance (0.45) reflects sustained orthodox polemic (the Sanatanist-Arya Samaj controversies, purdah-and-Purusha-Sukta debates) and philological pushback, met without coercive response. The temporal series run on one shared grid (t=0..150, seven points, both metrics authored at every point). No suppression_requirement series is authored: the enforcement picture is static by design — the reading never built enforcement capacity, so its scalar suppression captures the whole story.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setting seat the reading is a vocation: leaders are identity_locked not by doctrine alone but by institutional identity fusion — their authority, livelihood, and biography ARE the reading, so the same structure that looks like light coordination to a mobile practitioner looks like an existential commitment to them. Practitioners experience a low-stakes voluntary framework with cheap exit. Dalit anticaste movements experience it as a weapon of unequal value: scripturally potent, but accessed through institutions they do not control. Apologetic public intellectuals occupy an arbitrage position — full rhetorical benefit, zero maintenance cost, exit at will — which is why the extraction drift in the measurements attaches to usage patterns rather than to any seat's burden. The excluded orthodox pandits, were they seated, would compute a fifth experience: an attack on transmission that severs shruti from its commentarial chain. The engine derives these divergences from power, exit, and role data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   No victims are declared and none exist inside this reading's operation: every seated party derives a low directionality value near the beneficiary end. Movement leaders derive low d through their beneficiary secondary role (they collect authority and institutional support), moderated by their identity_locked exit, which keeps them from the pure-arbitrage end. Practitioners and dalit anticaste movements derive low d straightforwardly. Apologetic intellectuals derive the lowest d of all: beneficiary role plus mobile exit plus powerful standing places them nearest the subsidy end — they are carried by the reading at no cost. Orthodox pandits and academic indologists are excluded and observer seats respectively; neither feeds the derivation. No directionality_overrides are needed: the structural derivation from beneficiary declarations and exit options already lands every agent where the story says they stand.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling scriptural authority with egalitarian and colonial-era critique — remains live, so no mandatrophy declaration is made and none is warranted: the arrangement has not outlived its function. The classification guards against mislabeling in both directions. Against pure-extraction mislabeling: the reading's costs are interpretive and voluntary, its beneficiaries numerous and its victims absent, which is the signature of genuine coordination rather than cover — the apologetic-use risk is routed to an omega rather than baked into the type. Against naive-rope complacency: the rising theater_ratio series is authored honestly so the lifecycle detector can flag heritage-performance drift if it crosses the functional threshold. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — a coherent pairing that raises no zombie flag; the reading would be missed if it vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the kernel vedic_corpus_social_prescription; what would the sibling readings (orthodox_varna_reading, colonial_orientalist_reading) change structurally if adopted in place of this one?',
    'Compile and compare the sibling stories: presence of victim sets, enforcement requirements, and epsilon deltas locate where the disagreement carries structural weight.',
    'If the orthodox sibling''s structure governs actual social behavior while this reading governs only self-description, this story''s low measured extraction understates the kernel''s total extraction across the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this file instantiates one of three competing readings of a contested scriptural kernel.').

omega_variable(
    purusha_sukta_binding_force,
    'Is the Purusha Sukta''s varna cosmogony (RV 10.90) descriptive myth, allegory, or a prescriptive charter, and does its genre settle its social force?',
    'Philological and reception-history analysis: comparison with Indo-European cosmogonic genres, the earliest commentarial treatments, and whether any pre-modern community actually derived binding social duty from the hymn.',
    'If prescriptive-charter readings dominated pre-modern reception, the no-prescription axiom weakens and this reading''s reference frame destabilizes; if allegorical or descriptive readings dominate, the frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purusha_sukta_binding_force, empirical, 'Genre and reception status of the strongest counterexample passage inside the corpus.').

omega_variable(
    apologetic_displacement_effect,
    'Does adoption of the no-prescription reading reduce caste practice, or does it displace hierarchy onto sibling arrangements (custom, Dharmashastra-derived practice) while sanitizing self-description?',
    'Comparative community studies measuring caste-practice indicators (marriage patterns, temple access, labor relations) in communities where this reading is dominant versus matched orthodox communities.',
    'If displacement dominates, the reading functions as reputational cover and its effective extraction rises above the authored low value despite carrying no internal victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apologetic_displacement_effect, empirical, 'Whether the reading reforms practice or launders reputation.').

omega_variable(
    corpus_boundary_underdetermination,
    'Does ''Vedic'' mean the Rigveda Samhita alone (where the no-prescription claim is strongest), the four Samhitas, or shruti inclusive of Brahmanas and Upanishads (where prescriptive-adjacent content multiplies)?',
    'Track which corpus boundary the reading''s own authorities commit to in teaching, translation, and canon formation; the boundary is a framing choice, not a discovery.',
    'A wider boundary forces the allegory-or-late-stratum apparatus to work harder and raises the reading''s fragility; a narrow boundary stabilizes it but invites the cherry-picking charge. This story''s classification is stable under either framing; the balance of the sibling dispute shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corpus_boundary_underdetermination, conceptual, 'Framing under-determination of the authoritative corpus boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t25, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(vedi_tr_t50, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(vedi_tr_t75, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 75, 0.24).
narrative_ontology:measurement(vedi_tr_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 100, 0.27).
narrative_ontology:measurement(vedi_tr_t125, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 125, 0.31).
narrative_ontology:measurement(vedi_tr_t150, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 150, 0.35).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement(vedi_be_t25, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 25, 0.14).
narrative_ontology:measurement(vedi_be_t50, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(vedi_be_t75, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 75, 0.16).
narrative_ontology:measurement(vedi_be_t100, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 100, 0.17).
narrative_ontology:measurement(vedi_be_t125, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 125, 0.18).
narrative_ontology:measurement(vedi_be_t150, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 150, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'do the Vedas mandate caste?' fails the epsilon-invariance test as a single constraint: measuring it through the reformist reading yields negligible extraction with no victim set, while measuring it through the orthodox reading yields enforced hierarchy with identifiable victims, and through the orientalist reading yields administrative codification with its own beneficiary structure. Three labels, three constraints, three files. This file holds the reformist reading (low epsilon, no victims, voluntary affiliation). The upstream/downstream structure runs both ways: the orthodox reading supplies the literal target this reading defines itself against, and this reading's spread exerts structural pressure on the orientalist codification project by undermining its scriptural warrant. All three stories link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
