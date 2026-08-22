% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__reformist_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: vedic_corpus_social_prescription__reformist_spiritual_reading
 *   human_readable: Reformist Spiritual Reading of the Vedic Corpus (No Prescriptive Social Content)
 *   domain: religious/hermeneutic/social
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the reformist spiritual reading —
 *   of the contested kernel vedic_corpus_social_prescription: what the Vedic
 *   corpus prescribes for social order. Under this reading the constraint is
 *   an interpretive norm: the corpus's core teaches spiritual unity (ekam
 *   sat) and metaphorical cosmology and contains no binding social
 *   legislation; varna verses are cosmogonic allegory or later accretion. The
 *   standing arrangement under contest — communities governing their relation
 *   to the corpus by this norm — is assessed here by the reading's own
 *   lights, yielding a single stable epsilon. Family decomposition per the
 *   epsilon-invariance principle: the colloquial label 'what do the Vedas say
 *   about society?' covers three structurally distinct constraints — the
 *   orthodox varna reading (high epsilon; victim set: persons bound to
 *   servile varna duties), the colonial orientalist reading (high epsilon;
 *   victims: colonized populations governed by codified 'Hindu law' and a
 *   tradition frozen for administration), and this reading (epsilon 0.14; no
 *   victim set). They are separate files linked by
 *   network.affects_constraints; nothing here averages across them.
 *
 * KEY AGENTS:
 *   - reformist_acharya_lineages: agenda-setting beneficiary (organized/mobile) — runs the teaching, translation, and curricular apparatus that maintains the reading
 *   - diaspora_hindu_communities: primary beneficiary (moderate/identity_locked) — inherits continuity with the corpus without caste obligation
 *   - universalist_vedanta_adherents: secondary beneficiary (moderate/mobile) — enters the tradition through the reading's universalist frame
 *   - orthodox_pandit_establishment: excluded voice (institutional/identity_locked) — custodian of the commentarial tradition, contests the reading from outside reformist institutions
 *   - philological_indology_scholars: analytical observer (institutional/analytical) — dates the strata and catalogs prescriptive material; collects and pays nothing
 *   - dalit_equality_advocates: excluded voice (organized/constrained) — objects that the reading launders ongoing hierarchy; outside the reformist hermeneutic forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.14).
domain_priors:suppression_score(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.15).
domain_priors:theater_ratio(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__reformist_spiritual_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__reformist_spiritual_reading, rope).
narrative_ontology:human_readable(vedic_corpus_social_prescription__reformist_spiritual_reading, "Reformist Spiritual Reading of the Vedic Corpus (No Prescriptive Social Content)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__reformist_spiritual_reading, "religious/hermeneutic/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__reformist_spiritual_reading, 'd6be310f-c71c-4cfc-8943-a3e0875f8356').
narrative_ontology:cs_kernel_codification('d6be310f-c71c-4cfc-8943-a3e0875f8356', fixed_text).
narrative_ontology:cs_authority_grounding('d6be310f-c71c-4cfc-8943-a3e0875f8356', lineage).
narrative_ontology:cs_interpretation_layer_present('d6be310f-c71c-4cfc-8943-a3e0875f8356').
narrative_ontology:cs_reading_relation('d6be310f-c71c-4cfc-8943-a3e0875f8356', vedic_corpus_social_prescription__orthodox_varna_reading, forecloses).
narrative_ontology:cs_reading_relation('d6be310f-c71c-4cfc-8943-a3e0875f8356', vedic_corpus_social_prescription__colonial_orientalist_reading, forecloses).
narrative_ontology:cs_axiom('d6be310f-c71c-4cfc-8943-a3e0875f8356', foundational, vedic_core_contains_no_social_legislation).
narrative_ontology:cs_axiom_status(vedic_core_contains_no_social_legislation, holdable).
narrative_ontology:cs_axiom_grounding('d6be310f-c71c-4cfc-8943-a3e0875f8356', vedic_core_contains_no_social_legislation, empirically_contingent).
narrative_ontology:cs_axiom('d6be310f-c71c-4cfc-8943-a3e0875f8356', foundational, spiritual_unity_is_the_corpuses_teaching).
narrative_ontology:cs_axiom_status(spiritual_unity_is_the_corpuses_teaching, holdable).
narrative_ontology:cs_axiom_grounding('d6be310f-c71c-4cfc-8943-a3e0875f8356', spiritual_unity_is_the_corpuses_teaching, theological).
narrative_ontology:cs_axiom('d6be310f-c71c-4cfc-8943-a3e0875f8356', secondary, varna_verses_are_cosmogonic_allegory).
narrative_ontology:cs_axiom_status(varna_verses_are_cosmogonic_allegory, holdable).
narrative_ontology:cs_axiom_grounding('d6be310f-c71c-4cfc-8943-a3e0875f8356', varna_verses_are_cosmogonic_allegory, empirically_contingent).
narrative_ontology:cs_reference_frame('d6be310f-c71c-4cfc-8943-a3e0875f8356', pristine_revelation_without_social_law).
narrative_ontology:cs_drift_state('d6be310f-c71c-4cfc-8943-a3e0875f8356', contemporary_philological_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6be310f-c71c-4cfc-8943-a3e0875f8356', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__reformist_spiritual_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_acharya_lineages).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, diaspora_hindu_communities).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__reformist_spiritual_reading, universalist_vedanta_adherents).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, ekam_sat_pluralism_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, varna_verses_cosmogonic_allegory_thesis).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__reformist_spiritual_reading, social_legislation_as_later_accretion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teacher lineages, monastic orders, and publishing houses in the Arya Samaj, Ramakrishna Mission, and global Vedanta lines that translate the samhitas, train teachers, and set the curricula through which the reading is transmitted. They decide which commentaries circulate and how the varna verses are glossed. Their reward is continuity, students, and standing; their cost is the scholarly labor of maintaining the gloss. They could teach a different hermeneutic, at reputational cost to their lineages.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_acharya_lineages, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__reformist_spiritual_reading, reformist_acharya_lineages, beneficiary).

% Second- and third-generation practitioners and temple congregations outside South Asia who learn the tradition through this reading: they recite, festival-keep, and study the corpus without inheriting caste obligations. Leaving the reading would mean either accepting hierarchical framings of their own scriptures or stepping away from the heritage altogether; both are costly to family ties and self-understanding.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, diaspora_hindu_communities, beneficiary,
    moderate, biographical, identity_locked, global).

% Converts, yoga-lineage students, and spiritual seekers who enter through the reading's universalist frame that truth is one and sages name it variously. The reading is their on-ramp to the corpus. Little binds them: they can disaffiliate or switch lineages cheaply, and many hold the reading lightly alongside other practices.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, universalist_vedanta_adherents, beneficiary,
    moderate, biographical, mobile, global).

% Custodians of the commentarial transmission — traditional pathshalas, dharmashastra pedagogy, ritual families — who hold that the corpus reaches practice only through its received interpreters and that the reformist gloss severs text from tradition. They teach and publish in their own venues and are largely absent from reformist seminaries, diaspora pulpits, and global Vedanta curricula.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_pandit_establishment, excluded,
    institutional, generational, identity_locked, continental).

% Academic philologists and historians of religion who date the corpus's strata, catalog prescriptive material in the Vedic and immediately post-Vedic layers, and publish stratification and reception studies. They collect no benefit and bear no cost from the reading; their findings bear on every reading of the corpus and are cited by all sides.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, philological_indology_scholars, observer,
    institutional, generational, analytical, global).

% Ambedkarite and backward-class equality movements that press the lived reality of caste against scriptural exoneration narratives. They argue the reading relocates hierarchy outside scripture while hierarchy persists in practice, and they seek standing in the forums where the corpus's social meaning is decided; they are rarely invited into reformist teaching institutions.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__reformist_spiritual_reading, dalit_equality_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__reformist_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__reformist_spiritual_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: how communities that reject hereditary hierarchy can nonetheless share one scriptural canon — common recitation, curricula, and conversion-and-retention practices — without each household renegotiating the corpus's authority from scratch. The reading supplies a shared interpretive standard (the core teaches unity; social rules are later accretion) that lets egalitarian families, diaspora temples, and global Vedanta centers use the same texts.
% TRANSFER_FUNCTION: Moves interpretive authority from hereditary exegetes to reformist teachers, translators, and lay readers; moves cultural continuity and belonging to adherents; moves little material value — the principal flows are prestige, membership, and voluntary scholarly labor toward the institutions that maintain the reading.
% ABSENT_VOICES: Orthodox pandits and dharmashastra specialists would object that the reading amputates the corpus from its transmission chain; Dalit and backward-class equality advocates would object that it launders continuing caste practice by relocating hierarchy outside scripture. Both are largely absent from reformist seminary syllabi, diaspora pulpit circuits, and global Vedanta curricula, where the reading is taught as settled.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, millions of practitioners would lose their working settlement between scriptural reverence and egalitarian ethics: diaspora religious education, convert onboarding, and reformist liturgy would reorganize around either orthodox hermeneutics, with caste obligations reattached, or exit from the tradition; the reformist institutional sector built on the reading would dissolve.
% FOUNDING_PROBLEM: The nineteenth-century collision between colonial-modern egalitarian critique and scriptural authority: reformers in the Brahmo Samaj, Arya Samaj, and Neo-Vedanta needed the Vedic corpus to remain authoritative and revered while rejecting caste as divinely ordained; the reading was built to secure that reconciliation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic indology documents the stratified corpus and locates prescriptive material in later strata; colonial administrative records show the governance problem that codification addressed; Dalit intellectual testimony, notably Ambedkar's Annihilation of Caste, attests that the authority-versus-equality tension the reading manages remains unresolved. No corroborating source attests the problem is dead.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__reformist_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__reformist_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__reformist_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__reformist_spiritual_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

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
 *   Authored as rope: a genuine coordination function (shared canonical access under egalitarian commitments), minimal coercive overhead, net-beneficiary participants, and live alternatives. Metric rationale: extractiveness 0.14 sits just above the identity_coordination floor (0.08), reflecting institutional self-perpetuation and prestige flows but no rent collection; suppression 0.15 is a raw, unscaled structural property limited to in-group conformity pressure, since rival readings are neither banned nor priced out; theater_ratio 0.12 because teaching, translation, and recitation do real work; accessibility_collapse 0.30 because understanding the hermeneutic does not foreclose orthodox or philological alternatives, which remain fully live; resistance 0.50 because sustained orthodox, philological, and Dalit-critical pushback keeps the reading contested rather than settled. The two temporal series share one six-point grid (1820-2020); both rise gently as the reading professionalizes and acquires apologetic deployments. No suppression_requirement series is authored: the reading has no enforcement-capacity dynamic to trace, and the static picture is carried by the scalar. Receipt surface: extraction is floor-level and no named seat captures it — benefits flow diffusely to practitioners as continuity and belonging; fixing (any community abandoning the reading) is cheap, so persistence signals service rather than lock-in.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the reading is faithful recovery of the corpus's plain sense; from the diaspora beneficiary seat it is inherited liberation; from the excluded orthodox seat it is amputation of the tradition from its own commentary; from the observer seat it is one live hermeneutic among three. Same texts, four experiences. The engine computes per-seat classifications from the structural data — power, exit, role — not from this prose.
 *
 * DIRECTIONALITY LOGIC:
 *   All three declared beneficiary groups derive low directionality: the reading subsidizes continuity and belonging rather than taxing anyone. The agenda-setting lineage sits nearest the beneficiary pole (it both administers the norm and collects legitimacy from it); diaspora communities, identity_locked, sit deeper toward subsidy than mobile universalist adherents, whose cheap exit already dampens any residual cost they bear. Excluded and observer seats contribute no extraction arithmetic as parties. Spatial scope is global, which modestly amplifies whatever extraction exists in the engine's computation — hence epsilon 0.14 rather than the bare coordination floor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling scriptural authority with egalitarian ethics — remains live wherever the corpus is taught, so no mandatrophy obtains and the scaffold/piton question does not arise. The classification guards against both mislabelings: against pure-extraction labeling (the reading serves status and continuity interests, but no victim set and no suppressed exit run through its structure) and against rope-romanticization (the omegas track whether apologetic deployment constitutes a separate extractive arrangement that would deserve its own story with victims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading (reformist_spiritual_reading) of the kernel vedic_corpus_social_prescription; do the three sibling readings differ merely in evaluation of one constraint, or does each instantiate a structurally distinct constraint with its own epsilon, victim set, and enforcement profile?',
    'Compile and classify all three sibling stories (orthodox_varna_reading, colonial_orientalist_reading, this file) and compare epsilon, victim sets, and enforcement requirements across the family.',
    'Confirms the family decomposition and the linkage edges; if the readings turned out to share one epsilon, the three files would collapse into one constraint and the network edges would be redundant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the kernel''s readings are separate constraints or one constraint seen from three angles.').

omega_variable(
    purusha_sukta_literal_status,
    'Is the varna passage of Rigveda 10.90 (and cognate stratification verses) cosmogonic allegory, as this reading holds, or binding social legislation, as the orthodox reading holds?',
    'Stratified philological analysis of the samhita layers, comparative reception history, and study of how the verse functioned in ritual versus legal contexts across the transmission period.',
    'A literal-legislation verdict imports a victim set (persons assigned servile varna duties by the verse''s authority) into this constraint and pushes it toward tangled_rope or snare; an allegory verdict preserves the low-epsilon rope profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purusha_sukta_literal_status, empirical, 'Textual status of the varna verses: allegory or law.').

omega_variable(
    apologetic_shield_deployment,
    'When the reading is deployed to deflect contemporary caste critique (''caste is not in our scriptures''), does that deployment constitute a separate extractive arrangement with its own beneficiaries and harmed parties, rather than part of this constraint?',
    'Decompose into a second story if shield-deployment exhibits its own enforcement machinery, a distinct beneficiary seat, and identifiable parties who bear costs when the reading is used to close critique.',
    'This file remains a low-epsilon rope with no victim set; the shield-deployment story would carry victims, higher epsilon, and likely a tangled_rope or snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apologetic_shield_deployment, conceptual, 'Whether apologetic deployment of the reading is a separate constraint from the reading itself.').

omega_variable(
    diaspora_assent_vs_identity_pressure,
    'Does diaspora adherence to the reading reflect genuine doctrinal assent or identity-preservation pressure — that is, is the diaspora seat''s identity lock structural (family and community embeddedness) or internalized (self-concept fused with the egalitarian-heritage frame)?',
    'Retention and switching data on diaspora adherents who encounter orthodox framings of the same texts: if adherence survives exposure to alternatives at high rates under low social cost, the lock is internalized; if adherence tracks community density, it is structural.',
    'Affects the diaspora seat''s directionality placement and whether the measured coordination benefit is net or partly coerced; heavy internalization would raise the effective suppression the seat carries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_assent_vs_identity_pressure, empirical, 'Source of diaspora adherence: assent or identity pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__reformist_spiritual_reading, 1820, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_reform_reading_tr_t1820, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1820, 0.04).
narrative_ontology:measurement_basis(vedic_reform_reading_tr_t1820, observed).
narrative_ontology:measurement(vedic_reform_reading_tr_t1860, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1860, 0.05).
narrative_ontology:measurement_basis(vedic_reform_reading_tr_t1860, observed).
narrative_ontology:measurement(vedic_reform_reading_tr_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1900, 0.06).
narrative_ontology:measurement_basis(vedic_reform_reading_tr_t1900, observed).
narrative_ontology:measurement(vedic_reform_reading_tr_t1940, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1940, 0.08).
narrative_ontology:measurement_basis(vedic_reform_reading_tr_t1940, observed).
narrative_ontology:measurement(vedic_reform_reading_tr_t1980, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(vedic_reform_reading_tr_t1980, observed).
narrative_ontology:measurement(vedic_reform_reading_tr_t2020, vedic_corpus_social_prescription__reformist_spiritual_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement_basis(vedic_reform_reading_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(vedic_reform_reading_be_t1820, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1820, 0.06).
narrative_ontology:measurement_basis(vedic_reform_reading_be_t1820, observed).
narrative_ontology:measurement(vedic_reform_reading_be_t1860, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1860, 0.08).
narrative_ontology:measurement_basis(vedic_reform_reading_be_t1860, observed).
narrative_ontology:measurement(vedic_reform_reading_be_t1900, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1900, 0.09).
narrative_ontology:measurement_basis(vedic_reform_reading_be_t1900, observed).
narrative_ontology:measurement(vedic_reform_reading_be_t1940, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1940, 0.11).
narrative_ontology:measurement_basis(vedic_reform_reading_be_t1940, observed).
narrative_ontology:measurement(vedic_reform_reading_be_t1980, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement_basis(vedic_reform_reading_be_t1980, observed).
narrative_ontology:measurement(vedic_reform_reading_be_t2020, vedic_corpus_social_prescription__reformist_spiritual_reading, base_extractiveness, 2020, 0.14).
narrative_ontology:measurement_basis(vedic_reform_reading_be_t2020, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vedic_corpus_social_prescription__reformist_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__reformist_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__reformist_spiritual_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one colloquial label, three epsilon-invariant stories. The philological record is upstream of all three; historically the orthodox reading is upstream of this one (reformism defines itself against literalism), and the colonial reading reshaped the field both other readings respond to. Edges from this file point at both siblings; their files should carry reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
