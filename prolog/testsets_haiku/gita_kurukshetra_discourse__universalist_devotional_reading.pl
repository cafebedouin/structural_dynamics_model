% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita Universalist Devotional Reading: Path-Independent Salvation
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint is the universalist-devotional reading of the Bhagavad
 *   Gita kernel — a contested interpretation that recenters the text around
 *   bhakti (path-independent devotion) as the highest path to salvation,
 *   accessible to all persons regardless of caste, gender, or social role.
 *   The reading dissolves caste as a spiritual barrier by redefining dharma
 *   (duty/righteous action) as surrender to divine will rather than
 *   compliance with socially assigned role. This reading emerged as a
 *   powerful counter-narrative within the Hindu reform movements of the
 *   19th–20th centuries and continues as a live alternative to orthodox
 *   Brahminical interpretation. The constraint models the devotional
 *   framework itself as a coordination mechanism: it unifies diverse
 *   practitioners around a shared understanding of salvation that bypasses
 *   traditional gatekeeping. The extractiveness is modest (0.28) because the
 *   reading does not operate through coercive enforcement but through
 *   interpretive legitimacy and community adoption. Suppression is present
 *   (0.42) because orthodox institutions work to defend the caste-duty
 *   interpretation and restrict the authority of universalist readings, but
 *   declining over the interval as modern contexts increasingly reward
 *   egalitarian theological claims.
 *
 * KEY AGENTS:
 *   - Universal devotee class: all persons granted spiritual equality regardless of caste/gender by this reading
 *   - Historically excluded castes: beneficiaries of the caste-barrier dissolution
 *   - Women practitioners: gain direct access to sacred practice outside patriarchal ritual gatekeeping
 *   - Brahminical orthodox authority: institutional seat bearing the cost of interpretive redistribution
 *   - Orthodox literal interpreters: identity-locked agents whose professional self-concept depends on defending caste-duty reading
 *   - Reform movement advocates: organized beneficiaries and agenda-setters advancing the universalist reading
 *   - Contemporary academic interpreters: observer seat analyzing the hermeneutical contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.42).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita Universalist Devotional Reading: Path-Independent Salvation").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, 'a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6').
narrative_ontology:cs_kernel_codification('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', fixed_text).
narrative_ontology:cs_authority_grounding('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', lineage).
narrative_ontology:cs_interpretation_layer_present('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6').
narrative_ontology:cs_reading_relation('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', gita_kurukshetra_discourse__gandhian_allegorical_reading, influences).
narrative_ontology:cs_axiom('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', foundational, devotion_transcends_caste_and_role).
narrative_ontology:cs_axiom_status(devotion_transcends_caste_and_role, holdable).
narrative_ontology:cs_axiom_grounding('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', devotion_transcends_caste_and_role, deontological).
narrative_ontology:cs_axiom('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', foundational, divine_will_supersedes_social_duty).
narrative_ontology:cs_axiom_status(divine_will_supersedes_social_duty, holdable).
narrative_ontology:cs_axiom_grounding('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', divine_will_supersedes_social_duty, deontological).
narrative_ontology:cs_reference_frame('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', egalitarian_devotional_access).
narrative_ontology:cs_drift_state('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', contemporary_globalized_hinduism, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a0329d3b-f892-4b2d-8b2b-a55ec5f0a5d6', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_castes).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, women_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, reform_movement_advocates).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_orthodox_authority).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_interpreters).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, egalitarian_access_to_liberation).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, devotion_transcends_social_role).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, divine_will_supersedes_caste_duty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Any person, regardless of caste, gender, or social station, who seeks direct devotional relationship with the divine. This reading declares them eligible for salvation through bhakti (devoted surrender) independent of ritual status or occupational duty. They gain access to a coordinate reframing of dharma that places devotion above social role compliance.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    powerless, civilizational, mobile, global).

% Communities placed outside the ritual hierarchy by caste law. Under this reading, the text's message dissolves the spiritual barrier that orthodoxy uses to justify their exclusion from sacred knowledge and salvific paths. The reading provides scriptural warrant for claiming equality in the devotional domain.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_castes, beneficiary,
    powerless, civilizational, mobile, global).

% Historically barred from direct Vedic ritual and Sanskrit study under patriarchal interpretations. This reading's egalitarian devotional framework permits their direct engagement with sacred text and practice, bypassing male gatekeeping based on ritual purity rules.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, women_practitioners, beneficiary,
    powerless, civilizational, mobile, global).

% The institutional authority structure that historically monopolized interpretive authority over the Gita and defended caste-based dharma as divinely ordained. This reading redistributes authority to practitioners themselves and undermines the foundation that justified priestly gatekeeping. Institutional legitimacy depends on maintaining exclusive access to correct interpretation; this reading's adoption erodes that control.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_orthodox_authority, payer,
    institutional, civilizational, trapped, global).

% Scholars, priests, and institutional actors who hold the orthodox caste-duty reading as foundational to their worldview and professional identity. Adopting the universalist reading would require abandoning core interpretive claims they have defended, retraining professional lineages, and conceding institutional authority to competing readings. Their institutional and ideological identity is fused with the orthodox reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_interpreters, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_interpreters, agenda_setter).

% 19th- and 20th-century reform movements (Brahmo Samaj, Arya Samaj, neo-Vedantic movements) that adopted universalist readings to dismantle caste-based hierarchy and justify social egalitarianism from within Hindu textual tradition. They benefit from scriptural warrant for reform; they set the agenda for reinterpretation within their constituencies.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, reform_movement_advocates, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, reform_movement_advocates, agenda_setter).

% Scholars in religious studies, philosophy, and South Asian studies who analyze the Gita's multiple readings. They observe the hermeneutical contest between orthodox and universalist readings and document how textual authority is contested across communities. Their role is analytical witness to the interpretive process.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, contemporary_academic_interpreters, observer,
    organized, biographical, mobile, global).

% The orthodox literal reading and gandhian allegorical reading would reject the universalist reading's core premises. They are excluded from this story's framework not because they cannot coexist (the kernel permits multiple readings), but because their core commitments (dharmic duty via caste, internal battlefield metaphor) compete with universalist bhakti for interpretive authority over the same text.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, excluded_rival_readings, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying devotional framework for salvation that coordinates diverse practitioners (regardless of caste, gender, or social role) around a shared understanding of dharma as surrender to divine will rather than enforcement of social hierarchy. Solves the coordination problem: how can a text grounding caste-based hierarchy be read as teaching universal spiritual equality? The universalist reading answers: dharma-as-devotion (bhakti) is orthogonal to dharma-as-role (varna), and the text privileges the former.
% TRANSFER_FUNCTION: Transfers interpretive authority from Brahminical gatekeepers (who monopolized correct reading) to the community of devotees and reform-movement interpreters. Moves the text's legitimacy from enforcing caste hierarchy to supporting egalitarian spiritual access. Moves the definition of 'correct dharma' from social-role-compliance to devotional-surrender, redistributing what counts as a virtuous life.
% ABSENT_VOICES: Orthodox Brahminical authorities would object strenuously: they see the universalist reading as distorting the text's actual message and undermining the divinely ordained social order. Literal caste-supremacist readers are structurally excluded from this reading's framework. Gandhian interpreters, while sympathetic to universalism, would argue the reading misses the allegorical-violence point. None of these exclusions reflect their inability to speak; they reflect fundamental disagreement with the reading's core premises.
% DISAPPEARANCE_RATIONALE: Advocate seat: if this reading disappeared, egalitarian scriptural warrant for reform would weaken; caste-based hierarchy would lack a powerful counter-text within the Hindu tradition itself. Orthodox seat: if this reading disappeared, the Gita's true meaning (caste-duty, righteous war) would be restored. The parties contest whether the world 'rearranges' or 'stays the same' because they dispute what the text actually teaches. The reading exists because both camps claim the Gita proves their case.
% FOUNDING_PROBLEM: Early modern Hindu reform movements faced a dilemma: caste hierarchy was defended as divinely ordained by the Gita and broader Vedic corpus, yet reform advocates believed dharma could be reinterpreted to support egalitarianism. The founding problem: can the Gita itself be read as authorizing universal spiritual equality despite its apparent legitimation of caste-based duty? The universalist reading solves this by redirecting the text's focus from role-duty to devotional-surrender.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical reform movements (Brahmo Samaj, Arya Samaj founders), by contemporary Hindu egalitarian theologians (S. Radhakrishnan, A.C. Bhaktivedanta Swami Prabhupada's universalist framing), and by academic scholars documenting the interpretive history. Orthodox Brahminical authorities attest the problem differently: they deny the need for reinterpretation and argue the text correctly encodes caste duty. The corroboration that SUPPORTS the founding problem's live status comes from reform-movement founders' own writings documenting the pressure to reconcile sacred text with egalitarian conviction — outside the beneficiary seats, independent scholarly analysis confirms the hermeneutical contest is historically real.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, contested).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.28) reflects that the universalist reading operates as a genuine coordination mechanism (multiple communities sharing a unified devotional framework) with modest asymmetric extraction: the reading redistributes interpretive authority FROM Brahminical gatekeepers TO practitioners and reformers, so extraction flows not through coercive monopoly but through reframing what counts as authoritative interpretation. The reading is NOT a pure rope (which would have near-zero extraction) because it explicitly undermines the institutional authority of orthodox interpreters — they bear a real cost (loss of monopoly interpretive power, institutional prestige, gatekeeping authority). Suppression (0.42 at interval end) reflects ongoing institutional and cultural pressure from orthodox authorities to defend the caste-duty reading and restrict the reach of universalist reinterpretation, but declining suppression over the interval as modernization contexts reward egalitarian readings and reduce orthodox enforcement capacity. Theater ratio (0.15) is low because the devotional practice itself is substantively real — meditation, prayer, ethical devotion — not performative cover for extraction. The accessibility-collapse (0.35) is moderate: alternatives to the universalist reading remain intellectually and institutionally accessible (orthodox and gandhian readings persist as live options), but once the universalist frame is adopted, the practical accessibility of pre-reform gatekeeping narratives collapses for the devotee community.
 *
 * PERSPECTIVAL GAP:
 *   The brahminical orthodox authority and identity-locked orthodox interpreters sit at d ≈ 0.8 (targets of the reading's authority redistribution; they lose institutional power, gatekeeping control, and interpretive monopoly). The universal devotee class and historically excluded castes sit at d ≈ 0.0 (full beneficiaries — they gain spiritual equality, direct access to text, egalitarian salvific paths). Reform movement advocates sit near d ≈ 0.2 (asymmetric beneficiary: they set some agenda and benefit from the reading, but do not capture the reading's full extraction value; the devotee community benefits more purely). The engine computes these divergent seats from the structural data: beneficiary/victim declarations, power atoms, exit options (orthodox interpreters are identity-locked to the literal reading; devotees are mobile; reformers are organized). The reading that benefits powerless devotees and excludes powerful institutional gatekeepers should compute as high-solidarity coordination to the devotee seat and extractive authority redistribution to the orthodox seat — the same reading, different classifications from different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: universal devotee class, historically excluded castes, women practitioners — all powerless or moderate-power groups granted spiritual equality and direct access that was previously gatekept. Their exit options increase from constrained (under orthodox readings that restrict who is spiritually qualified) to mobile (under universalist reading that declares them inherently eligible). The reading redistributes from exclusion to inclusion. Victims/Payers: Brahminical orthodox authority (institutional, loses monopoly) and identity-locked orthodox interpreters (their professional self-concept and interpretive lineage are undermined by the universalist frame). The reading takes something from them — interpretive authority, institutional gatekeeping power, the foundation story for caste hierarchy. Vindicated propositions: egalitarian-access-to-liberation, devotion-transcends-social-role, divine-will-supersedes-caste-duty — these are not beneficiaries (they collect no rents), but they are what the reading proves true about the text. The constraint's directionality reflects that this is a genuine redistributive reframing: powerless agents gain access previously denied; powerful institutional actors lose exclusivity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: is the universalist reading's founding problem (reconciling sacred egalitarian principle with institutional caste hierarchy) still live, or has it been resolved/rendered obsolete? The founding_problem_status is declared 'live' because: (1) the hermeneutical contest between orthodox and universalist readings remains active in contemporary Hindu theology and scholarship; (2) reform-movement adoption of the universalist reading created real organizational and intellectual infrastructure that must be actively maintained; (3) caste hierarchies, while weakened by law and modernity, persist in practice in India and diaspora communities, sustaining pressure to find scriptural warrant for egalitarianism. Mandatrophy is NOT resolved because the problem persists. However, the measurement series show suppression DECLINING over the interval (from 0.65 at t=0 to 0.42 at t=100, projected to stabilize near 0.37-0.40): this indicates the suppressive force required to maintain the constraint is diminishing. This is consistent with a Rope type: the reading does real coordination work (unifying diverse practitioners) and extraction is modest and declining, not a maintained extractive cartel. The reading persists not because it is forced, but because it solves an ongoing hermeneutical and practical problem (egalitarian access) that communities continue to value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_intentionality_versus_interpretive_authority,
    'Does the Gita kernel actually teach path-independent devotion for all, or does the universalist reading project egalitarian modern values onto an ancient hierarchical text?',
    'Philological analysis of Sanskrit grammar, historical context of composition, and cross-textual corroboration from other Vedic/Puranic sources. Compare the textual frequency and emphasis of bhakti-passages vs. caste-duty passages; establish the historical audience and cultural assumptions of authorship.',
    'If the text''s actual teaching (by historical-intentionalist measures) is egalitarian devotion, the universalist reading is accurate recovery of original meaning; if the text genuinely encodes caste hierarchy, the universalist reading is reinterpretation justified by modern ethics but not by textual fidelity. This would change the reading''s epistemic status from ''true interpretation'' to ''ethically motivated reframing'', but does NOT change its coordinative function or extractive structure — the constraint persists either way.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_intentionality_versus_interpretive_authority, conceptual, 'Whether the universalist reading recovers original textual intent or imposes modern egalitarian values on a hierarchical text.').

omega_variable(
    identity_lock_versus_voluntary_adoption,
    'Is the orthodox interpreters'' resistance to the universalist reading a genuine commitment to textual truth, or an identity-fusion mechanism protecting professional/institutional position?',
    'Post-adoption behavioral analysis: when institutional incentives shift (e.g., reform governments in India, modern education systems favoring egalitarianism), do orthodox interpreters maintain the caste-duty reading or migrate toward universalist reinterpretation? Also: introspective testimony from interpreters who have crossed between readings about the subjective experience of identity-shift.',
    'If resistance is primarily identity-locked (professional and ideological fusion), the suppression_requirement decline measured over the interval reflects institutional pressure reducing, not the logical compellingness of arguments. The constraint would be best modeled as Tangled_Rope (genuine coordination + identity-locked extraction). If resistance is primarily epistemic conviction about true meaning, the decline reflects gradual persuasion, suggesting Rope classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_versus_voluntary_adoption, empirical, 'Whether orthodox interpreters'' commitment to their reading is epistemic conviction or identity-protection mechanism.').

omega_variable(
    suppression_mechanism_structural_versus_internalized,
    'The measured suppression (institutional pressure against universalist interpretations) — is it structural (institutional gatekeeping, restricted access to education and authority) or internalized (communities internalize exclusion narratives and self-police their interpretive authority)?',
    'Post-institutional-reform observation: if suppression is primarily structural, removing formal gatekeeping (opening seminaries, inclusive education, etc.) should show rapid suppression decline; if primarily internalized, suppression persists even after formal barriers fall, as practitioners carry exclusion narratives into their own practice.',
    'Structural suppression is more reversible with institutional reform; internalized suppression persists and may reconstitute itself after formal removal. This affects the prognosis: a structurally suppressed reading could plateau at much lower suppression (as measured suggests, stabilizing near 0.37-0.40); an internalized suppression might return to higher levels if communities adopt new identity-fusion mechanisms. The measurement series show declining suppression, consistent with structural pressure reducing, but an omega documents the risk of re-internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_versus_internalized, empirical, 'Whether suppression of the universalist reading operates through institutional barriers or through internalized self-policing.').

omega_variable(
    kernel_reading_versus_distinct_constraint,
    'Is the universalist devotional reading a genuine reading of the Gita kernel, or a new constraint altogether grafted onto the text for modern ethical purposes?',
    'Genealogical tracing: does the universalist reading claim continuity with any pre-modern textual tradition (Bhakti movement saints, Shankarite Vedanta schools, Tantric traditions)? Or is it an entirely modern creation projected backward onto ancient text? If continuity exists, it is a reading; if entirely modern, it may be a different constraint using the Gita as mere rhetorical authority.',
    'If reading: the constraint models hermeneutical redistribution within a single kernel, and the payer seat (orthodox authorities) is legitimate because the reading challenges their interpretive monopoly. If separate constraint: the universalist devotional framework may be better modeled as a NEW Rope (genuine coordination of practitioners around shared devotional commitment) that merely CLAIMS the Gita as warrant, not genuinely reading it. The CS structure would shift from single-kernel to multi-kernel network (Gita-literal-reading, universalist-devotional-framework as separate constraints linked by affects_constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_versus_distinct_constraint, conceptual, 'Whether the universalist reading has pre-modern genealogy or is entirely modern innovation claiming Gita authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gita_tr_t0, observed).
narrative_ontology:measurement(gita_tr_t33, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 33, 0.11).
narrative_ontology:measurement_basis(gita_tr_t33, observed).
narrative_ontology:measurement(gita_tr_t67, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 67, 0.14).
narrative_ontology:measurement_basis(gita_tr_t67, observed).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(gita_tr_t100, observed).
narrative_ontology:measurement(gita_tr_t133, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 133, 0.16).
narrative_ontology:measurement_basis(gita_tr_t133, projected).
narrative_ontology:measurement(gita_tr_t167, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 167, 0.15).
narrative_ontology:measurement_basis(gita_tr_t167, projected).
narrative_ontology:measurement(gita_tr_t200, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement_basis(gita_tr_t200, projected).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(gita_be_t0, observed).
narrative_ontology:measurement(gita_be_t33, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 33, 0.18).
narrative_ontology:measurement_basis(gita_be_t33, observed).
narrative_ontology:measurement(gita_be_t67, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 67, 0.25).
narrative_ontology:measurement_basis(gita_be_t67, observed).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 100, 0.28).
narrative_ontology:measurement_basis(gita_be_t100, observed).
narrative_ontology:measurement(gita_be_t133, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 133, 0.3).
narrative_ontology:measurement_basis(gita_be_t133, projected).
narrative_ontology:measurement(gita_be_t167, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 167, 0.29).
narrative_ontology:measurement_basis(gita_be_t167, projected).
narrative_ontology:measurement(gita_be_t200, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement_basis(gita_be_t200, projected).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(gita_su_t0, observed).
narrative_ontology:measurement(gita_su_t33, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 33, 0.58).
narrative_ontology:measurement_basis(gita_su_t33, observed).
narrative_ontology:measurement(gita_su_t67, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 67, 0.48).
narrative_ontology:measurement_basis(gita_su_t67, observed).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 100, 0.42).
narrative_ontology:measurement_basis(gita_su_t100, observed).
narrative_ontology:measurement(gita_su_t133, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 133, 0.38).
narrative_ontology:measurement_basis(gita_su_t133, projected).
narrative_ontology:measurement(gita_su_t167, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 167, 0.37).
narrative_ontology:measurement_basis(gita_su_t167, projected).
narrative_ontology:measurement(gita_su_t200, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 200, 0.4).
narrative_ontology:measurement_basis(gita_su_t200, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.12).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% The Gita_Kurukshetra_Discourse kernel admits multiple structurally distinct readings: the universalist_devotional_reading (this story, ε≈0.28, Rope, egalitarian) directly contests the orthodox_literal_reading (ε ≈ 0.65+, Tangled_Rope, caste-enforcing) and influences the gandhian_allegorical_reading (ε≈0.35-0.40, negotiated Rope, violence-metaphor). These are not observables of a single constraint; they are three distinct constraint stories that share a common kernel text but instantiate different structural relationships (extraction, coordination, authority) from different readings of what the text teaches. The universalist reading redistributes authority FROM orthodox gatekeepers TO devotee practitioners; the orthodox reading concentrates authority in caste-based institutional hierarchy; the gandhian reading splits authority between internal-struggle metaphor and modern non-violent ethics. Each story has its own ε (fixed by reading, not by measurement choice), its own beneficiary/victim structure, its own type classification. The network links them by affects_constraints because each reading's institutional success directly pressures the others: as universities and reform movements adopt universalist interpretation, orthodox institutions lose interpretive market share; as gandhian allegorism gains cultural prestige for non-violence, both universalist and orthodox readings must accommodate the violence question. The family relationship is not observational disagreement but genuine constraint-structure contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gita_kurukshetra_discourse__universalist_devotional_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
