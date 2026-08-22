% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_universalist_devotional, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Bhagavad Gita: Universalist Devotional Reading (Path-Independent Dharma)
 *   domain: religious/textual/ethical
 *
 * SUMMARY:
 *   The Bhagavad Gita is a 700-verse textual kernel of Hindu philosophy,
 *   embedded in the Mahabharata and presented as a dialogue between Prince
 *   Arjuna and Lord Krishna on the eve of the Kurukshetra war. The
 *   universalist devotional reading interprets the text as teaching that
 *   dharma (duty/righteousness) is not caste-bound social role performance,
 *   but rather surrender to divine will through bhakti (devotional love). On
 *   this reading, spiritual access is universally available regardless of
 *   caste, gender, or ritual status—a radical inversion of the orthodox
 *   Brahminical interpretation, which reads the same text as mandating
 *   caste-based duty and legitimating the violence of righteous war. This
 *   constraint story instantiates the universalist devotional reading as a
 *   single, ε-invariant constraint on textual authority and spiritual
 *   legitimacy. It is one of three sibling constraints (all readings of the
 *   same kernel text); the siblings are the orthodox_literal_reading and the
 *   gandhian_allegorical_reading. The three readings coexist as live
 *   interpretive traditions, each grounded in different hermeneutic
 *   strategies, different material histories, and different communities of
 *   practice.
 *
 * KEY AGENTS:
 *   - universalist_devotional_interpreters — scholars and teachers advancing the reading, benefit from its intellectual coherence and spiritual resonance
 *   - historically_excluded_groups — lower castes, women, ritual outsiders, spiritually empowered by the reading's universalism
 *   - orthodox_brahminical_authority — institutional gatekeepers whose monopoly on spiritual access is undermined by the reading
 *   - orthodox_literal_readers — conservative interpreters experiencing the reading as a suppression of textual truth
 *   - contemporary_devotional_practitioners — modern bhakti practitioners benefiting from the reading's authorization of their practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.31).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.18).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Bhagavad Gita: Universalist Devotional Reading (Path-Independent Dharma)").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious/textual/ethical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '67febbea-7cc5-4f49-8ac8-63d1e2fbe12e').
narrative_ontology:cs_kernel_codification('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', fixed_text).
narrative_ontology:cs_authority_grounding('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', lineage).
narrative_ontology:cs_interpretation_layer_present('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e').
narrative_ontology:cs_reading_relation('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', gita_kurukshetra_discourse__orthodox_literal_reading, coexists_with).
narrative_ontology:cs_reading_relation('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', foundational, caste_not_spiritual_barrier).
narrative_ontology:cs_axiom_status(caste_not_spiritual_barrier, holdable).
narrative_ontology:cs_axiom_grounding('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', caste_not_spiritual_barrier, deontological).
narrative_ontology:cs_axiom('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', foundational, dharma_is_devotional_surrender).
narrative_ontology:cs_axiom_status(dharma_is_devotional_surrender, holdable).
narrative_ontology:cs_axiom_grounding('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', dharma_is_devotional_surrender, deontological).
narrative_ontology:cs_reference_frame('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', universal_spiritual_equality).
narrative_ontology:cs_drift_state('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', contemporary_institutional_recognition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('67febbea-7cc5-4f49-8ac8-63d1e2fbe12e', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, contemporary_devotional_practitioners).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahminical_authority).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_readers).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, spiritual_equality_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, devotional_accessibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Textual scholars, philosophers, and spiritual teachers who read the Gita as teaching universal access to salvation through devotion (bhakti) independent of caste. They actively produce interpretive scholarship, teach this reading, and argue for its historical legitimacy in the text. They benefit from the reading's plausibility and intellectual standing. Their exit is arbitrage: they can publish, teach, or migrate interpretations across institutional and theological contexts.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universalist_devotional_interpreters, agenda_setter,
    organized, generational, arbitrage, global).

% Devotees from lower castes, women, and other groups historically barred from Vedic study and ritual by orthodox Brahminical gatekeeping. This reading affirms their spiritual legitimacy and access to salvation without requiring institutional mediation or caste-based hierarchies. Their benefits are spiritual legitimacy and dignity; their constraints remain social and economic.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, historically_excluded_groups, beneficiary,
    powerless, biographical, constrained, global).

% Brahminical priesthoods and textual establishment whose legitimacy and institutional authority rested partly on control of Vedic knowledge, caste-based ritual role hierarchy, and exclusive interpretive gatekeeping. This reading dissolves their monopoly on spiritual authority by making dharma accessible to all and redefining duty as devotional surrender rather than caste-bound role performance. Their identity as custodians of the sacred is undermined; their options are limited by institutional and theological commitments to the orthodox reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_brahminical_authority, payer,
    institutional, generational, identity_locked, regional).

% Conservative interpreters and scholars who read the Gita as textually mandating caste-based dharma and legitimating righteous violence (Kurukshetra war as paradigm). They experience the universalist reading as a rewriting that suppresses the text's literal content and removes the ethical framework their communities built on it. They possess institutional and theological resources to resist, but are constrained by the text's ambiguity and increasing access to competing interpretations.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_readers, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literal_readers, observer).

% Early European scholars and translators who adopted and promoted the universalist reading partly as a strategy to render Hindu philosophy compatible with European enlightenment values and critique of caste hierarchy. They frame themselves as neutral scholars but their advocacy amplifies the universalist reading's institutional reach.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, colonial_orientalist_scholars, observer,
    institutional, biographical, arbitrage, global).

% Modern practitioners of bhakti yoga and devotional paths who adopt this reading and build spiritual practice on it. They experience the constraint as enabling and liberating—it authorizes their devotional practice, validates their spiritual experience, and removes institutional gatekeeping from their path to the divine. They have moderate mobility: they can shift interpretations, seek teachers, or abandon institutional religion entirely.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, contemporary_devotional_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Political movements that invoke Gita authority for Hindu majoritarian identity but depend partly on the orthodox literal reading (caste hierarchy as religious cosmology, violence as dharmic righteous action). They are partially excluded from benefiting from the universalist reading because its egalitarian and non-violent reframing contradicts their political agenda. They would actively resist the universalist reading's institutional adoption.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, postcolonial_hindu_nationalism, excluded,
    organized, generational, constrained, national).

% The scholarly apparatus of comparative religion, philology, and hermeneutics that examines all competing readings and traces their genealogy. They document which reading is supported by which textual evidence, who advocates for it historically, and what material conditions enabled each reading's rise.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, analytical_textual_tradition, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the legitimacy problem for devotional practitioners outside the Brahminical priesthood: provides textual authorization for their spiritual practice, removes institutional gatekeeping, and affirms spiritual equality. Coordinates a universal devotee community around bhakti as a dharmic path independent of caste role or ritual hierarchy.
% TRANSFER_FUNCTION: Transfers spiritual authority and legitimacy from Brahminical institutional gatekeepers to universal devotional practitioners. Moves the definition of dharma from caste-bound role performance to individual devotional surrender. Moves access to salvation from restricted (Vedic study, ritual role) to universal (sincere devotion). The constraint itself does not transfer material wealth, but it redistributes social legitimacy and spiritual standing.
% ABSENT_VOICES: Orthodox literal readers and Brahminical institutional authorities are partially excluded: the reading develops against their opposition, and they would argue the universalist reading ignores textual passages mandating caste duty and righteous violence. Postcolonial Hindu nationalist movements are strategically excluded because the reading's egalitarianism and non-violence undermine their political project. What is gained by excluded parties' absence: the reading is not compromised by the orthodox framework or the political agenda that depends on caste hierarchy.
% DISAPPEARANCE_RATIONALE: If this reading disappeared—if the universalist devotional interpretation vanished from textual tradition and was replaced entirely by the orthodox literal reading—then historical outcomes would reorganize: institutional Brahminical authority over spiritual access would remain uncontested, mass devotional movements would lose textual legitimacy, the spiritual equality of lower castes would lack foundational authorization, and the Gita's ethical teaching would remain locked into caste-role definitions and war justification. Millions of practitioners would lose a text that affirms their spiritual dignity.
% FOUNDING_PROBLEM: Early devotional movements (particularly the Bhakti movement, 11th–18th centuries) generated mass spiritual practice outside Brahminical institutional control, and faced the problem of textual legitimacy: were practitioners' devotional paths authorized by the foundational text (the Gita), or were they deviations? The universalist reading solves this by re-reading the Gita itself as teaching that bhakti (devotion) is the true dharma, accessible to all regardless of caste, and that surrender to divine will supersedes caste-role performance.
% FOUNDING_PROBLEM_CORROBORATION: Textual scholars (Rammohan Roy, Vivekananda, Zaehner, Flood, and contemporary Sanskrit philologists) attest the Gita contains verses supporting the universalist reading (e.g., Bhagavad Gita 9.32: 'all are welcome to the path'). Mass devotional practitioners attest the reading's necessity for their spiritual legitimacy. Critical scholars outside both the orthodox and universalist traditions document the reading's emergence as a historical interpretive event tied to Bhakti movements and colonial encounters. Brahminical orthodox authorities dispute whether the universalist reading faithfully represents the text (they argue it suppresses context and caste-role passages), so the founding problem's resolution remains contested by the reading's opponents.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.31 at interval end) because the universalist devotional reading operates as genuine coordination solving a coordination problem (mass devotional practitioners need textual legitimacy). The reading is not suppressed: orthodox authorities oppose it, but they do not prevent its circulation or teaching—suppression remains low (0.18). Theater is minimal (0.12) because the reading does not require performative maintenance; it survives on interpretive plausibility and community practice. Accessibility collapse is very low (0.22): the reading leaves the orthodox alternative accessible to scholars and conservative communities; it does not foreclose competing interpretations. Resistance is high (0.71) because orthodox authorities and literalist readers actively resist the reading, argue against it textually, and maintain institutional pressure against its adoption. The measurement series shows extractiveness rising from near-zero in the early Bhakti period (when the reading was barely formalized) to its modern stable level (when it has achieved wide institutional recognition), while suppression and theater both fall as the reading becomes normalized and no longer requires defensive theoretical work. The grid spans roughly 2000 years from the earliest known Bhakti attestations to the present.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between seats is structural and fundamental. The universalist interpreters experience the constraint as solving a real problem (devotional practitioners need spiritual legitimacy) with minimal coercive overhead—a genuine rope. Orthodox Brahminical authority experiences the same constraint as erosion of their authority, undermining their gatekeeping function, and extracting their monopoly on spiritual access—from their seat it appears as snare-like extraction. Historically excluded groups experience it as removal of a barrier (a snare dissolving), not as a new constraint. This per-seat divergence is exactly what the engine should detect and report: the same textual reading is experienced differently by parties with opposite structural relationships to Brahminical institutional authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (universalist devotional interpreters, historically excluded groups) are credited low directionality (near 0.0 or negative effective extraction). The orthodox Brahminical authority, losing institutional monopoly and authority over spiritual access, is credited high directionality toward extraction (d near 1.0). Orthodox literal readers occupy middle ground: they lose interpretive market share but retain the ability to teach and publish, so d is moderate. No directionality override is needed; the beneficiary/victim declarations and exit options should derive the correct values.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist devotional reading does NOT exhibit mandatrophy. Its founding problem—mass devotional practitioners need textual legitimacy—remains live. The reading continues to serve this function; practitioners still rely on it, scholars still teach it, and its institutional standing has grown over time (not declined). Unlike a mandatroph, the reading has not outlived its function or become purely theatrical. The low theater_ratio (0.12) confirms this: the reading's operation is substantially functional, not performative. A mandatrophy reading of the Gita would look different—e.g., a ritualistic practice maintained despite loss of its original purpose. This reading is not that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_evidence_ambiguity,
    'Does the Gita text itself contain sufficient evidence for the universalist devotional reading, or is the reading a hermeneutic overlay that selectively emphasizes some verses while downplaying others?',
    'Systematic philological analysis of all Gita verses related to caste, dharma, and devotion; comparative examination of how each reading selects and contextualizes evidence; documentation of which verses are cited by each reading as foundational.',
    'If the universalist reading is textually justified, extractiveness and suppression are low (genuine coordination solving a legitimacy problem). If the reading is a selective hermeneutic overlay, extractiveness rises (it involves rewriting the text''s meaning against orthodox interpretation) and suppression rises (maintaining the reading requires active defense against literal reading).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_evidence_ambiguity, empirical, 'Whether the universalist reading is grounded in textual evidence or imposed by hermeneutic strategy.').

omega_variable(
    brahminical_authority_mechanism,
    'Is Brahminical institutional authority genuinely undermined by the universalist reading, or does the reading coexist with orthodox institutions without actually eroding their power?',
    'Historical examination of institutional access to Gita teaching, caste-based gatekeeping practices, and institutional responses to the universalist reading; documentation of whether Brahminical institutions lose material resources, followers, or authority-claims after the reading''s rise.',
    'If the reading genuinely undermines Brahminical authority, then orthodox authority is a legitimate target and extraction measure is justified (d near 1.0). If orthodox institutions coexist with the universalist reading without actual erosion, then the reading''s beneficiary status for excluded groups is higher (they benefit), but the extraction from orthodox authority is lower (they lose nothing real).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_authority_mechanism, empirical, 'Whether the universalist reading actually erodes Brahminical institutional power or merely offers an alternative alongside it.').

omega_variable(
    colonial_amplification_contamination,
    'To what extent did colonial-era European Orientalist scholars artificially amplify and promote the universalist devotional reading as a strategy to render Hindu philosophy compatible with European enlightenment values and to critique Hindu caste hierarchy?',
    'Genealogical analysis of the universalist reading''s rise during colonial period; documentation of which European scholars promoted it and why; comparison of indigenous pre-colonial Hindu devotional tradition''s interpretive practice with colonial-era reframing.',
    'If colonial amplification is substantial, the reading''s organic authority (ε from genuine coordination) is contaminated by external power projection, and suppression of the orthodox reading by colonial institutional force should be factored in. The reading would then carry both organic coordination (devotional community need) and colonial extraction (rewriting Hindu tradition to suit European frameworks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_amplification_contamination, empirical, 'Degree of colonial European institutional amplification of the universalist reading versus indigenous Hindu origin.').

omega_variable(
    spiritual_vs_social_emancipation_decoupling,
    'Does the universalist devotional reading''s provision of spiritual equality actually translate to social emancipation for historically excluded groups, or does it leave material caste-based oppression untouched while offering only spiritual dignity?',
    'Longitudinal social analysis of caste mobility, economic outcomes, and institutional access before and after adoption of the universalist reading; examination of whether historically excluded groups who adopt the reading experience social advancement or only spiritual affirmation.',
    'If spiritual dignity decouples from social emancipation, the reading''s benefits for excluded groups are real but limited to spiritual domain—extractiveness for them remains higher than measured (they still bear material caste costs). If spiritual affirmation catalyzes social movements that produce material changes, then the reading''s coordination function is deeper and extractiveness for excluded groups is more accurately measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spiritual_vs_social_emancipation_decoupling, empirical, 'Whether spiritual equality from the reading produces material social change or remains confined to spiritual domain.').

omega_variable(
    reading_identity_fusion_mechanism,
    'For contemporary practitioners who adopt the universalist devotional reading, is the reading constitutive of their identity as devotees (identity-locked exit), or is it a contingent interpretive choice they can modify or abandon?',
    'Ethnographic study of practitioner responses when the universalist reading is questioned or alternative readings are presented; examination of whether practitioners experience threat to identity or merely interpretive disagreement.',
    'If the reading is identity-locked for practitioners, their exit is constrained even if material constraints ease—they carry the reading''s framework with them. If the reading is a contingent choice, practitioners have higher exit freedom and the constraint operates with lower suppression. The engine derives exit_options from declared data, so this omega documents the ambiguity in what ''mobility'' means for spiritual practitioners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_fusion_mechanism, conceptual, 'Degree to which practitioners'' adoption of the universalist reading constitutes their spiritual identity versus a chosen interpretation.').

omega_variable(
    sibling_reading_foreclosure_hypothesis,
    'Is there a logical relationship in which the universalist devotional reading FORECLOSES the orthodox literal reading within a single framework, or do the two readings merely coexist as competing interpretations held by different parties?',
    'Formal analysis of the core premises: universalist (caste is not a spiritual barrier, dharma is devotion not role, violence is not central) vs. orthodox (caste is cosmological, dharma is role-based, righteous war is dharmic). If one premise directly contradicts the other such that no single coherent framework can hold both, foreclosure is true; if both can coexist in different frameworks or be reconciled by reading-level distinctions, they coexist.',
    'If foreclosure is true, then in mature intellectual discourse one reading should eliminate the other (the universalist should win in principled debate). If coexistence is true, then both readings should persist indefinitely. Current historical evidence shows coexistence, which suggests the readings do not logically foreclose each other but rather partition the interpretation space. Classification: coexists_with is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_hypothesis, conceptual, 'Logical relationship between universalist and orthodox reading axioms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(gita_tr_t0, projected).
narrative_ontology:measurement(gita_tr_t400, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 400, 0.07).
narrative_ontology:measurement_basis(gita_tr_t400, observed).
narrative_ontology:measurement(gita_tr_t800, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 800, 0.09).
narrative_ontology:measurement_basis(gita_tr_t800, observed).
narrative_ontology:measurement(gita_tr_t1200, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1200, 0.11).
narrative_ontology:measurement_basis(gita_tr_t1200, observed).
narrative_ontology:measurement(gita_tr_t1600, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement_basis(gita_tr_t1600, observed).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement_basis(gita_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(gita_be_t0, projected).
narrative_ontology:measurement(gita_be_t400, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 400, 0.15).
narrative_ontology:measurement_basis(gita_be_t400, observed).
narrative_ontology:measurement(gita_be_t800, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 800, 0.22).
narrative_ontology:measurement_basis(gita_be_t800, observed).
narrative_ontology:measurement(gita_be_t1200, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement_basis(gita_be_t1200, observed).
narrative_ontology:measurement(gita_be_t1600, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 1600, 0.31).
narrative_ontology:measurement_basis(gita_be_t1600, observed).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement_basis(gita_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gita_su_t0, projected).
narrative_ontology:measurement(gita_su_t400, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 400, 0.28).
narrative_ontology:measurement_basis(gita_su_t400, observed).
narrative_ontology:measurement(gita_su_t800, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 800, 0.24).
narrative_ontology:measurement_basis(gita_su_t800, observed).
narrative_ontology:measurement(gita_su_t1200, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1200, 0.2).
narrative_ontology:measurement_basis(gita_su_t1200, observed).
narrative_ontology:measurement(gita_su_t1600, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 1600, 0.18).
narrative_ontology:measurement_basis(gita_su_t1600, observed).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement_basis(gita_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__universalist_devotional_reading, 0.1).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel gita_kurukshetra_discourse. The orthogonal_literal_reading and gandhian_allegorical_reading are sibling constraints, each with their own ε values and stakeholder structures. The three readings coexist in Hindu intellectual tradition; they do not resolve to a single 'true' reading. Each story carries distinct beneficiary/victim sets and extraction measures. The network links connect them as members of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
