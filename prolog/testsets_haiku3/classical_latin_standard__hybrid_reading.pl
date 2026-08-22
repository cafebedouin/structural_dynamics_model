% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Classical Latin Standard (Hybrid Reading): Fidelity + Legitimate Domain Development
 *   domain: linguistic/institutional/commitment_systems
 *
 * SUMMARY:
 *   The hybrid reading of Classical Latin legitimizes textual fidelity to
 *   Classical norms while carving out exceptions for ecclesiastical and
 *   technical domains. This is a READING of a contested kernel—the Classical
 *   text corpus itself—that structures authority differently than its sibling
 *   readings (continuity: living transmission as legitimate; reconstruction:
 *   pure Classical recovery only). The hybrid reading embeds institutional
 *   gatekeeping: legitimate post-Classical developments must be justified by
 *   domain-functional necessity and approved by academic authorities, not by
 *   historical fact of use. This produces moderate extractiveness (partial
 *   delegitimization but with accommodation), moderate suppression (some
 *   forms legitimized, others rejected), and rising theater (the distinction
 *   between 'legitimate domain development' and 'barbarism' becomes
 *   increasingly performative as institutional authority is exercised to
 *   maintain boundaries).
 *
 * KEY AGENTS:
 *   - institutional_latinists: set and enforce the standard; benefit from gatekeeping authority
 *   - ecclesiastical_scholars: retain legitimized post-Classical ecclesiastical vocabulary but remain subordinate
 *   - technical_practitioners: retain domain-specific coinages but under institutional approval
 *   - medieval_linguistic_innovators: retroactively classified; some legitimized, others suppressed
 *   - non_classical_dialect_users: trapped by identity-lock; can only use Latin through institutional channels
 *   - reconstruction_reading_advocates: excluded, wanting pure Classical recovery
 *   - continuity_reading_advocates: excluded, wanting living transmission treated as legitimate
 *   - philological_academies: interpretive authority below the Classical-text kernel
 *   - analytical_observer: studies the constraint structure without participating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.52).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.58).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Classical Latin Standard (Hybrid Reading): Fidelity + Legitimate Domain Development").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "linguistic/institutional/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, 'e028c5f9-8f35-488f-bb28-7c6c8189e46e').
narrative_ontology:cs_kernel_codification('e028c5f9-8f35-488f-bb28-7c6c8189e46e', fixed_text).
narrative_ontology:cs_authority_grounding('e028c5f9-8f35-488f-bb28-7c6c8189e46e', extraction).
narrative_ontology:cs_interpretation_layer_present('e028c5f9-8f35-488f-bb28-7c6c8189e46e').
narrative_ontology:cs_reading_relation('e028c5f9-8f35-488f-bb28-7c6c8189e46e', classical_latin_standard__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e028c5f9-8f35-488f-bb28-7c6c8189e46e', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('e028c5f9-8f35-488f-bb28-7c6c8189e46e', foundational, classical_textual_normativity_with_institutional_exception).
narrative_ontology:cs_axiom_status(classical_textual_normativity_with_institutional_exception, holdable).
narrative_ontology:cs_axiom_grounding('e028c5f9-8f35-488f-bb28-7c6c8189e46e', classical_textual_normativity_with_institutional_exception, deontological).
narrative_ontology:cs_axiom('e028c5f9-8f35-488f-bb28-7c6c8189e46e', foundational, domain_specific_legitimacy_institutional_gatekeeping).
narrative_ontology:cs_axiom_status(domain_specific_legitimacy_institutional_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('e028c5f9-8f35-488f-bb28-7c6c8189e46e', domain_specific_legitimacy_institutional_gatekeeping, deontological).
narrative_ontology:cs_reference_frame('e028c5f9-8f35-488f-bb28-7c6c8189e46e', classical_textual_authority).
narrative_ontology:cs_drift_state('e028c5f9-8f35-488f-bb28-7c6c8189e46e', contemporary_academic_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e028c5f9-8f35-488f-bb28-7c6c8189e46e', '2026-06-11T14:23:47Z').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, institutional_latinists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, ecclesiastical_scholars).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, technical_practitioners).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, medieval_linguistic_innovators).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, non_classical_dialect_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, academies, and philological societies control the setting and enforcement of the Classical standard. They define what counts as 'correct' Latin and what counts as 'barbarism.' Under the hybrid reading, they have authority to carve out exceptions for ecclesiastical and technical domains but retain ultimate gatekeeping power. They benefit by maintaining institutional prestige, controlling certification of Latin competence, and deriving career advancement from being authorities on legitimacy. They are mobile in exit: if the hybrid standard failed, they could adopt another reading or abandon the field, though doing so would cost institutional status.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, institutional_latinists, agenda_setter,
    institutional, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, institutional_latinists, beneficiary).

% Priests, theologians, and church institutions use Latin in liturgy and theological discourse. The hybrid reading permits them to retain post-Classical ecclesiastical vocabulary (e.g., medieval liturgical Latin forms, Christian theological coinages) that would be rejected as barbarism under reconstruction reading. They benefit from institutional recognition of their domain-specific forms. But their exit is constrained: abandoning the hybrid standard would require either adopting pure reconstruction (losing their legitimate vocabulary) or continuity (losing institutional authority backing). They remain subordinate: their legitimacy is granted conditionally by institutional latinists, not self-evident.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, ecclesiastical_scholars, beneficiary,
    institutional, generational, constrained, universal).

% Scientists, physicians, and legal professionals use Latin terminology in technical contexts (binomial nomenclature, anatomical terminology, legal formulas, pharmaceutical Latin). The hybrid reading legitimizes their post-Classical technical coinages as domain-appropriate development. They benefit by having institutional backing for their technical vocabulary. But their exit is constrained: they depend on institutional authority for legitimacy, and their own technical communities may not have sufficient authority to defend their coinages without academic backing.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, technical_practitioners, beneficiary,
    moderate, biographical, constrained, global).

% A historical collective (medieval monks, scribes, administrators, scholars) whose linguistic innovations and natural developments are now retroactively evaluated. Forms they created without consciousness of violating a 'standard' are now classified as either 'legitimate domain development' (if ecclesiastical or technical) or 'barbarism' (if outside privileged domains). They have no voice in the classification. As a collective, they represent the suppressed alternative: the continuity reading that would validate their practice as natural, legitimate development. They are now assessed by institutional standards they could not have anticipated and largely cannot defend. Authoring this as non-agent: the collective is historical and cannot participate in contemporary standard-setting, but the constraint's enforcement retroactively delegitimizes their innovation.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, medieval_linguistic_innovators, payer,
    powerless, biographical, trapped, universal).
narrative_ontology:stakeholder_non_agent(classical_latin_standard__hybrid_reading, medieval_linguistic_innovators).

% Contemporary speakers and writers who use post-Classical Latin forms outside the legitimized ecclesiastical and technical domains. This includes those who might prefer or innovate using medieval or later developments in informal, literary, philosophical, or experimental contexts, and those who use Latin outside institutional channels (amateur Latin communities, revival movements, artistic practice). They are suppressed: their linguistic innovations are classified as barbarism, and they cannot gain institutional recognition without adopting Classical norms. Their exit is identity-locked: Latin is bound to their cultural, professional, or intellectual identity (amateur classicists, latinists with ideological commitments to living transmission, Latin revival communities), and abandoning the constraint means abandoning participation in Latin discourse entirely.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, non_classical_dialect_users, payer,
    powerless, immediate, identity_locked, universal).

% Philologists and classicists who advocate for the reconstruction reading (pure Classical recovery, rejection of all medieval drift, binary Classical/non-Classical distinction). They argue the hybrid reading is inconsistent and ad hoc: why allow ecclesiastical exceptions but not other forms? They want rigorous recovery of Classical Latin and see post-Classical forms as corruption. They have scholarly voice and institutional presence (are published, teach, advise students) but are epistemically marginalized by the hybrid framework's dominance in Latin pedagogy. Their exit is trapped: they are committed to Latin philology, but the field is dominated by the hybrid reading; leaving would mean abandoning their disciplinary identity.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, reconstruction_reading_advocates, excluded,
    powerful, generational, trapped, universal).

% Linguists, historical linguists, and scholars who advocate for the continuity reading (living transmission, legitimate drift, no privileged 'correct' form, language as natural evolving system). They argue Latin is a naturally evolving language and the hybrid reading's gatekeeping contradicts linguistic reality. They want Latin treated like any natural language without institutional authority privileging one historical stratum. They have academic presence (primarily in historical linguistics, sociolinguistics, critical theory) but are marginalized within Latin studies proper. Their exit is constrained: adopting the hybrid or reconstruction readings would require abandoning their linguistic principles; abandoning Latin entirely is possible but costly professionally.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, continuity_reading_advocates, excluded,
    moderate, generational, constrained, universal).

% Formal scholarly bodies (academies, university departments, editorial boards, dictionary/grammar projects) that certify Latin instruction, approve textbooks, adjudicate disputes over correctness, maintain reference standards (Oxford Latin Dictionary, Lewis & Short, major grammar references), and publish authoritative guidelines. Under the hybrid reading, these bodies operate as the interpretive authority below the Classical-text kernel. They continuously adjudicate whether new or contested forms count as legitimate domain development or barbarism. They exercise substantial institutional power in deciding legitimacy but are structurally constrained by the textual kernel and must justify every exception by reference to Classical principles or domain necessity.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, philological_academies, agenda_setter,
    institutional, generational, mobile, universal).

% Linguistic historians, sociolinguists, critical theorists, and institutional analysts who study the constraint itself: how the standard is enforced, who benefits, what forms of speech are suppressed, how legitimacy is constructed, and how authority is maintained. They do not participate in standard-setting or advocacy for particular readings; they analyze the structure.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, institutional_latinists).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared reference standard for Latinity that enables communication across domains (ecclesiastical, technical, literary, academic) and across time: speakers can recognize each other's utterances as 'Latin' and can read Classical texts with stable phonetic and grammatical norms. Without this coordination, Latin fragments into mutually incomprehensible regional and domain-specific dialects and becomes unusable as a lingua franca.
% TRANSFER_FUNCTION: Transfers institutional prestige and gatekeeping authority from the speaker/writer communities to institutional latinists and philological academies: to be heard as 'correct' requires submission to institutional standards and approval. Transfers linguistic agency: autonomous medieval and post-Classical innovations are reclassified retroactively as either sanctioned-domain developments or barbarisms, removing credit from the innovators themselves.
% ABSENT_VOICES: Pure continuity advocates (who would argue the standard is fabricated and legitimacy is democratic, not institutional) and pure reconstruction advocates (who would argue the exceptions are inconsistent) are both academically present but epistemically marginalized. Medieval speakers whose innovations are now classified as barbarism have no voice to defend their own linguistic creativity. Communities outside privileged domains (casual-discourse users, literary experimenters) are excluded from the conversation about what legitimate development looks like.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, Latin would either fragment into incompatible domain-specific and regional forms (no shared coordinate standard to anchor communication) or would require reconstruction of a new standard through deliberate collective choice. The institutional structure that maintains certification and authority would require reorganization. Ecclesiastical and technical practice would lose authoritative guidance on what forms count as legitimate within their domains.
% FOUNDING_PROBLEM: After the fall of Rome, Latin transmission fragmented: medieval scribal and ecclesiastical practices diverged from Classical usage; technical and administrative Latin developed new vocabulary for new concepts; different regions and communities developed different reflexes of Late Latin and Romance. By the Renaissance, no shared understanding existed of what 'correct' Latin was—Classical recovery, living transmission, or domain-specific elaboration? The constraint emerged to restore a unified standard that could serve as reference across domains and time while remaining practically usable.
% FOUNDING_PROBLEM_CORROBORATION: Institutional latinists and philological academies attest the founding problem as LIVE: fragmentation persists without institutional gatekeeping; natural drift continues to produce new forms that would dissolve the standard if not controlled. Continuity reading advocates attest the problem is SOLVED or MISCONSTRUED: linguistic practice shows no actual fragmentary incapacity; the 'problem' is institutional anxiety about control, not communication failure. Historical linguists document that medieval and early modern communities communicated effectively using their own norms without modern institutional standards.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the constraint partially legitimizes post-Classical forms—ecclesiastical and technical users are accommodated—but delegates legitimacy to institutional gatekeeping rather than accepting historical practice as self-justifying. The suppression (0.58) reflects active institutional enforcement: forms outside privileged domains are classified as barbarism and suppressed, while domain-specific forms are either approved or rejected based on criteria institutional latinists control. Theater (0.31) is rising: the distinction between 'legitimate development' and 'barbarism' increasingly appears arbitrary to external observers as institutional authorities fine-tune boundaries. Accessibility collapse (0.68) is moderately high because once the constraint's authority structure is understood, the alternatives (pure reconstruction, living transmission) appear as structurally different readings requiring abandonment of the institutional framework. Resistance (0.47) is moderate: continuity and reconstruction advocates resist actively, but they are marginalized epistemically within the academic mainstream; ecclesiastical and technical users accept the hybrid framework because it legitimizes their practice while maintaining institutional authority.
 *
 * PERSPECTIVAL GAP:
 *   The institutional latinists and philological academies perceive the hybrid reading as principled compromise: it maintains fidelity to the Classical text while remaining practically functional. Ecclesiastical scholars and technical practitioners perceive it as conditional tolerance: their forms are legitimized only if they justify them to institutional authorities and accept subordinate status. Continuity advocates perceive it as fabricated gatekeeping: the exceptions are arbitrary and undermine the reading's coherence. Reconstruction advocates perceive it as inconsistent: the exceptions corrupt the purity they see as essential to the standard. Non-classical dialect users perceive it as suppression: they are excluded from legitimacy unless they adopt a form they did not invent and must perform institutional deference to use it. The engine should compute different seat-level types from this asymmetric structure: the institutional seats compute as beneficiaries with moderate directionality (benefiting from gatekeeping, facing moderate resistance); the technical/ecclesiastical seats compute as constrained beneficiaries (accommodated but subordinate, d ~0.35-0.45); the non-classical users compute as targets (identity-locked, suppressed, d ~0.75-0.85).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional latinists (institutional power, mobile exit) are the primary beneficiaries: they control the standard, define legitimacy, and derive prestige and career advancement. Their directionality is low (d ~0.1-0.2: they benefit substantially and face weak exit barriers). Ecclesiastical scholars and technical practitioners (institutional/moderate power, constrained exit) are secondary beneficiaries: they retain legitimacy for their domain-specific forms but must accept institutional authority as sovereign. Their directionality is slightly beneficiary-leaning but constrained (d ~0.35-0.45: they benefit incidentally but cannot leave without losing institutional recognition). Non-classical dialect users (powerless, identity-locked exit) are the targets: they are suppressed, cannot exit without abandoning their linguistic identity, and must adopt the institutional standard to participate. Their directionality is near target (d ~0.75-0.85: they bear the constraint's cost and have no exit). Medieval linguistic innovators (retroactively assessed, powerless) are historical targets: their innovations are now suppressed as barbarism regardless of their intentionality. This asymmetry should generate seat divergence: the agenda-setter seats compute as rope (coordination justifying the constraint) while the target seats compute as snare (suppression with no genuine coordination benefit to them).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids mandatrophy by remaining reflexively accountable to the founding problem (fragmentation of Latin transmission). The founding_problem_status is CONTESTED: institutional latinists attest it as LIVE (fragmentation persists without institutional control), while continuity advocates attest it as SOLVED or MISCONSTRUED (linguistic communities communicated effectively without institutional standards). The hybrid reading resolves this via the interpretive layer: philological academies continuously adjudicate whether new forms count as legitimate domain development or barbarism, maintaining the appearance that the standard is re-earned against drift rather than simply imposed. However, the rising theater_ratio (from 0.12 to 0.31 over the interval) suggests mandatrophy beginning: the distinction between 'legitimate development' and 'barbarism' becomes increasingly performative as institutional authority is exercised to maintain boundaries despite evidence that the founding problem (actual fragmentation) does not justify continued suppression. The measurement trajectory shows extractiveness and suppression rising over the interval while the founding problem's urgency declines (academic Latin is less central to institutional communication over the 50-year period), which is precisely the mandatrophy signature: the constraint persists, with rising theater and extraction, even as its founding problem atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_legitimacy_boundary_ambiguity,
    'What objective principle distinguishes ''legitimate domain-specific development'' (approved) from ''barbarism'' (suppressed)? Is the distinction grounded in functional necessity of the domain, in historical use patterns, in institutional authority, or in a circular appeal to institutional judgment?',
    'Meta-analysis of institutional decisions: do approved domain developments follow a consistent principle (e.g., ''forms that have been used continuously in the domain for X years,'' ''forms that solve a communicative problem that Classical forms cannot''), or do approvals appear arbitrary and contingent on institutional preference?',
    'If the boundary is arbitrary or institutional-preference-dependent, the hybrid reading reduces to a snare with gatekeeping cover: the constraint''s persistence depends on suppressing alternatives and on institutional authority, not on the founding problem. If the boundary is principled and functionally grounded, the reading remains a tangled_rope with legitimate accommodation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_legitimacy_boundary_ambiguity, empirical, 'Whether domain legitimacy follows an objective principle or depends on institutional discretion').

omega_variable(
    founding_problem_persistence_question,
    'Does the founding problem (fragmentation of Latin transmission after Rome''s fall) persist as a live communicative crisis, or has the problem been solved such that the constraint now persists for institutional rent-seeking rather than necessity?',
    'Sociolinguistic observation of contemporary Latin use across domains: do speakers actually encounter communication failures due to lack of a unified standard, or do they communicate effectively within domain-specific and reading-specific norms? Longitudinal institutional records: is there documentary evidence that institutional constraint enforcement prevents fragmentation, or does the documentary record show institutional authority being exercised to suppress alternatives regardless of communication outcomes?',
    'If the founding problem persists as live, the constraint remains a tangled_rope (coordination + extraction). If the problem is solved, the constraint transitions to a snare (extraction without coordination benefit, or with coordination benefit that could be served by less extractive means) or a piton (persisting via inertia and institutional theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_question, empirical, 'Whether the founding problem remains live or has been solved').

omega_variable(
    reading_foreclosure_mechanism,
    'Are the three readings (continuity, reconstruction, hybrid) genuinely COEXISTENT (held by different parties, neither logically ruling out the other) or does the hybrid reading''s authority structure FORECLOSE the reconstruction and continuity readings by making their core premises incoherent within the institutional framework?',
    'Discourse analysis: can continuity and reconstruction advocates remain coherent if they accept the hybrid reading''s framework, or must they explicitly reject the Classical-text kernel or the institutional gatekeeping structure to maintain their positions? Historical analysis: have institutional authorities explicitly delegitimized the continuity and reconstruction readings by declaring them non-starters, or do they treat them as live alternatives that they happen to reject on grounds of principle?',
    'If the readings are coexistent, the kernel remains genuinely contested and multiple readings are structurally live. If the hybrid reading forecloses its siblings, the classification should include that foreclosure in the cs_structure.reading_relations (relation: forecloses), and the constraint''s authority structure should be understood as eliminating alternatives, not accommodating them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, conceptual, 'Whether the hybrid reading''s authority structure forecloses or merely defeats its sibling readings').

omega_variable(
    ecclesiastical_and_technical_subordination_extraction,
    'Is the subordinate status of ecclesiastical and technical domains (forms approved by institutional authority rather than self-evidently legitimate) a necessary feature of coordination or an extractive enforcement mechanism that concentrates authority?',
    'Counterfactual institutional design: could a coordinate standard be maintained if ecclesiastical and technical domains had co-authority over legitimacy in their own domains (not gatekept by institutional latinists)? Historical analysis: when ecclesiastical and technical users did have authority over their own domains (medieval and early modern periods), did Latin fragment communicatively, or did it remain usable across domains and time?',
    'If subordination is extractive, the constraint should compute as a snare for ecclesiastical/technical seats (they appear to benefit but actually bear extraction costs in exchange for token legitimacy). If subordination is necessary for coordination, the constraint remains tangled_rope for those seats (genuine coordination benefit justifies asymmetric authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_and_technical_subordination_extraction, conceptual, 'Whether ecclesiastical/technical subordination is necessary or extractive').

omega_variable(
    identity_lock_mechanism_for_non_classical_users,
    'Is the identity-lock of non-classical dialect users (inability to exit because Latin is bound to their professional/cultural identity) a structural feature of the constraint, or a condition these speakers import from their own relationship to Latin?',
    'Post-exit trajectory analysis: if non-classical users abandon the hybrid standard and adopt an alternative reading (continuity or reconstruction), do they lose professional standing, cultural identity, or access to resources, or are these losses imposed by institutions enforcing the hybrid standard? Ethnographic/interview data: do non-classical users articulate their relationship to the standard as externally enforced (suppression) or internally necessary (they need the standard to maintain their professional/cultural identity)?',
    'If the identity-lock is imposed by institutional enforcement, the constraint is more suppressive than authored (d shifts higher, suppression mechanisms are structural rather than self-imposed). If the identity-lock is self-imposed, the constraint remains at the authored suppression level (the constraint structures incentives; the speaker''s own commitments provide enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_non_classical_users, empirical, 'Whether identity-lock is structural or self-imposed in the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clas_tr_t0, classical_latin_standard__hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clas_tr_t10, classical_latin_standard__hybrid_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clas_tr_t20, classical_latin_standard__hybrid_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(clas_tr_t30, classical_latin_standard__hybrid_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(clas_tr_t40, classical_latin_standard__hybrid_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(clas_tr_t50, classical_latin_standard__hybrid_reading, theater_ratio, 50, 0.31).

% Extraction over time
narrative_ontology:measurement(clas_be_t0, classical_latin_standard__hybrid_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clas_be_t10, classical_latin_standard__hybrid_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(clas_be_t20, classical_latin_standard__hybrid_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(clas_be_t30, classical_latin_standard__hybrid_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(clas_be_t40, classical_latin_standard__hybrid_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(clas_be_t50, classical_latin_standard__hybrid_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(clas_su_t0, classical_latin_standard__hybrid_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clas_su_t10, classical_latin_standard__hybrid_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(clas_su_t20, classical_latin_standard__hybrid_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(clas_su_t30, classical_latin_standard__hybrid_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(clas_su_t40, classical_latin_standard__hybrid_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(clas_su_t50, classical_latin_standard__hybrid_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(classical_latin_standard__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, ecclesiastical_authority_linguistic_legitimacy).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, technical_latin_nomenclature_stability).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the contested kernel 'classical_latin_standard.' The kernel is the Classical Latin text corpus that different reading communities interpret through different authority structures. CONTINUITY_READING treats Latin as a living language with legitimate drift transmitted through unbroken practice; RECONSTRUCTION_READING treats Classical Latin as recoverable only through philological archaeology and rejects medieval developments; HYBRID_READING (this constraint) carves out institutional exceptions for ecclesiastical and technical domains while maintaining Classical textual fidelity as the default. The three readings have different extractiveness profiles: hybrid (this constraint, moderate) is positioned between continuity (low, minimal suppression) and reconstruction (high, binary rejection of drift). Each reading has its own authority structure, beneficiary set, and victim set. They are structurally related via network.affects_constraints rather than merged into one story, following the ε-invariance principle: each reading instantiates a distinct constraint with a distinct ε, different beneficiaries/victims, and different enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, powerless, 0.81).
constraint_indexing:directionality_override(classical_latin_standard__hybrid_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
