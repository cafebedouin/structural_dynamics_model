% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__subordinationist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__subordinationist, []).

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
 *   constraint_id: john_1_1_logos__subordinationist
 *   human_readable: Logos as Created/Subordinate Divine Agent (Subordinationist Reading)
 *   domain: theology/biblical-hermeneutics/christology
 *
 * SUMMARY:
 *   This constraint story models one reading of the contested kernel John
 *   1:1—specifically, the subordinationist reading in which the Logos is
 *   understood as a created being, the first and highest creation of the
 *   Father but not co-eternal, co-equal, or consubstantial with the Father.
 *   This reading was historically prominent in early Christianity (Arius,
 *   many Nicene-period communities), remains alive in some non-trinitarian
 *   traditions, and continues to challenge the orthodox consensus. The
 *   subordinationist reading operates as a tangled_rope: it genuinely solves
 *   a coordination problem (how to maintain strict monotheism while honoring
 *   the Logos's preeminence and role), but it extracts from high-church
 *   traditions by claiming that their authority (based on councils, creeds,
 *   and the full divinity of Christ) rests on a misreading of Scripture. The
 *   claim/metric independence is deliberate: subordinationist interpreters
 *   would claim this constraint is closer to rope (a genuine exegetical
 *   coordination framework); we author metrics suggesting tangled_rope or
 *   snare-flavored operation (moderate extraction, suppression, theater ratio
 *   growth over time) to model how the constraint actually functions in the
 *   competitive landscape of theological authority. The engine will compute
 *   different types from different seats; that divergence is exactly what
 *   this story is designed to measure.
 *
 * KEY AGENTS:
 *   - subordinationist_theology_tradition — institutional agenda-setter (organized, generational, constrained exit) — sets the exegetical standard and defends it
 *   - arian_communities — organized beneficiary (organized, biographical, identity-locked) — communities that identify with subordinationist Christology and are excluded from high-church structures
 *   - monarchian_interpreters — moderate beneficiaries (moderate, biographical, constrained) — individual scholars committed to strict monotheism frameworks
 *   - high_church_traditions — institutional payer (institutional, generational, constrained) — Catholic, Orthodox, Reformed traditions whose authority rests on full divinity claim
 *   - nicene_orthodoxy_adherents — powerful payers (powerful in aggregate, biographical/generational, identity-locked) — billions of Christians whose faith identity is organized around orthodoxy
 *   - exegetical_scholarship_community — observers (institutional, biographical, analytical) — scholars who produce evidence about the historical exegesis and textual arguments
 *   - institutional_hierarchy_enforcers — agenda-setters and payers (institutional, generational, constrained) — councils, magisterial bodies, seminaries that enforce orthodoxy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__subordinationist, 0.68).
domain_priors:suppression_score(john_1_1_logos__subordinationist, 0.72).
domain_priors:theater_ratio(john_1_1_logos__subordinationist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, extractiveness, 0.68).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(john_1_1_logos__subordinationist, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__subordinationist, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__subordinationist, "Logos as Created/Subordinate Divine Agent (Subordinationist Reading)").
narrative_ontology:topic_domain(john_1_1_logos__subordinationist, "theology/biblical-hermeneutics/christology").

domain_priors:requires_active_enforcement(john_1_1_logos__subordinationist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__subordinationist, 'b00cdf67-808b-46b3-86c1-d89f5bd9290c').
narrative_ontology:cs_kernel_codification('b00cdf67-808b-46b3-86c1-d89f5bd9290c', fixed_text).
narrative_ontology:cs_authority_grounding('b00cdf67-808b-46b3-86c1-d89f5bd9290c', lineage).
narrative_ontology:cs_interpretation_layer_present('b00cdf67-808b-46b3-86c1-d89f5bd9290c').
narrative_ontology:cs_reading_relation('b00cdf67-808b-46b3-86c1-d89f5bd9290c', john_1_1_logos__orthodox_christological, coexists_with).
narrative_ontology:cs_reading_relation('b00cdf67-808b-46b3-86c1-d89f5bd9290c', john_1_1_logos__non_incarnational_monotheist, coexists_with).
narrative_ontology:cs_axiom('b00cdf67-808b-46b3-86c1-d89f5bd9290c', foundational, logos_is_created_being).
narrative_ontology:cs_axiom_status(logos_is_created_being, holdable).
narrative_ontology:cs_axiom_grounding('b00cdf67-808b-46b3-86c1-d89f5bd9290c', logos_is_created_being, empirically_contingent).
narrative_ontology:cs_axiom('b00cdf67-808b-46b3-86c1-d89f5bd9290c', foundational, strict_monotheistic_coherence).
narrative_ontology:cs_axiom_status(strict_monotheistic_coherence, holdable).
narrative_ontology:cs_axiom_grounding('b00cdf67-808b-46b3-86c1-d89f5bd9290c', strict_monotheistic_coherence, deontological).
narrative_ontology:cs_reference_frame('b00cdf67-808b-46b3-86c1-d89f5bd9290c', logos_subordinate_to_father).
narrative_ontology:cs_drift_state('b00cdf67-808b-46b3-86c1-d89f5bd9290c', post_nicene_orthodoxy_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b00cdf67-808b-46b3-86c1-d89f5bd9290c', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__subordinationist, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, subordinationist_theology_tradition).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, arian_communities).
narrative_ontology:constraint_beneficiary(john_1_1_logos__subordinationist, monarchian_interpreters).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, high_church_traditions).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, nicene_orthodoxy_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, arian_communities).
narrative_ontology:constraint_victim(john_1_1_logos__subordinationist, institutional_hierarchy_enforcers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets John 1:1 and related passages to establish and maintain a reading where the Logos is the first and highest creation of the Father but not ontologically identical to or consubstantial with the Father. Sets exegetical standards, trains interpreters, and defends the interpretation against alternative readings through theological argumentation and textual scholarship. Collects authority and institutional prestige from maintaining this coherent interpretive framework.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, subordinationist_theology_tradition, agenda_setter,
    institutional, generational, constrained, global).

% Communities organized around subordinationist Christology who understand the Logos doctrine as foundational to their faith identity and institutional continuity. They benefit from a reading that preserves monotheistic consistency (no tritheism charge) while honoring the Logos's preeminence and role in creation. They also bear the cost of exclusion from high-church sacramental structures and institutional authority that treat the subordinationist reading as heresy.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, arian_communities, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, arian_communities, payer).

% Scholars and theologians committed to monarchian frameworks (e.g., modalism, strict monotheism) who use the subordinationist reading to defend the absolute uniqueness and monarchy of God the Father against trinitarianism. They benefit from a coherent exegetical and theological account that avoids multiplying divine natures. Exit involves abandoning their theological commitment structure.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, monarchian_interpreters, beneficiary,
    moderate, biographical, constrained, regional).

% Institutional churches (Catholic, Orthodox, Reformed, mainstream Protestant) whose authority, sacramental theology, and Christological doctrine rest on the claim that the Logos/Christ is fully divine, consubstantial with the Father, and the proper object of worship. The subordinationist reading poses a direct threat to their theological legitimacy and their claim to apostolic authority. They invest in suppressing or marginalizing the reading through councils, creeds (Nicaea, Constantinople), catechesis, and institutional enforcement.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, high_church_traditions, payer,
    institutional, generational, constrained, global).

% Billions of Christians across denominations whose faith identity is organized around orthodox Christology and the Nicene-Constantinopolitan affirmation of Christ's full divinity and consubstantiality with the Father. They are not direct payers in an economic sense but bear a doctrinal cost: the subordinationist reading challenges the christological foundation of their worship, sacramental theology, and institutional belonging. Exit involves a potentially devastating shift in faith identity.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, nicene_orthodoxy_adherents, payer,
    powerful, generational, identity_locked, global).

% Academic biblical scholars, historians of doctrine, and historians of religions who examine the textual evidence for John 1:1, the historical development of Christological doctrine, and the exegetical arguments across competing readings. They produce evidence and analysis that inform how different communities understand the Logos passage and evaluate the plausibility of subordinationist vs. orthodox interpretations.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, exegetical_scholarship_community, observer,
    institutional, biographical, analytical, global).

% Church councils, magisterial bodies, confessional standards, and educational institutions (seminaries, universities) that codify and enforce the orthodox reading of John 1:1 as doctrinally binding. They invest in suppression of the subordinationist reading through institutional authority, canonical enforcement, and control over who can teach and practice theology within their bounds. Exit would mean loss of doctrinal control and institutional coherence.
narrative_ontology:constraint_stakeholder(john_1_1_logos__subordinationist, institutional_hierarchy_enforcers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(john_1_1_logos__subordinationist, institutional_hierarchy_enforcers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__subordinationist, subordinationist_theology_tradition).
narrative_ontology:fixing_cost_class(john_1_1_logos__subordinationist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: John 1:1 explicitly states 'the Logos was with God and the Logos was God.' The subordinationist reading was built to solve the exegetical problem of how to understand this statement within strict monotheism: if the Logos is 'with God' (distinct), how can it also 'be God' without implying two equal gods? The subordinationist solution: the Logos IS divine (θεός, without article) but not the one God (ὁ θεός with article)—it is the first and highest created expression of divine power and wisdom, subordinate to the Father's ultimate authority.
% FOUNDING_PROBLEM_CORROBORATION: Patristic scholars (David Steenberg, Michel René Barnes, Rowan Williams) confirm the exegetical problem was real and engaged seriously by early church interpreters. Orthodox theologians confirm it was a genuine problem but argue Nicene orthodoxy was the correct solution, not subordinationism. Modern exegetical scholarship (Raymond Brown, C.K. Barrett, others) contests whether John 1:1 itself presents the logical problem or whether later systematization imposed it. No external corroboration from secular scholarship that the problem should be solved the subordinationist way; external corroboration only that the problem existed and was debated.
narrative_ontology:founding_problem_status(john_1_1_logos__subordinationist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__subordinationist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(john_1_1_logos__subordinationist, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__subordinationist, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__subordinationist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(john_1_1_logos__subordinationist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(john_1_1_logos__subordinationist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the subordinationist reading, if accepted, would transfer doctrinal authority away from the high-church traditions by claiming their foundational Christological claims are biblically unfounded. Suppression is higher (0.72) because the reading's persistence at all depends on actively defended exegetical work and communities—orthodoxy's institutional dominance (through councils, catechesis, denominational control) systematically suppresses subordinationist interpretation in most Christian contexts. Theater ratio is moderate (0.41) and rising, indicating that as subordinationist communities have become smaller and more marginalized, the maintenance of the reading increasingly involves performative exegesis (close textual reading that emphasizes the Logos's distinction from the Father, deliberate choices to translate προς as 'toward' rather than 'at', rhetorical emphasis on created-being language) rather than uncontested exegetical consensus. The measurement series track one shared time grid: extractiveness and suppression both rise gradually over the interval, suggesting that institutional entrenchment of orthodoxy has made the subordinationist reading progressively more extractive (harder to maintain, more identity-locking for adherents) and more actively suppressed (greater investment by high-church authorities in preventing it). Theater ratio rises more slowly, indicating the performative overhead is present but the reading hasn't yet devolved into pure theater.
 *
 * PERSPECTIVAL GAP:
 *   The subordinationist agenda-setter sees the constraint as rope—a genuine exegetical coordination framework that solves a real problem (monotheistic coherence, biblical fidelity). The high-church traditions see it as snare—a false and heretical exegesis sustained only by willful misreading and a marginalized community's identity investment. The engine will compute per-seat types that reflect these asymmetries: from the agenda-setter's seat, the constraint looks like coordination (moderate extraction, justified by the problem solved); from the payer's seat, it looks like enforced extraction (high suppression, no real alternative, identity-locked victims). The exegetical scholarship community (observer seat) sees both a genuine exegetical problem and genuine institutional politics shaping which reading dominates, and thus would compute a different type again—seeing the constraint as partially coordinate (real theological tension) and partially snare-like (institutional power suppresses the minority reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist tradition and Arian communities are beneficiaries (d low, toward 0): they receive institutional prestige, community identity, and theological coherence from this reading. High-church traditions and orthodox adherents are payers (d high, toward 1.0): they bear the threat to institutional authority, the cost of maintaining orthodoxy against a biblically grounded alternative, and the identity threat if the reading gains credibility. Monarchian interpreters are mixed: they benefit from the exegetical framework (low d) but are trapped in a constrained exit (identity-locked to strict monotheism), which moderates their beneficiary position slightly. Arian communities in particular are identity-locked: they cannot exit without abandoning their theological identity, their community belonging, and their understanding of salvation history. This makes their beneficiary status partially coercive—they benefit but cannot leave—a hallmark of tangled_rope dynamics. High-church adherents are also identity-locked (faith identity built around orthodoxy) but they are on the payer side, which makes their identity-lock a primary suppression mechanism: they stay in orthodoxy despite the exegetical challenge because their identity is fused with it. The exegetical scholarship community sits at d=0.5 (symmetric, analytical position, no structural benefit or cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The subordinationist reading was built to solve a genuine exegetical and theological problem: how to understand John 1:1 in the context of monotheism. The founding problem (parsing 'with God' and 'was God' without tritheism) is live and was contested throughout early Christianity. The constraint persists because (1) the exegetical problem is real and not fully resolved even in modern scholarship, and (2) communities organized around subordinationist theology continue to exist and maintain the reading. However, the founding problem's status is contested: high-church traditions argue that the exegetical problem is either fabricated (John 1:1 never implied tritheism) or was rightly solved by Nicene orthodoxy (the Logos is divine and coequal, not separate from God). From the high-church perspective, the founding problem is dead and the constraint persists as pure institutional politics. From the subordinationist perspective, the founding problem is live and the constraint persists as the correct solution. The theater_ratio growth (0.28 → 0.41) suggests that as subordinationism became a minority reading, the exegetical work required to sustain it has become increasingly performative—communities invest in close textual analysis and rhetorical emphasis not because the exegetical case is self-evident but because the institutional environment makes it necessary to maintain the reading at all. This is not full piton-level theater (the exegetical problem is genuinely contested; the reading is not purely theatrical), but it indicates incipient divergence between the real exegetical value of the reading and the institutional labor required to maintain it. Mandatrophy is not present yet—the founding problem is live enough to justify institutional investment—but the trajectory suggests potential vulnerability if the founding problem were resolved in favor of orthodoxy or if the reading's exegetical plausibility weakened further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exegetical_problem_reality,
    'Is the exegetical problem that subordinationism was built to solve (reconciling ''with God'' and ''was God'' in strict monotheism) a genuine logical tension that John''s text itself presents, or is it a later systematization imposed onto the text?',
    'Close historical exegesis of John 1:1 and parallel passages in context of Jewish monotheism and Second Temple theology. Linguistic analysis of Greek prepositions and the article. Study of earliest Christian interpretations (patristic commentary) to see whether the tension was recognized and debated immediately, or emerged later as trinitarian systematization developed.',
    'If the problem is genuine and pre-systematic, subordinationism is a legitimate exegetical option responding to a real textual tension. If the problem is imposed later, subordinationism is a constructed response to a false problem, weakening its claim to biblical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exegetical_problem_reality, empirical, 'Whether the exegetical problem is inherent to John 1:1 or constructed by later theological systems.').

omega_variable(
    canonical_status_divergence,
    'Does the subordinationist reading foreclose the orthodox reading, or do the two coexist as live options in the theological space?',
    'Logical analysis of the core premises. If subordinationism asserts ''the Logos is created'' and orthodoxy asserts ''the Logos is uncreated,'' can both be true in the same framework? (No—they logically foreclose each other.) Can the same community or party hold both? (Only if they occupy different interpretive frames.) Examination of how parties actually treat the alternative reading: as logically impossible, as heretical error, as a live but rejected option, or as an incommensurable frame.',
    'If subordinationism forecloses orthodoxy (or vice versa) within a single exegetical framework, the relation is forecloses, and the constraint is a straight binary contest. If they coexist as options held by different parties without logical resolution, the relation is coexists_with, and the constraint models a stable competitive landscape. If the reading influences but doesn''t foreclose the sibling (e.g., by raising the evidentiary bar for orthodoxy''s claims), the relation is influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(canonical_status_divergence, conceptual, 'Whether subordinationism logically eliminates or coexists with the orthodox reading.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression of the subordinationist reading primarily structural (institutional exclusion from seminaries, councils, confessional boundaries) or internalized (communities have absorbed the message that subordinationism is false and adopted orthodoxy as their own belief)?',
    'Post-institutional-suppression trajectory: if suppression removed (seminary teaches subordinationism equally, councils no longer condemn it, confessions become optional), would adherents maintain subordinationist belief and community identity, or would they revert to orthodoxy? If identities remain stable after structural suppression is removed, the suppression was primarily internalized. If identities shift, suppression is primarily structural.',
    'If suppression is structural, it is reversible; removing institutional barriers would restore the reading''s viability. If suppression is internalized, barriers alone will not restore it; the internalized rejection is the constraint''s staying power. An internalized suppression indicates the constraint has succeeded in fusing the victims'' identity with orthodoxy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether measured suppression reflects institutional barriers or internalized belief.').

omega_variable(
    kernel_reading_committer_frame,
    'Is this subordinationist constraint one reading of the textual kernel John 1:1, or is it a fundamentally different constraint (a theological doctrine) that happens to claim scriptural warrant?',
    'Definitional: does the subordinationist reading treat John 1:1 as the kernel (the fixed commitment that drives the interpretation), or does it treat subordinationist theology as the kernel (the fixed commitment that drives the interpretation of John 1:1)? The difference is directional: kernel-reading prioritizes explicating the text; theology-as-kernel prioritizes maintaining the doctrine and fits the text to it.',
    'If this is a kernel reading, it models one interpretation of a textual commitment shared across the Christian tradition. If this is theology-as-kernel, it models a separate doctrinal constraint that subordinationism enforces and John 1:1 is the warrant (not the kernel). The classification affects how the constraint relates to orthodoxy and non-incarnational monotheism: as alternative readings of the same text (kernel-reading) or as competing doctrines that appeal to the same passage (theology-as-kernel).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the constraint is a reading of the John 1:1 text or a theological doctrine that cites the text as warrant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__subordinationist, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t0, john_1_1_logos__subordinationist, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(john_tr_t0, observed).
narrative_ontology:measurement(john_tr_t5, john_1_1_logos__subordinationist, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(john_tr_t5, observed).
narrative_ontology:measurement(john_tr_t10, john_1_1_logos__subordinationist, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(john_tr_t10, observed).
narrative_ontology:measurement(john_tr_t15, john_1_1_logos__subordinationist, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(john_tr_t15, observed).
narrative_ontology:measurement(john_tr_t20, john_1_1_logos__subordinationist, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(john_tr_t20, observed).
narrative_ontology:measurement(john_tr_t25, john_1_1_logos__subordinationist, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(john_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(john_be_t0, john_1_1_logos__subordinationist, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(john_be_t0, observed).
narrative_ontology:measurement(john_be_t5, john_1_1_logos__subordinationist, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(john_be_t5, observed).
narrative_ontology:measurement(john_be_t10, john_1_1_logos__subordinationist, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(john_be_t10, observed).
narrative_ontology:measurement(john_be_t15, john_1_1_logos__subordinationist, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(john_be_t15, observed).
narrative_ontology:measurement(john_be_t20, john_1_1_logos__subordinationist, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(john_be_t20, observed).
narrative_ontology:measurement(john_be_t25, john_1_1_logos__subordinationist, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(john_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t0, john_1_1_logos__subordinationist, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(john_su_t0, observed).
narrative_ontology:measurement(john_su_t5, john_1_1_logos__subordinationist, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(john_su_t5, observed).
narrative_ontology:measurement(john_su_t10, john_1_1_logos__subordinationist, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(john_su_t10, observed).
narrative_ontology:measurement(john_su_t15, john_1_1_logos__subordinationist, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(john_su_t15, observed).
narrative_ontology:measurement(john_su_t20, john_1_1_logos__subordinationist, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(john_su_t20, observed).
narrative_ontology:measurement(john_su_t25, john_1_1_logos__subordinationist, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(john_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__subordinationist, identity_coordination).
narrative_ontology:boltzmann_floor_override(john_1_1_logos__subordinationist, 0.12).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__orthodox_christological).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, john_1_1_logos__non_incarnational_monotheist).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, nicene_creedal_authority).
narrative_ontology:affects_constraint(john_1_1_logos__subordinationist, christological_councils_enforcement).

% DUAL FORMULATION NOTE:
% The John 1:1 kernel decomposes into three constraint stories, one per major reading in the tradition. Subordinationist, Orthodox, and Non-incarnational interpretations are structurally distinct constraints with different ε values, beneficiary/victim structures, and types. Subordinationist (this file) extracts from orthodoxy by challenging its scriptural foundation. Orthodox challenges subordinationism by asserting full divinity. Non-incarnational challenges both by denying hypostatic distinctness. All three influence the others: they form a constrained competitive landscape where no reading has full institutional dominance outside its own communities, but orthodoxy has the highest organizational power. Network edges track this mutual influence and family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(john_1_1_logos__subordinationist, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
