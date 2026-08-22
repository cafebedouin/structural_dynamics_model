% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV-Only Exclusive Inspiration Doctrine
 *   domain: religious/theological
 *
 * SUMMARY:
 *   The KJV-Only exclusive-inspiration arrangement holds that the 1611 King
 *   James Bible is the only inspired and inerrant English Scripture and that
 *   every later translation is corrupted or inferior. Through one structure
 *   it performs two things at once: it coordinates a real community function
 *   — a single fixed English text unifying worship, memorization, preaching,
 *   and cross-generational identity — and it moves interpretive authority,
 *   money, and status upward: KJV-Only leadership becomes the sole arbiter of
 *   what counts as true Scripture, defense-literature ministries monetize the
 *   corruption narrative, and modern translations, their translators, and
 *   members who prefer them are suppressed as illegitimate or spiritually
 *   dangerous. The arrangement requires active enforcement (pulpit rulings,
 *   church discipline, seminary control, publishing gatekeeping) because the
 *   alternatives are physically ubiquitous — free online, in every bookstore
 *   — and must be collapsed rhetorically and socially rather than materially.
 *   This story instantiates the exclusive_inspiration_reading of the
 *   kjv_text_1611 kernel; the sibling readings are separate constraints with
 *   their own files, victim sets, and enforcement profiles (see
 *   kernel_context). Claim and metrics are independent: claimed_type is
 *   tangled_rope because a genuine coordination function and asymmetric
 *   extraction operate through the same enforced structure; the metrics
 *   describe the arrangement's actual operation as the historical record
 *   shows it.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - kjv_only_leadership: agenda-setter (institutional / identity_locked) — administers textual orthodoxy, collects authority and the revenue that flows through its institutions
 *   - kjv_only_publishing_ministries: beneficiary (organized / mobile) — monetizes the corruption narrative through defense literature, seminars, and media
 *   - congregants_in_kjv_only_churches: primary payer with incidental beneficiary position (powerless / constrained) — bears comprehension, family-fracture, and choice costs; receives cohesion and a settled answer
 *   - pastors_using_modern_versions: payer (moderate / constrained) — disciplined, denounced, and removed for version choice
 *   - seminary_students_in_kjv_only_colleges: payer (powerless / constrained) — tuition and credentials foreclosed from mainstream academic paths
 *   - modern_bible_translators: excluded and payer (institutional / mobile) — condemned without a seat in the conversation; the enforcement object
 *   - academic_religion_scholars: analytical observer (institutional / analytical) — documents the arrangement from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.74).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.78).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV-Only Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/theological").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '0be854da-4675-4813-8ca5-0879f871dc1e').
narrative_ontology:cs_kernel_codification('0be854da-4675-4813-8ca5-0879f871dc1e', fixed_text).
narrative_ontology:cs_authority_grounding('0be854da-4675-4813-8ca5-0879f871dc1e', lineage).
narrative_ontology:cs_interpretation_layer_present('0be854da-4675-4813-8ca5-0879f871dc1e').
narrative_ontology:cs_reading_relation('0be854da-4675-4813-8ca5-0879f871dc1e', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('0be854da-4675-4813-8ca5-0879f871dc1e', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('0be854da-4675-4813-8ca5-0879f871dc1e', foundational, kjv_exclusive_verbal_inspiration).
narrative_ontology:cs_axiom_status(kjv_exclusive_verbal_inspiration, holdable).
narrative_ontology:cs_axiom_grounding('0be854da-4675-4813-8ca5-0879f871dc1e', kjv_exclusive_verbal_inspiration, theological).
narrative_ontology:cs_axiom('0be854da-4675-4813-8ca5-0879f871dc1e', foundational, post_1611_textual_evidence_corrupt).
narrative_ontology:cs_axiom_status(post_1611_textual_evidence_corrupt, holdable).
narrative_ontology:cs_axiom_grounding('0be854da-4675-4813-8ca5-0879f871dc1e', post_1611_textual_evidence_corrupt, empirically_contingent).
narrative_ontology:cs_reference_frame('0be854da-4675-4813-8ca5-0879f871dc1e', perfect_preservation_in_kjv_1611).
narrative_ontology:cs_drift_state('0be854da-4675-4813-8ca5-0879f871dc1e', contemporary_textual_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0be854da-4675-4813-8ca5-0879f871dc1e', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_ministries).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, congregants_in_kjv_only_churches).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, pastors_using_modern_versions).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, seminary_students_in_kjv_only_colleges).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_bible_translators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, congregants_in_kjv_only_churches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, Bible-college presidents, radio teachers, and conference speakers who define which English text counts as Scripture for their networks. They select the texts read from the pulpit, approve or reject curriculum, decide which teachers remain in fellowship, and denounce versions and scholars from the platform. Their standing, income, and institutional roles depend on the exclusive-inspiration position remaining the settled answer; publicly revising it would cost them their platforms, their institutions, and their standing in every network they belong to.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% Produce and sell books, videos, comparison charts, and seminar series arguing that modern translations are corrupted — manuscript-history primers, conspiracy exposes, version-comparison guides. Revenue comes from the continuing need to defend the position; if modern translations were admitted as legitimate, the defense-literature market would largely disappear. The apparatus could in principle pivot to other products, but its brand and backlist are bound to the corruption narrative.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_ministries, beneficiary,
    organized, biographical, mobile, global).

% Attend churches where only the KJV may be read publicly or privately. They receive liturgical unity, shared memorization, dense community bonds, and a settled answer to an anxiety most had no tools to resolve themselves. They bear archaic-language comprehension costs, are told that relatives reading modern versions are handling a corrupted text, and face social and spiritual consequences — fractured families, lost fellowship, questions about their own faith — if they switch versions or leave. Leaving means leaving church, community, and often family at once.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, congregants_in_kjv_only_churches, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, congregants_in_kjv_only_churches, beneficiary).

% Pastors within or adjacent to KJV-Only networks who conclude from study that modern translations are more accurate and begin using or recommending them. They face pulpit discipline, denunciation from peers, loss of speaking invitations and camp-meeting circuits, and in many cases removal from their positions. Their ordination networks, references, and livelihoods sit inside the movement they would have to exit, and the exit itself is read as spiritual fall rather than scholarly revision.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, pastors_using_modern_versions, payer,
    moderate, biographical, constrained, national).

% Students at unaccredited Bible colleges that teach Textus Receptus apologetics as settled fact and treat textual criticism as a hostile discipline. Tuition and years are invested in credentials that mainstream institutions do not recognize; the training forecloses paths into academic biblical studies and leaves graduates dependent on the movement's own churches, colleges, and ministries for employment.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, seminary_students_in_kjv_only_colleges, payer,
    powerless, immediate, constrained, national).

% Translation committees and textual scholars whose work the arrangement declares corrupted, conspiratorial, or Satanic. They publish in academic venues the movement does not read, and their expertise is dismissed a priori inside it; they bear reputational attack — accusations of New Age influence, heresy, and deliberate corruption — without any seat in the conversation that condemns them. Their institutions and careers are outside the arrangement, so they are not trapped by it, but they are its enforcement object.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_bible_translators, excluded,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, modern_bible_translators, payer).

% Historians and sociologists of religion who study the movement from outside. They document its 20th-century origins, its institutional structure, its publishing economy, and its relationship to broader fundamentalist history. They neither collect from nor bear costs under the arrangement, and their analyses are typically dismissed by the movement itself.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, academic_religion_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single fixed English text unifies public worship, memorization, preaching, cross-referencing, and cross-generational transmission; a frozen canonical answer removes the need for each congregation and each generation to adjudicate among competing versions, and gives the community a stable identity boundary.
% TRANSFER_FUNCTION: Moves interpretive authority — the right to say what 'true Scripture' is — from all readers and scholars to KJV-Only leadership; moves money from congregants to defense-literature publishers, colleges, and radio ministries through book sales, tuition, and donations; moves status from modern translators (branded corruptors) to KJV-Only defenders (branded faithful).
% ABSENT_VOICES: Modern translation committees and academic textual critics are structurally absent: their testimony is dismissed a priori as the work of the corruptors, so the unanimity inside KJV-Only circles arises partly because the people equipped to contest the corruption narrative are excluded from the room by the narrative itself. Members who prefer modern versions are present but silenced by discipline; their objections surface only as exits.
% DISAPPEARANCE_RATIONALE: If the exclusive-inspiration claim vanished overnight, KJV-Only churches would have to adjudicate version choice on ordinary textual and pastoral grounds; the defense-literature market would collapse; leadership authority structures built on being the arbiter of textual purity would dissolve or reorganize; congregants would access modern translations without spiritual jeopardy, and many families currently split over version choice would re-integrate; the KJV itself would retain its place as a classic and liturgically beloved translation among several rather than as the only true one. The arrangement, not the text, holds these positions in place.
% FOUNDING_PROBLEM: The textual instability crisis of the late 19th and early 20th centuries: critical Greek editions (Westcott-Hort 1881 and successors) displaced the Textus Receptus underlying the KJV, and new English revisions (RV 1885, ASV 1901) fragmented the previously unified English Bible. Congregations faced a genuine and unsettling question — which English Bible is the word of God? The KJV-Only answer froze it: the 1611 text, already in place and already loved, is the final preserved form, and the instability is resolved by declaring one side of it corrupted.
% FOUNDING_PROBLEM_CORROBORATION: Church historians and textual critics outside the movement corroborate the founding anxiety as historically real while dating the exclusive-inspiration doctrine itself to 20th-century sectarian literature (Wilkinson 1930; the Ruckman-era systematizations from the 1960s; Riplinger 1993) rather than to any continuous doctrine of the church — the standard accounts of the Revised Version and of fundamentalist controversies are the corroborating sources. No source outside the benefiting parties attests that the founding problem remains live in the form the arrangement claims; that attestation comes only from KJV-Only leadership itself, whose axiom makes every rival answer definitionally corrupt.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74) because the exclusivity claim decouples access to 'true Scripture' from any textual merit of the alternatives: the gate, not the text, is the product, and the gate is owned by the leadership. Suppression (0.78) is structural — pulpit enforcement, church discipline, seminary control, publishing gatekeeping, family-fracture costs of exit — with a probable internalized component carried by members (see the suppression_mechanism_ambiguity omega). Theater_ratio (0.48): a large and growing share of the movement's scholarly output is performative — circular argumentation from the conclusion, conspiracy typologies, acrostic and numerical 'evidence' — while the underlying coordination work (shared liturgy, memorization, boundary maintenance) is real, keeping the ratio below the level at which the arrangement would be mostly performance. Accessibility_collapse (0.38) is low: alternatives are materially ubiquitous and free; the arrangement collapses them rhetorically and socially, not physically. Resistance (0.62) is substantial and organized outside the movement — the KJV-Only controversy literature, denominational and seminary repudiations, and the steady attrition of pastors and members who exit — but weak inside it. Coalition note: the payer seats are numerous but atomized; congregants face discipline individually and lack coordination infrastructure, so their aggregate latent power does not convert into internal resistance. Measurements share one grid (t = 0, 19, 38, 57, 76, 95 over 1930–2025): extractiveness and suppression rise monotonically as the movement institutionalizes (Wilkinson 1930, the Ruckman-era systematization, the Riplinger-era mass-market apologetics, internet-era persistence), theater rises as the scholarly apparatus becomes increasingly performative, and no cyclical pattern is claimed — the trajectory is an enforcement ratchet, not an oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as the defense of God's preserved words — from inside, enforcement is faithfulness, not coercion, and the exclusion of rival scholarship is discernment. The payer seats experience the same structure as a closed gate: comprehension costs, severed family ties, foreclosed livelihoods, and questions they are not permitted to ask. The excluded seat (modern translators) experiences it as condemnation without reply. The leadership seat is identity-locked by a fusion of two kinds: institutional (ministries, colleges, and publishing houses have become the position — their assets, staffs, and audiences exist only inside it) and ideological (within the movement's worldview, conceding a textual error concedes Satan's campaign, so revision is not a correction but a defeat). If that frame broke, the leadership seat's classification would change faster than the congregant seat's: leaders would face immediate platform loss, while members would experience relief of a long-carried anxiety. The engine computes these per-seat classifications from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership and publishing ministries sit near the beneficiary end: they collect authority and revenue and control enforcement, with the publishing revenue flowing through leadership-controlled institutions — which is why gain_flow names the leadership seat. Congregants are genuinely dual-positioned — real coordination benefits (community, liturgy, certainty) against real costs (comprehension, family fracture, suppressed choice) — placing them mid-to-target side, with constrained exit that keeps them from arbitraging away the costs. Disciplined pastors and indebted students sit near the full-target end with constrained exit: their livelihoods and credentials are inside the arrangement. Modern translators, though structurally outside the community, carry high directionality toward the arrangement because its suppression operates on them directly — they are the enforcement object, not incidental bystanders. No directionality overrides were needed: the beneficiary/victim declarations plus the exit-option profile produce the correct relationships for every seat. Suppression (0.78) is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled — by each agent's directionality toward the arrangement and by scope — in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two opposite mislabels. Reading the arrangement as pure snare would erase the real coordination function: a shared text genuinely unifies worship, memorization, and cross-generational transmission, and many participants are net beneficiaries by their own assessment — they would fight for the arrangement, which no pure snare's victims do. Reading it as pure rope would excuse the extraction as coordination cost: but the extraction flows from the exclusivity claim, not from text-sharing — version-tolerant congregations that love the KJV sustain equivalent cohesion without declaring all other translations corrupt, so the exclusivity layer is separable in principle (see the separability omega). The R5 interview supports the hybrid reading rather than a zombie reading: the founding problem (19th–20th century textual instability) was real and its residue is contested rather than dead, so the mismatch consumer should expect no dead-mandate flag; the arrangement persists not because its problem died but because its enforcement keeps the problem alive — every rival answer is definitionally corrupt, so the question can never be settled from inside.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates the exclusive_inspiration_reading of the kjv_text_1611 kernel; how much of the measured extraction is specific to the exclusivity claim rather than inherent to any commitment to a shared English text?',
    'Compile the sibling-reading stories (kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading) from the same kernel and compare computed per-seat extraction; the delta isolates the exclusivity claim''s contribution.',
    'If extraction is reading-specific, the kernel''s shared-text coordination is benign and the exclusivity claim is the extraction mechanism; if extraction persists across readings, the textual-authority structure itself carries it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Reading-specific versus kernel-inherent extraction across the KJV kernel family.').

omega_variable(
    doctrinal_sincerity_ambiguity,
    'Do the movement''s defenders sincerely hold the exclusive-inspiration doctrine, or does a gap exist between the leadership''s private assessment of the textual evidence and its public position?',
    'Published recantations and exit accounts of former KJV-Only leaders and scholars, compared against internal teaching materials and correspondence where available.',
    'A sincerity gap would shift the arrangement toward pure extraction with coordination as cover (snare-leaning); sincere belief keeps it a hybrid with identity-locked enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_sincerity_ambiguity, empirical, 'Sincerity of the exclusivity claim among its enforcers.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by congregants structural (church discipline, social cost of exit, institutional dependence) or internalized (genuine belief that reading modern versions is spiritually dangerous)?',
    'Post-exit trajectory of former members: if version anxiety and guilt persist after leaving the enforcement environment, a substantial internalized component is present.',
    'If internalized, effective suppression is higher than the structural measure and travels with the member after exit; the payer seat''s computed classification shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of version choice among members.').

omega_variable(
    shared_text_vs_exclusivity_separability,
    'Is the community-coordination function of a single shared English text separable from the exclusivity claim that all other translations are corrupt?',
    'Compare congregations that prefer the KJV for literary and liturgical reasons without holding the exclusivity doctrine: if they sustain equivalent cohesion and transmission, the functions are separable.',
    'If separable, the exclusivity claim is the extraction mechanism riding on genuine coordination and the tangled_rope reading is extraction-weighted; if inseparable in practice, part of the measured extraction is the price of the identity coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_text_vs_exclusivity_separability, conceptual, 'Whether shared-text coordination requires the exclusivity claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_exclusive_tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(kjv_exclusive_tr_t19, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 19, 0.22).
narrative_ontology:measurement(kjv_exclusive_tr_t38, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 38, 0.3).
narrative_ontology:measurement(kjv_exclusive_tr_t57, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 57, 0.38).
narrative_ontology:measurement(kjv_exclusive_tr_t76, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 76, 0.44).
narrative_ontology:measurement(kjv_exclusive_tr_t95, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 95, 0.48).

% Extraction over time
narrative_ontology:measurement(kjv_exclusive_be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kjv_exclusive_be_t19, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 19, 0.46).
narrative_ontology:measurement(kjv_exclusive_be_t38, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 38, 0.58).
narrative_ontology:measurement(kjv_exclusive_be_t57, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 57, 0.66).
narrative_ontology:measurement(kjv_exclusive_be_t76, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 76, 0.71).
narrative_ontology:measurement(kjv_exclusive_be_t95, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 95, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(kjv_exclusive_su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(kjv_exclusive_su_t19, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 19, 0.5).
narrative_ontology:measurement(kjv_exclusive_su_t38, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 38, 0.6).
narrative_ontology:measurement(kjv_exclusive_su_t57, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 57, 0.68).
narrative_ontology:measurement(kjv_exclusive_su_t76, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 76, 0.74).
narrative_ontology:measurement(kjv_exclusive_su_t95, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 95, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the KJV question' conflates three structurally distinct claims (per the epsilon-invariance principle): (1) the exclusive-inspiration claim (this story — high extraction, gate-keeping enforcement, modern translations and dissenting members as victims); (2) the revisable-translation claim (the KJV as improvable in light of better manuscripts — low extraction, scholarship-facing); (3) the functional-equivalence claim (complementary translations — low extraction, coordination-facing). Each gets its own epsilon, beneficiaries, victims, and enforcement profile. The family is linked because the upstream claim (the KJV's genuine literary and liturgical achievement, which no sibling disputes) is cited as evidence by the downstream exclusivity claim: the text's real excellence is the platform on which the exclusivity gate is built. This story's high epsilon comes from the exclusivity premise, not from the text's status — measuring the text's excellence instead of the exclusivity arrangement would be measuring a different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
