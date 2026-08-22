% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: Functional-Equivalence Reading of the KJV's Authority (Plural Translation Ecology)
 *   domain: religious_studies/textual_criticism
 *
 * SUMMARY:
 *   This story instantiates the functional-equivalence reading of the kernel
 *   kjv_text_1611: the arrangement in which multiple English Bible
 *   translations coexist, each serving distinct functions — the King James
 *   for literary cadence, public reading, and historical witness; modern
 *   versions for clarity in teaching, evangelism, and private study; critical
 *   editions beneath the scholarship. No single text holds gate-keeping
 *   power; authority is decentralized across the version ecology, and the
 *   price paid is coordination cost (cross-version friction) rather than
 *   extraction. Per the epsilon-invariance principle, the colloquial label
 *   'the authority of the KJV' decomposes into three structurally distinct
 *   claims — exclusive inspiration, functional equivalence (this file), and
 *   revisable translation — each with its own epsilon, beneficiary/victim
 *   structure, and classification; this story links both siblings via
 *   network.affects_constraints. Claim and metrics are independent authored
 *   facts: the claimed type is what this reading's structure appears to be
 *   from the authoring seat, and the metrics describe the arrangement's
 *   observed operation without being tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - lay_bible_readers: diffuse beneficiaries (moderate/mobile) — choose among versions per purpose; bear only purchase costs and minor cross-version friction
 *   - teaching_pastors: functional beneficiaries (moderate/mobile) — consume register diversity for exposition and congregational communication
 *   - literary_scholars: beneficiaries of the KJV's retained register (moderate/mobile) — depend on the text staying in print, cited, and taught
 *   - translation_publishers: commercial beneficiaries with agenda-setting reach (institutional/arbitrage) — fund, produce, and position editions; the seat monetary gains accrue to
 *   - denominational_liturgical_bodies: agenda-setters by ratification (institutional/constrained) — authorize version lists for official worship
 *   - kjv_only_advocates: excluded opponents (organized/identity_locked) — reject the plural arrangement on doctrinal grounds and sit outside its decision rooms
 *   - academic_textual_critics: analytical observers (analytical/analytical) — supply the manuscript base and critical editions every camp draws on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.17).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.07).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.17).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.07).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "Functional-Equivalence Reading of the KJV's Authority (Plural Translation Ecology)").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious_studies/textual_criticism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, '91b821c2-1c91-48a4-b15b-a1c6a5dff12f').
narrative_ontology:cs_kernel_codification('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', fixed_text).
narrative_ontology:cs_authority_grounding('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', practice).
narrative_ontology:cs_interpretation_layer_present('91b821c2-1c91-48a4-b15b-a1c6a5dff12f').
narrative_ontology:cs_reading_relation('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', foundational, translation_multiplicity_serves_communication).
narrative_ontology:cs_axiom_status(translation_multiplicity_serves_communication, holdable).
narrative_ontology:cs_axiom_grounding('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', translation_multiplicity_serves_communication, instrumental).
narrative_ontology:cs_axiom('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', secondary, functional_selection_norm).
narrative_ontology:cs_axiom_status(functional_selection_norm, holdable).
narrative_ontology:cs_axiom_grounding('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', functional_selection_norm, conventional).
narrative_ontology:cs_reference_frame('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', functional_pluralism_ecology).
narrative_ontology:cs_drift_state('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', contemporary_digital_distribution_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('91b821c2-1c91-48a4-b15b-a1c6a5dff12f', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, lay_bible_readers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, teaching_pastors).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, literary_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, translation_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, denominational_liturgical_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read scripture for devotion, study, and worship. They choose among dozens of English versions — an early-modern literary register for reading aloud or recalling familiar phrasing, contemporary wording for teaching children or first-time readers. Their costs are purchase prices and the occasional friction of following along when a congregation or study group uses a different version. Switching versions is a bookstore decision, not a rupture.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, lay_bible_readers, beneficiary,
    moderate, biographical, mobile, global).

% Preach and teach weekly and need phrasing their congregations understand. Most keep several versions at hand: a modern one for exposition, the King James for cadence, quotation, and congregational familiarity. Their professional standing rests on communicative effectiveness rather than on any particular version, and they move between texts freely as audiences change.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, teaching_pastors, beneficiary,
    moderate, biographical, mobile, regional).

% Study the King James as a monument of English prose and a witness to early modern theology and translation practice. Their work depends on the text remaining in print, cited, and taught; they hold no stake in its devotional exclusivity and routinely cite modern versions in adjacent research. The text is one archive among many they handle.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, literary_scholars, beneficiary,
    moderate, generational, mobile, global).

% Commission, produce, and market English Bible editions. Revenue arrives as per-copy sales, licensing fees, and edition lines (study Bibles, specialty formats). They decide which translation projects get funded and how editions are positioned — literal, readable, accurate — shaping the version landscape without controlling how churches actually use the texts. Product lines can be launched or retired at will.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, translation_publishers, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, translation_publishers, agenda_setter).

% Authorize which versions may be read in official worship and publish lectionaries and liturgical resources keyed to particular texts. Most mainline bodies authorize the King James alongside several modern versions, formally ratifying the plural arrangement. Tradition and canon law bound what they authorize, but they face no pressure to narrow the list.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, denominational_liturgical_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, denominational_liturgical_bodies, beneficiary).

% Hold that the King James is the only legitimate English Bible and organize to defend that position through publications, conferences, and congregational networks. They experience the plural version market as doctrinal erosion. Leaving the position would mean abandoning a community and an identity built around the text, so adherence persists across generations regardless of surrounding practice.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_only_advocates, excluded,
    organized, generational, identity_locked, global).

% Reconstruct the earliest attainable text of the scriptures from manuscript evidence and publish critical editions that translation committees draw on. Their work underpins modern versions and documents the King James's own sources alike. They take no side in which version congregations read.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, academic_textual_critics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__functional_equivalence_reading, translation_publishers).
narrative_ontology:fixing_cost_class(kjv_text_1611__functional_equivalence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches English renderings of the scriptures to the functions readers need them for: an early-modern literary register for inherited language and public reading, contemporary phrasing for instruction and evangelism, critically edited bases for scholarship. The division of labor is maintained by use rather than by any central registry — communities adopt the version that performs their task and set aside ones that do not.
% TRANSFER_FUNCTION: Moves modest per-copy payments and licensing fees from readers and institutions to publishers and translation committees. It also moves textual authority itself diffusely: prestige accrues to whichever version performs a given function well, rather than concentrating in a single authorized text, so attention and trust circulate across the version ecology instead of pooling in one place.
% ABSENT_VOICES: King-James-only advocates are organized and vocal but sit outside the rooms where lectionaries, seminary curricula, and translation projects are decided; they would object that plurality licenses corruption. Less visibly, lay readers without guidance bear the coordination cost of choosing among versions and rarely have a seat in decisions about which versions get produced at all.
% DISAPPEARANCE_RATIONALE: If the plural arrangement vanished overnight — every version but one out of print, the norm of complementarity collapsed — preaching, teaching, liturgy, and scholarship would all reorganize around whatever single text remained: publishers would lose their edition lines, pastors would lose register options, scholars would lose the comparative object of study, and the version dispute would end by fiat rather than persuasion.
% FOUNDING_PROBLEM: Early seventeenth-century England lacked a single authorized vernacular Bible: rival translations carried factional annotations, notably the Geneva Bible's Calvinist marginalia, and the church sought one text that episcopal and Puritan parties alike could read aloud without dispute.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the English Reformation trace the dissolution of the uniformity problem to the loss of any enforcement mechanism for a single text and, decisively, to denominational pluralism; sociologists of religious publishing document a multi-version market no actor seeks to consolidate. The Church of England's own authorization of multiple versions for official worship attests institutionally that no single text is required. No body outside King-James-only circles asserts the original uniformity requirement today.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__functional_equivalence_reading, 0.17, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.17 at interval end) because the arrangement extracts little: participation is voluntary, switching costs are a bookstore decision, and the monetary flows are ordinary commerce near the coordination floor for a resource-allocation mechanism. Suppression is very low (0.07) and authored as a raw structural property — nothing enforces pluralism coercively; if anything, suppression pressure runs the other way, from the excluded KJV-only seat against the arrangement. Accessibility collapse is low (0.15): understanding the arrangement multiplies alternatives rather than closing them. Resistance is moderate (0.38): the King-James-only controversy is a documented, organized counter-movement that actively contests pluralism, which is real friction even though it cannot coerce. Theater is low (0.16): version marketing has performative elements ('most accurate yet'), but the underlying functions — register provision, clarity, critical grounding — are genuinely performed. The temporal series run on one shared grid (t=0..140 in years, anchored at 1881, the Revised Version's breach of the KJV's effective monopoly, to 2021). Base extractiveness follows a rise-and-fall arc: the mid-century multiplication of copyrighted versions with competitive marketing pushed extraction to its peak around t=60-80 (roughly 1941-1981); digital distribution and open licensing then deflated it. The oscillation is driven by external publishing economics and technology, not by intermittent reinforcement — the cycle is a side effect of market structure, not an extraction mechanism. Base_properties values are measured at interval end (t=140). No suppression_requirement series is authored: enforcement capacity is static and negligible, which the scalar already captures.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From a KJV-only advocate's position, this same arrangement is not benign coordination but licensed corruption — they instantiate the sibling constraint, in which the version plurality is the harm. From the publisher seat, the ecology is a product portfolio with editorial risk. From the pastor's seat it is a toolkit; from the scholar's, a preserved archive; from the lay reader's, a shelf. The engine computes per-seat classifications from the structural data (power, exit, role); the authored claim speaks from the reading's own seat and does not adjudicate the divergence. The excluded seat matters most: because kjv_only_advocates are identity_locked, their dissent persists indefinitely without converting into exit, sustaining permanent contestation at the kernel level while the plural arrangement itself faces no enforcement burden.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared actor derives a directionality near the beneficiary end: all four beneficiary groups hold mobile or arbitrage-grade exit, so effective extraction damps toward subsidy for readers, pastors, and scholars, and sits mildly positive only for publishers — the seat that receives the monetary flows (recorded in gain_flow) while bearing editorial and inventory risk. No victims are declared because this reading constructs no asymmetric extraction: the coordination costs it imposes (friction, confusion, duplication) are diffuse and borne by the same beneficiaries it serves, which is the signature of near-pure coordination. Denominational liturgical bodies combine agenda-setting with beneficiary position — they ratify version lists but are themselves served by the flexibility. Suppression is authored raw and unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The KJV's founding mandate — to be the single authorized text unifying a national church — is dead: no institution pursues enforced uniformity, and the arrangement's persistence is explained by genuine function substitution (the literary-historical register) rather than by inertia or performance. This distinction is exactly what the classification protects: reading the dead founding mandate as zombie persistence would misclassify a load-bearing cultural artifact as a piton, while reading the retained function as the original mandate would erase the genealogy and hide why the KJV's authority needed re-grounding at all. The founding-problem interview records status=dead with verdict=world_rearranges; the mismatch flag this combination raises is cross-checked against the computed theater path, which stays low — the arrangement persists by use, not by ritual. Conversely, the mandatrophy lens prevents the opposite error: treating the whole plural ecology as mere market churn ignores that its coordination function (matching register to purpose) is real and currently performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading (functional_equivalence) of the kernel kjv_text_1611; do the sibling readings (exclusive_inspiration, revisable_translation) instantiate structurally different constraints over the same text, and which reading governs any given community?',
    'Longitudinal adoption and enforcement data across denominations: if any body successfully enforces single-text exclusivity, the exclusive reading''s constraint is live there; if manuscript-driven systematic revision uptake spreads, the revisable reading dominates; where version choice follows use-case, this reading governs.',
    'Under the exclusive reading the same kernel computes as a high-suppression gatekeeping structure with identifiable victims (readers and translators of other versions); under the revisable reading it computes as a managed improvement process. This story''s low-extraction classification holds only within the functional-equivalence reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three live readings of the KJV kernel; sibling readings would change the victim set, epsilon, and classification.').

omega_variable(
    revision_churn_vs_reader_value,
    'Does the multi-version market allocate versions to genuine functional niches, or does edition churn manufacture demand beyond the coordination value delivered?',
    'Compare per-edition textual and translational deltas against sales figures and marketing spend; natural experiment from open-license and free digital texts compressing margins without collapsing version diversity.',
    'If churn-driven, effective extraction rises toward hybrid territory and the publisher seat''s directionality shifts toward target; if niche-driven, the current low-extraction profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_churn_vs_reader_value, empirical, 'Whether version proliferation tracks reader value or manufactured demand.').

omega_variable(
    fragmentation_coordination_cost,
    'How large is the coordination cost this reading accepts in exchange for decentralized authority — pew-level version mismatch, incompatible quotation, memorization drift — and is it bounded?',
    'Congregational surveys measuring cross-version friction; observation of lectionary and curriculum standardization choices under voluntary conditions.',
    'Unbounded growth would push communities toward de facto convergence on a single version, generating revival pressure on the exclusive reading; bounded costs leave the plural equilibrium stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_coordination_cost, empirical, 'Size and trajectory of the coordination cost traded against centralized textual authority.').

omega_variable(
    kjv_register_substitutability,
    'Is the King James''s retained literary-historical function intrinsic to the 1611 register, or could modern literary translations eventually substitute for it?',
    'Reception studies tracking whether new literary translations displace KJV phrasing in literature, music, and public rhetoric over successive cohorts.',
    'Full substitutability would hollow the KJV seat''s retained function and push it toward inertial, performance-maintained persistence; irreducibility keeps the complementary arrangement load-bearing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kjv_register_substitutability, conceptual, 'Whether the KJV''s retained function under this reading is durable or slowly decaying.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__functional_equivalence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__functional_equivalence_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__functional_equivalence_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__functional_equivalence_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(kjv__tr_t80, kjv_text_1611__functional_equivalence_reading, theater_ratio, 80, 0.22).
narrative_ontology:measurement(kjv__tr_t100, kjv_text_1611__functional_equivalence_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement(kjv__tr_t120, kjv_text_1611__functional_equivalence_reading, theater_ratio, 120, 0.18).
narrative_ontology:measurement(kjv__tr_t140, kjv_text_1611__functional_equivalence_reading, theater_ratio, 140, 0.16).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 60, 0.29).
narrative_ontology:measurement(kjv__be_t80, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(kjv__be_t100, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(kjv__be_t120, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 120, 0.19).
narrative_ontology:measurement(kjv__be_t140, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 140, 0.17).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kjv_text_1611__functional_equivalence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, resource_allocation).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the authority of the KJV' decomposes into three stories over the kernel kjv_text_1611. exclusive_inspiration_reading authors a high-suppression gatekeeping arrangement (identifiable victims: users and translators of other versions); functional_equivalence_reading (this file) authors a low-extraction plural coordination arrangement; revisable_translation_reading authors a managed-improvement arrangement. Their epsilons differ widely because they name different standing arrangements, not one arrangement viewed from different angles. Influence runs between them: wherever one reading governs a community, it changes the operating environment of the others — exclusivity suppresses the market this reading coordinates, and this reading's pluralism supplies the legitimating ecology in which revision projects proceed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
