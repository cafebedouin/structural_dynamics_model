% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoiousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoiousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoiousios_reading
 *   human_readable: Homoiousios Christology: Similar Substance Doctrine
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The homoiousios reading of Nicene Christology holds that Christ is of
 *   similar substance (homoios ousia) with the Father, preserving an
 *   ontological distinction while affirming divinity. This reading emerged in
 *   the fourth century as a moderate position between strict Arian
 *   subordinationism and absolute homoousios identity. It legitimated
 *   theological pluralism and regional episcopal autonomy but fragmented the
 *   imperial religious uniformity agenda. The constraint operates as both a
 *   doctrinal formula and an enforcement structure: councils anathematize it,
 *   emperors suppress it, regional churches defend it. The measurement series
 *   tracks how suppression intensified from 340 onward (Council of Antioch in
 *   341 condemned homoiousios variants), peaked at 360-375 (imperial
 *   crackdowns under Valens, who favored Arian-adjacent positions that made
 *   homoiousios seem dangerously orthodox), and modulated downward after
 *   Constantinople I in 381 (which appears to have stabilized a complex
 *   settlement permitting some regional variance).
 *
 * KEY AGENTS:
 *   - Regional episcopal councils (beneficiary of theological autonomy; constrained by imperial pressure; identity-locked to exegetical tradition)
 *   - Imperial authority structure (agenda-setter enforcing uniformity; institutional power; seeks to suppress homoiousios)
 *   - Homoousios orthodox faction (payer — their doctrinal monopoly fragments; institutional power; constrained by need to continuously anathematize)
 *   - Theological exegetical schools (beneficiary of interpretive freedom; identity-locked to academic/scholastic tradition; moderate power)
 *   - Ecumenical council authority (agenda-setter determining orthodoxy; institutional power; variably aligned with empire or regional interests)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, 0.48).
domain_priors:suppression_score(nicene_christological_kernel__homoiousios_reading, 0.62).
domain_priors:theater_ratio(nicene_christological_kernel__homoiousios_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoiousios_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoiousios_reading, rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoiousios_reading, "Homoiousios Christology: Similar Substance Doctrine").
narrative_ontology:topic_domain(nicene_christological_kernel__homoiousios_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoiousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoiousios_reading, 'b3019358-06f4-47d2-a73b-71971f0d6ef2').
narrative_ontology:cs_kernel_codification('b3019358-06f4-47d2-a73b-71971f0d6ef2', fixed_text).
narrative_ontology:cs_authority_grounding('b3019358-06f4-47d2-a73b-71971f0d6ef2', lineage).
narrative_ontology:cs_interpretation_layer_present('b3019358-06f4-47d2-a73b-71971f0d6ef2').
narrative_ontology:cs_reading_relation('b3019358-06f4-47d2-a73b-71971f0d6ef2', nicene_christological_kernel__homoousios_reading, coexists_with).
narrative_ontology:cs_axiom('b3019358-06f4-47d2-a73b-71971f0d6ef2', foundational, christ_ontologically_distinct_from_father).
narrative_ontology:cs_axiom_status(christ_ontologically_distinct_from_father, holdable).
narrative_ontology:cs_axiom_grounding('b3019358-06f4-47d2-a73b-71971f0d6ef2', christ_ontologically_distinct_from_father, deontological).
narrative_ontology:cs_axiom('b3019358-06f4-47d2-a73b-71971f0d6ef2', foundational, regional_ecclesiastical_autonomy_legitimate).
narrative_ontology:cs_axiom_status(regional_ecclesiastical_autonomy_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('b3019358-06f4-47d2-a73b-71971f0d6ef2', regional_ecclesiastical_autonomy_legitimate, conventional).
narrative_ontology:cs_reference_frame('b3019358-06f4-47d2-a73b-71971f0d6ef2', nicene_pluralist_framework).
narrative_ontology:cs_drift_state('b3019358-06f4-47d2-a73b-71971f0d6ef2', imperial_uniformity_pressure_381, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b3019358-06f4-47d2-a73b-71971f0d6ef2', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, regional_episcopal_councils).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, exegetical_schools).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoiousios_reading, theological_pluralism_advocates).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, imperial_religious_uniformity_agenda).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, ecclesiastical_institutional_cohesion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoiousios_reading, homoousios_orthodox_faction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Regional councils maintain interpretive authority over Christology within their sees. The homoiousios doctrine allows them to defend Christ's distinction from the Father while remaining within orthodox bounds, preserving local theological autonomy against imperial standardization. They benefit from the formula's flexibility: it permits dissent from strict Alexandrian homoousios while avoiding heresy charges.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, regional_episcopal_councils, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, regional_episcopal_councils, agenda_setter).

% Academic and monastic centers (Antiochene, Cappadocian traditions) develop Christological theology through scriptural and philosophical argument. The homoiousios formula permits schools to develop competing theories of how Christ relates to God without immediate heresy condemnation, enabling ongoing debate and refinement. Identity-locked: these institutions are constituted by theological innovation; exit means institutional dissolution.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, exegetical_schools, beneficiary,
    moderate, generational, identity_locked, regional).

% Bishops, theologians, and lay theologians who argue for graduated distinctions in the Godhead (the Son as similar to but not identical with the Father) find the homoiousios formulation legitimates their positions. They can publish, preach, and organize without facing immediate heresy trials, though imperial pressure and orthodox opposition are ever-present.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, theological_pluralism_advocates, beneficiary,
    moderate, biographical, identity_locked, regional).

% The empire seeks a single, standardized Christian confession that legitimates imperial authority and prevents sectarian fragmentation. The homoiousios doctrine's tolerance for regional and exegetical variance undermines this goal: it prevents the imperial imposition of a universal formula and perpetuates legitimate theological disagreement. The empire must continually enforce anathemas and suppress councils to suppress the doctrine's effects.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_religious_uniformity_agenda, payer,
    institutional, generational, constrained, universal).

% Bishops, theologians, and councils committed to strict homoousios (particularly Alexandrian and later imperial-backed positions) pay a cost: the homoiousios doctrine is presented as orthodox-compatible, fragmenting their monopoly on orthodoxy and legitimating what they view as semi-Arian heterodoxy. They must expend resources defending homoousios against homoiousios advocates and continually anathematize the similar-substance formula. They are excluded from this reading's legitimacy structure.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, homoousios_orthodox_faction, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoiousios_reading, homoousios_orthodox_faction, excluded).

% Councils assembled to define orthodoxy (Constantinople I, subsequent synods) must decide whether to enforce homoousios universally or permit homoiousios as an acceptable variant. Their enforcement activity — anathematizing homoiousios councils, endorsing homoousios formulas, calling synods to settle the question — constitutes the constraint's active maintenance.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, ecumenical_council_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Emperors summon councils, ratify creeds, and enforce anathemas. Their interest in homoousios (standard post-Nicaea) or homoiousios (variably supported depending on emperor) shapes which doctrine survives enforcement. Imperial power ultimately determines whether homoiousios persists as a live option or is suppressed into heterodoxy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoiousios_reading, imperial_authority_structure, agenda_setter,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoiousios_reading, imperial_authority_structure).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoiousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an interpretive boundary within orthodox Christology: coordinates theological schools and regional councils around a shared formula that permits Christ to be divine and yet ontologically distinguished from the Father, solving the problem of reconciling monotheism with Christological confession without requiring absolute identity-of-substance.
% TRANSFER_FUNCTION: Transfers ecclesiastical legitimacy and interpretive authority from a universal standardized formula (homoousios) to regional episcopal councils and theological schools, allowing them to retain theological autonomy while remaining within the bounds of orthodoxy. The constraint moves doctrinal jurisdiction from imperial-backed ecumenical councils (which would enforce homoousios uniformly) to regional bodies (which can defend homoiousios variance).
% ABSENT_VOICES: Arianism and thoroughgoing non-Chalcedonian Christologies are excluded: they would argue that Christ is a creature, or that divine and human natures cannot be said to have a common substance at all. Their exclusion is maintained by both homoousios and homoiousios readings, but the homoiousios formula's own logic (similarity-but-distinction) creates ambiguity about where the Arian boundary lies. True Arians would demand a seat at the table but find themselves condemned by both major parties.
% DISAPPEARANCE_RATIONALE: If the homoiousios doctrine and its enforcement vanished overnight, theological debate would collapse toward either strict homoousios uniformity (empire's preference) or fragmentation into explicitly Arian and non-Chalcedonian churches (doctrinal clarification but institutional disaster). Regional episcopal autonomy would dissolve or reorganize under homoousios hegemony, consolidating ecclesiastical hierarchy and eliminating the live theological space homoiousios preserves.
% FOUNDING_PROBLEM: The Council of Nicaea (325 CE) produced the homoousios formula to condemn Arianism, but the formula's rigid language left room for competing interpretations and regional resistances. How can Christ be both fully divine and yet not identical in substance with the Father? The homoiousios doctrine emerges as an answer that preserves the letter of Nicaean authority while permitting ontological distinction and theological pluralism.
% FOUNDING_PROBLEM_CORROBORATION: Ecumenical councils from Constantinople I onward treated homoiousios as a heretical variant, not as a legitimate reading of Nicaea — attesting that the founding problem (theological pluralism compatible with orthodoxy) was not universally recognized as legitimate. However, regional episcopal councils and theologians in Cappadocia, Antioch, and Egypt testified that homoiousios solved the practical problem of maintaining orthodoxy while permitting schools to develop distinct exegetical traditions. Modern ecclesiastical historians (scholars outside the institutional beneficiaries of either reading) document that homoiousios was held by significant orthodox parties and was not defeated by theological argument alone, but by imperial enforcement.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoiousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoiousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoiousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoiousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoiousios_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoiousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoiousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoiousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.48 at interval end because homoiousios permits theological pluralism and regional autonomy — genuine coordination benefit to beneficiaries — while systematically fragmenting the empire's drive for religious uniformity and the Church's institutional consolidation. It is not pure extraction (beneficiaries genuinely gain autonomy and interpretive space) but it is substantially asymmetric: the empire and ecclesiastical hierarchy pay the cost of fragmentation while regional councils gain protected theological variance. Suppression is high (0.62) because the constraint's survival depends on continuous enforcement: councils must repeatedly anathematize homoiousios, emperors must suppress councils favoring it, and the doctrine persists only because regional parties have enough power to resist total suppression. Theater ratio grows from 0.25 to 0.45 over the interval as the original substantive question (how to reconcile monotheism with Christological confession) becomes increasingly a question of jurisdictional power: is homoiousios orthodoxy-compatible? The answer is yes if regional councils decide; no if the empire decides. By 360-375, enforcement is dominated by who holds imperial power, not by theological argument. Accessibility collapse is moderate (0.58) because the constraint does not eliminate alternatives entirely: homoousios remains a live option (the empire's preference), Arianism remains a live heterodoxy (the excluded lower bound), and homoiousios itself persists despite suppression — alternatives do not collapse, they compete. Resistance is high (0.72) because regional councils, theological schools, and even some emperors actively defend homoiousios against suppression, making the constraint's persistence a matter of ongoing contention, not settled fact.
 *
 * PERSPECTIVAL GAP:
 *   From a regional episcopal council's position, homoiousios is genuine theological coordination: it permits the council to affirm Christ's divinity and the Nicaean settlement while defending ontological distinction and local exegetical authority. From the imperial position, the same constraint is pure extraction: it allows regions to evade standardization and fragments the religious uniformity the empire requires for political control. The engine computes these divergent seats from the structural data: the beneficiary seat (regional councils) perceives coordination; the victim seat (imperial authority) perceives forced fragmentation. Neither perception is false; the constraint genuinely coordinates pluralism and genuinely extracts uniformity-denial.
 *
 * DIRECTIONALITY LOGIC:
 *   Regional episcopal councils and exegetical schools are structural beneficiaries (d near 0.2-0.3): they gain doctrinal autonomy, legitimacy for local theological work, and protected space within orthodoxy. Their exit is constrained and identity-locked (to their episcopal see or theological tradition) but within those constraints they are subsidized by the constraint — the formula grants them legitimacy they would not otherwise have under strict homoousios enforcement. The empire and ecclesiastical hierarchy sit near full targets (d near 0.85-0.95): they pay the cost of fragmentation, must spend resources on suppression, and lose the consolidated authority they would gain from homoousios uniformity. Their exit is constrained (empire cannot exit imperial politics; church cannot exit its institutional form) and their structural position is most threatened by the constraint's operation. Homoousios advocates sit mid-range (d near 0.65-0.75): they are neither pure targets nor pure beneficiaries, but rather institutional competitors whose doctrinal monopoly is fragmenting — they must defend their position continuously but retain significant power. The engine derives directionality from beneficiary/victim declarations; this reading names regional churches and exegetical schools as beneficiaries (they gain protected theological space) and imperial uniformity + ecclesiastical cohesion as victims (they pay fragmentation costs).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to reconcile Nicaean orthodoxy with theological pluralism — remains contested throughout the interval. At the interval start (325), the problem is live and urgent: Nicaea has condemned Arianism but left interpretive space open. By 360, the problem's status is contested: homoousios advocates claim the problem is solved (full identity eliminates ambiguity); homoiousios advocates claim the problem persists (pluralism is still needed). By 381 (Constantinople I), the council appears to have shifted the problem: rather than asking whether Christ and Father are identical in substance, it asks whether regional and ecumenical authorities can coexist. Mandatrophy (founding problem dead but constraint persists for institutional reasons) is incipient but not yet manifest at 381. The constraint is not yet a piton — it still solves a live problem for beneficiaries, and suppression remains high enough to require active enforcement for the empire's purposes. A piton diagnosis would require measurement showing theater ratio above 0.7+ and suppression declining as enforcement becomes purely theatrical; that is not evident by 381. The constraint remains a rope (genuine coordination for regional pluralism, though increasingly extractive from the uniformity perspective).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substance_ontology_ambiguity,
    'What does ''ousia'' (substance/essence) mean in the Christological context? Does it permit meaningful distinctions between Father and Son, or does any use of the term require absolute identity?',
    'Examination of contemporary theological texts (Cappadocian Fathers, Athanasius, others) to determine whether homoiousios had a coherent philosophical meaning or was always incoherent; or examination of later councils'' explanations of why homoousios superseded homoiousios.',
    'If homoiousios was coherent and philosophically defensible, the constraint represents genuine theological pluralism; if it was merely a stopgap or philosophical confusion, the constraint represents institutional resistance masquerading as doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substance_ontology_ambiguity, conceptual, 'Whether homoiousios has a coherent ontological meaning or is epistemically incoherent.').

omega_variable(
    imperial_enforcement_mechanics,
    'How much of homoiousios suppression is active imperial enforcement (anathemas, deposition of bishops, military pressure) versus passive institutional drift toward homoousios uniformity? What would suppression metrics show if we measure enforcement activity separately from doctrinal adoption?',
    'Detailed historical accounting of imperial decisions, council votes, and enforcement actions year-by-year; separation of doctrinal conformity driven by persuasion from conformity driven by threat.',
    'High active enforcement with low voluntary adoption would indicate the constraint persists only through coercion (supporting snare classification); high voluntary adoption with low enforcement would indicate coordination (supporting rope classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(imperial_enforcement_mechanics, empirical, 'Whether suppression is active enforcement or passive institutional drift.').

omega_variable(
    regional_autonomy_vs_fragmentation,
    'Do the regional councils that defend homoiousios perceive themselves as gaining genuine theological autonomy, or as defending a defensive position that fragments their authority relative to an ecumenical settlement?',
    'Analysis of council records, episcopal correspondence, and theological treatises to determine whether regional theologians experienced homoiousios as enabling local scholarship or as enforced provincial marginalization.',
    'If perceived as enabling (beneficiary self-perception aligns with structural analysis), the constraint is a rope with clear coordination benefits; if perceived as marginalization (beneficiaries feel victimized), the constraint''s benefits are more contested and extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_autonomy_vs_fragmentation, empirical, 'Whether regional councils experience homoiousios as autonomous or subordinate.').

omega_variable(
    kernel_foreclosure_question,
    'Does the homoiousios reading''s commitment to ''similar but distinct substance'' logically foreclose the homoousios reading''s commitment to ''identical substance'' within any single theological framework?',
    'Examination of whether a theologian could coherently hold both ''Christ is homoiousios'' and ''Christ is homoousios'' by interpreting the terms differently, or whether they represent genuinely incompatible metaphysical claims.',
    'If they foreclose each other (only one can be true), the reading relation should be ''forecloses''; if they coexist (different frameworks permit both), the reading relation should be ''coexists_with''. This affects how the engine models the kernel''s stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_question, conceptual, 'Whether homoiousios and homoousios are logically incompatible or merely advocated by different parties.').

omega_variable(
    suppression_structural_vs_internalized,
    'For theologians and bishops defending homoiousios, is suppression primarily structural (threat of deposition, anathema, loss of institutional position) or internalized (belief they are defending a minority view correctly, acceptance of orthodox marginality)?',
    'Examination of post-suppression behavior: if suppression is structural, theologians would switch positions when imperial threat changes; if internalized, they would maintain homoiousios even after enforcement pressure decreases.',
    'Structural suppression suggests the constraint is coercive; internalized suppression suggests beneficiaries are identity-locked and the constraint persists through legitimacy, not force. The distinction affects whether ''suppression'' should be understood as extrinsic enforcement or intrinsic commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of homoiousios is enforced externally or accepted internally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoiousios_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoiousios_reading, theater_ratio, 325, 0.25).
narrative_ontology:measurement_basis(nice_tr_t325, projected).
narrative_ontology:measurement(nice_tr_t340, nicene_christological_kernel__homoiousios_reading, theater_ratio, 340, 0.31).
narrative_ontology:measurement_basis(nice_tr_t340, observed).
narrative_ontology:measurement(nice_tr_t360, nicene_christological_kernel__homoiousios_reading, theater_ratio, 360, 0.39).
narrative_ontology:measurement_basis(nice_tr_t360, observed).
narrative_ontology:measurement(nice_tr_t375, nicene_christological_kernel__homoiousios_reading, theater_ratio, 375, 0.45).
narrative_ontology:measurement_basis(nice_tr_t375, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoiousios_reading, theater_ratio, 381, 0.41).
narrative_ontology:measurement_basis(nice_tr_t381, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 325, 0.38).
narrative_ontology:measurement_basis(nice_be_t325, projected).
narrative_ontology:measurement(nice_be_t340, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 340, 0.42).
narrative_ontology:measurement_basis(nice_be_t340, observed).
narrative_ontology:measurement(nice_be_t360, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 360, 0.51).
narrative_ontology:measurement_basis(nice_be_t360, observed).
narrative_ontology:measurement(nice_be_t375, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 375, 0.54).
narrative_ontology:measurement_basis(nice_be_t375, observed).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoiousios_reading, base_extractiveness, 381, 0.48).
narrative_ontology:measurement_basis(nice_be_t381, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 325, 0.45).
narrative_ontology:measurement_basis(nice_su_t325, projected).
narrative_ontology:measurement(nice_su_t340, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 340, 0.55).
narrative_ontology:measurement_basis(nice_su_t340, observed).
narrative_ontology:measurement(nice_su_t360, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 360, 0.68).
narrative_ontology:measurement_basis(nice_su_t360, observed).
narrative_ontology:measurement(nice_su_t375, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 375, 0.75).
narrative_ontology:measurement_basis(nice_su_t375, observed).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoiousios_reading, suppression_requirement, 381, 0.62).
narrative_ontology:measurement_basis(nice_su_t381, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoiousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoiousios_reading, 0.12).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoiousios_reading, nicene_christological_kernel__homoousios_reading).

% DUAL FORMULATION NOTE:
% The homoiousios and homoousios readings are two constraint stories instantiating the same Nicene Christological kernel. They share a common referent (the relationship between Christ and God the Father) but diverge on how that relationship should be understood and enforced. The homoiousios reading permits pluralism and regional autonomy; the homoousios reading enforces uniformity. Neither reading can be true without the other being false. They form a constraint family where the sibling homoousios reading (institutional authority, higher extractiveness from regional perspective, higher theater ratio as it enforces doctrinal uniformity) represents the alternative interpretation of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoiousios_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
