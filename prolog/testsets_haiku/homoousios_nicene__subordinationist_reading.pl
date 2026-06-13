% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios Subordinationist Reading: Shared Divinity with Functional/Ontological Subordination
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) condemned the Arian heresy and asserted
 *   that the Son is homoousios (of one substance) with the Father. But
 *   homoousios was semantically ambiguous: it could mean full ontological
 *   equality (the metaphysical-equality reading) or participation in divine
 *   substance while remaining functionally/ontologically subordinate to the
 *   Father (the subordinationist reading instantiated here). This constraint
 *   story models the subordinationist reading as a coherent theological
 *   position compatible with homoousios but fundamentally opposed by
 *   conciliar authorities. The reading benefits subordinationist communities
 *   (Arian remnants, Semi-Arians, scriptural literalists) who maintain that
 *   the Son derives being from the Father and is subordinate in agency and
 *   origin while sharing divine nature. It extracts from conciliar authority
 *   structures, forcing them to continually refute the reading and exclude it
 *   from orthodoxy. The constraint persists across the interval 325–451
 *   through the councils of Constantinople (381), Ephesus (431), and
 *   Chalcedon (451), each of which refined definitions to foreclose
 *   subordinationist interpretation—yet the reading never fully disappears
 *   from Eastern frontier communities and remains a interpretive option in
 *   scriptural literalism.
 *
 * KEY AGENTS:
 *   - Subordinationist theological communities (Arian, Semi-Arian remnants, Gothic episcopal succession): primary beneficiaries, identity-locked to this reading, organized around regional dioceses and scriptural interpretation
 *   - Scriptural-literalist exegetes (e.g., Eunomius, later evangelical expositors): beneficiaries who argue the reading is exegetically sound, constrained exit (retraining required to abandon)
 *   - Nicene conciliar authority (Nicaea, Constantinople, Ephesus, Chalcedon): institutional payers forced to defend their exclusion of the reading, continually refining definitions
 *   - Metaphysical-egalitarian theologians (Cappadocian Fathers, later Niceno-Constantinopolitan tradition): powerful payers engaged in ongoing polemic against the reading
 *   - Imperial political authority: agenda-setter enforcing conciliar orthodoxy through exile, confiscation, bishop appointment
 *   - Scriptural authority principle (abstract): vindicated proposition that Scripture must cohere with theological doctrine, favoring literalist exegesis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.68).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.79).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios Subordinationist Reading: Shared Divinity with Functional/Ontological Subordination").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, 'd43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d').
narrative_ontology:cs_kernel_codification('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', fixed_text).
narrative_ontology:cs_authority_grounding('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', lineage).
narrative_ontology:cs_interpretation_layer_present('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d').
narrative_ontology:cs_reading_relation('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', homoousios_nicene__metaphysical_equality_reading, coexists_with).
narrative_ontology:cs_reading_relation('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', foundational, subordination_compatible_with_homoousios).
narrative_ontology:cs_axiom_status(subordination_compatible_with_homoousios, holdable).
narrative_ontology:cs_axiom_grounding('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', subordination_compatible_with_homoousios, empirically_contingent).
narrative_ontology:cs_axiom('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', foundational, scriptural_literalism_authority_over_conciliar_decree).
narrative_ontology:cs_axiom_status(scriptural_literalism_authority_over_conciliar_decree, holdable).
narrative_ontology:cs_axiom_grounding('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', scriptural_literalism_authority_over_conciliar_decree, deontological).
narrative_ontology:cs_reference_frame('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', apostolic_scriptural_authority).
narrative_ontology:cs_drift_state('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', conciliar_rationalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d43fdfe9-a5d4-4c7c-98f0-d7e051a0d43d', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, scriptural_literalist_exegetes).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, nicene_conciliar_authority).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, metaphysical_egalitarian_theologians).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, scriptural_authority_over_conciliar_decree).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, trinitarian_gradualism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arian, Semi-Arian, and dynastically-embedded subordinationist parishes maintain that the Son derives being from the Father and shares divinity without claiming full ontological equality. They read homoousios as permitting functional subordination—the Son executes the Father's will, is subordinate in agency and origin, but participates in the divine nature. This reading preserves their scriptural literalism and avoids what they see as Nicene metaphysical overreach. Their identity, preaching tradition, and educational lineages are fused with this theological framework; exit means abandoning community authority structures and retraining theological understanding.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities, beneficiary,
    organized, generational, identity_locked, continental).

% Scholars and clergy who prioritize direct scriptural reading and resist metaphysical innovations absent from apostolic teaching. They argue that passages describing the Son as 'begotten,' 'sent,' and 'doing the Father's will' entail subordination, and that homoousios need not rule out this reading if 'same substance' is interpreted as functional participation rather than ontological identity. Their authority derives from exegetical skill and patristic citation; adopting metaphysical equality requires either reinterpreting Scripture or accepting post-apostolic innovation, both costly.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, scriptural_literalist_exegetes, beneficiary,
    moderate, biographical, constrained, regional).

% The conciliar authority structure—grounded in Nicaea's and subsequent councils' decrees—is forced to justify itself against the subordinationist reading's scriptural appeals. If homoousios is compatible with subordination, the councils' authority to settle doctrine via decree (rather than scriptural literalism alone) is weakened. The councils must continually defend against the reading by insisting on stricter interpretation, tightening definitions, and excluding subordinationist proposals as incoherent. This enforcement effort carries political and intellectual cost—legitimacy is contested rather than settled.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, nicene_conciliar_authority, payer,
    institutional, civilizational, arbitrage, continental).

% Theologians committed to reading homoousios as securing full ontological equality (Cappadocian fathers, Niceno-Constantinopolitan tradition, later Chalcedonian orthodoxy) experience this reading as directly opposing their exegetical and metaphysical conclusions. They must continually argue that subordinationist interpretations of homoousios are incoherent, logically contradictory, or heretical—work that would be unnecessary if the reading were foreclosed by conciliar decree. Their intellectual authority depends on defeating the reading repeatedly; they pay in ongoing polemic effort.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, metaphysical_egalitarian_theologians, payer,
    powerful, generational, mobile, continental).

% The abstract principle that Scripture—direct apostolic witness—holds authority over post-apostolic councils and metaphysical innovation. This reading vindicates that principle by holding that homoousios must cohere with scriptural literalism and subordinationist passages. The principle is not an actor but a vindicated proposition; it collects no rents but frames what is permissible to believe.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, scriptural_authority_principle, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(homoousios_nicene__subordinationist_reading, scriptural_authority_principle).

% The Roman imperial administration enforced Nicene orthodoxy through political machinery—exile, confiscation, and appointment of compliant bishops. From the imperial seat, subordinationism is an ongoing threat to doctrinal unity and thus to political stability. The reading persists despite enforcement because it has scriptural and patristic resources and appeals to communities (Goths, peripheral dioceses) beyond immediate imperial reach. The agenda-setter pays in suppression costs—continued exile and polemic are required to prevent the reading from gaining regional dominance.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, imperial_political_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% The sequence of councils (Nicaea 325, Constantinople 381, Ephesus 431, Chalcedon 451) observe and respond to the subordinationist reading by refining definitions. From this seat, the reading is a persistent competitor for doctrinal authority—one that forces the councils to justify their right to define the faith rather than merely transmitting Scripture. Each council's output is partly a response to subordinationist proposals; the councils are the analytical frame.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, ecclesiastical_councils, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological framework for interpreting Christ's divine nature in a way that honors both His participation in divinity and His obedience to the Father as described in Scripture. Solves the problem: how can the Son be 'God' (divine nature, homoousios) while Scripture depicts Him as sent, obedient, and subordinate in agency? This reading coordinates the authority of direct Scripture with the council's assertion that the Son shares the divine substance—by permitting subordination within that sharing.
% TRANSFER_FUNCTION: Transfers ecclesiastical-definition authority from imperial conciliar decree back toward scriptural literalism and regional theological autonomy. Subordinationist communities and their exegetes retain the right to interpret homoousios through scriptural categories rather than accepting conciliar metaphysical glosses. The conciliar authority structure loses the exclusive right to settle Christological doctrine; the reading redistributes power toward local interpretation and Scripture-first theology.
% ABSENT_VOICES: Eastern frontier communities (Goths, Persians, Copts with Monophysite sympathies) who held subordinationist positions but were not present at or did not ratify Nicaea and Constantinople. They would argue for theological pluralism and regional autonomy; their absence from conciliar tables meant their scriptural readings were never formally heard. Apocalyptic and mystical theologians (Montanists, later Hesychasts) who saw subordinationism as preserving the radical transcendence of the Father also were systematically excluded from official conciliar dialogue.
% DISAPPEARANCE_RATIONALE: If this reading disappeared—if homoousios were conclusively incompatible with any form of subordination—the landscape of Christian theology would reorganize around metaphysical equality as the sole orthodox position. Subordinationist communities would face forced choice: reinterpret their scriptural reading or exit communion. Regional theological autonomy would collapse into universal conciliar jurisdiction. Scriptural literalism would lose its strongest interpretive claim (the subordinationist passages become 'apparent' rather than 'real' teachings). The ecclesiastical politics would stabilize around egalitarian dogma.
% FOUNDING_PROBLEM: The Council of Nicaea (325) was convened to settle a schism: Arius and his followers taught that the Son was created, not eternal, and subordinate in being to the Father. Nicaea condemned this and asserted homoousios—'same substance'—to exclude Arian subordinationism. But 'same substance' was ambiguous: it could mean ontological identity (the metaphysical reading) or functional/participatory sharing (this subordinationist reading). The founding problem was: How can we exclude radical Arian subordinationism (creation ex nihilo of the Son) while preserving the scriptural appearance of subordination in agency and origin?
% FOUNDING_PROBLEM_CORROBORATION: The conciliar authorities (Nicaea, Constantinople, Ephesus, Chalcedon) attest that radical Arianism—the creation of the Son—was condemned and excluded. But subordinationist communities (documented in 4th-6th century regional synods, Gothic episcopal succession, fragments in Theodoret and later patristic debates) attest that functional/ontological subordination remained a live position compatible with homoousios. Imperial and conciliar authorities insist subordinationism is incoherent with homoousios; subordinationist theologians and biblical scholars (Eunomius, later Semiarians, and modern evangelical exegetes) attest that it is coherent. The corroboration for the founding problem's 'contested' status comes from post-Nicene councils' repeated need to refute subordinationism, demonstrating that the problem persists despite Nicaea's decree.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the reading's capacity to extract interpretive authority from the conciliar system: subordinationist communities claim the right to read homoousios through scriptural categories rather than accepting conciliar metaphysical glosses. This is extraction in the sense that authority flows away from centralized conciliar decree toward regional scriptural interpretation. Suppression (0.79) is high because the reading's persistence across 126 years despite imperial enforcement (exile, confiscation, doctrinal condemnation) demonstrates active coercive pressure—yet the reading never ceases. Theater (0.42) reflects the conciliar response: each successive council claims to have finally settled the doctrine, yet each council must repeat the refutation of subordinationism, suggesting performative rather than terminal resolution. Accessibility of alternatives (0.72) is moderately high at interval end: by 451, the equality reading is dominant in official channels, yet subordinationist alternatives remain intellectually defensible within scriptural literalism and persist in frontier communities. Resistance (0.58) is moderate: the reading meets genuine resistance from imperial enforcement and metaphysical theologians, but it is not overwhelmed—it continues to recruit new exegetes and defend itself through patristic citations. The measurement series show extractiveness, suppression, and theater rising across the interval as conciliar definitions tighten (Constantinople's refinement in 381, Ephesus and Chalcedon's further precision), while accessibility of alternatives and resistance show slight decline as conciliar dominance increases. The coercion grid models this as structural-level suppression hardening (organizational and individual suppression rising sharply) while structural resistance erodes, indicating the imperial machinery is overwhelmingly powerful but not total—the reading persists in organization and class-level resistance despite individual-level suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the conciliar-authority seat, the subordinationist reading is incoherent with homoousios and must be continuously refuted and excluded as heresy. From the subordinationist community's seat, the reading is a faithful exegetical option that honors both the homoousios assertion and the scriptural passages depicting subordination in agency and origin. From the imperial seat, the reading is a political threat to doctrinal unity and must be suppressed through force. From the egalitarian-theologian seat, the reading is intellectually defeated but must be continually refuted because it has scriptural appeals. From the analytical seat, the reading is a coherent live option if homoousios is semantically open to subordination—the key question is whether the term logically entails equality or permits gradation. The engine will compute per-seat classifications from the structural data: the conciliar-institutional seat should compute as beneficiary of the metaphysical-equality reading (which it controls) and payer under this subordinationist reading (which it must suppress). Subordinationist seats should compute as beneficiaries of this reading and targets of conciliar suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinationist communities are structural beneficiaries (d near 0.2–0.3): they retain interpretive authority, their scriptural reading is affirmed, their identity and community practice are vindicated. They are not identity-locked to accepting conciliar decree; they are identity-locked to scriptural literalism, which this reading permits. Egalitarian theologians and conciliar institutions are structural targets (d near 0.7–0.8): they must expend polemic effort, engage in repetitive refutation, enforce compliance through imperial machinery, and defend their authority against the reading's scriptural appeals. The imperial authority is the agenda-setter (d near 0.6): it benefits from doctrinal uniformity but must pay suppression costs; its exit is arbitrage (it can switch to enforcing a different doctrine or withdraw enforcement entirely). The reading's extractiveness operates through asymmetric distribution of interpretive authority: beneficiaries gain the right to read Scripture directly; payers lose the exclusive right to settle doctrine through decree. This is extraction because the conciliar system's survival depends on its authority to define homoousios—a reading that permits subordinationism undermines that claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (How can the Son be homoousios with the Father while Scripture depicts Him as sent, obedient, and subordinate?) was to be solved by the Council of Nicaea's assertion that homoousios excludes subordinationism. But the problem is not solved—it is deferred. The subordinationist reading shows that homoousios is ambiguous enough to permit the solution's evasion. By 451, after 126 years of conciliar refinement (Constantinople's further definition of the Holy Spirit, Ephesus's Christological precision, Chalcedon's two-natures doctrine), the doctrine is progressively clarified, yet the subordinationist reading persists—never adopting any conciliar refinement, simply reinterpreting the old terms through scriptural literalism. This is a sign of mandatrophy: the conciliar mechanism (decree → compliance) is not working. The founding problem remains contested because the boundary between 'same substance' (homoousios) and 'subordinate in agency' is exegetically live. The constraint's theater_ratio rises (0.42 by 451) because increasingly the councils are engaged in performative refutation (repeating the same arguments at each council) rather than terminal solution. The mandatrophy is not resolved; it is managed through ongoing suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoousios_semantic_ambiguity,
    'Does ''homoousios'' (same substance/essence) logically entail ontological equality of Father and Son, or is it semantically compatible with functional/hierarchical subordination where the Son derives being from the Father but shares divinity?',
    'Exegetical consensus among patristic scholars on the term''s usage in pre-Nicene sources (Origen, Dionysius of Alexandria, Methodius) and Nicene intent. Comparison of homoousios against homoiousios (similar substance) and heteroousios (different substance) to establish whether the term''s precision excludes subordination or merely excludes radical Arianism.',
    'If homoousios is semantically open to subordination, the reading remains coherent and the conciliar authority''s claim to have settled the doctrine is weakened—subordinationism is a legitimate exegetical option. If homoousios logically entails ontological equality, the reading is incoherent and the conciliar closure is complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoousios_semantic_ambiguity, empirical, 'Whether homoousios is semantically open to subordination or logically entails equality.').

omega_variable(
    scriptural_subordination_authenticity,
    'Do the scriptural passages depicting the Son as ''sent,'' ''obedient,'' ''begotten,'' and ''doing the Father''s will'' represent authentic teachings about metaphysical or functional subordination, or are they conventional incarnational language that does not entail subordination in being?',
    'Diachronic exegetical analysis: did first-century authors intend these passages to teach subordination in being, or only in function/agency during incarnation? Patristic testimony from pre-Nicene sources (Justin, Irenaeus, Clement, Origen) on how they read these passages.',
    'If subordination is authentically taught in Scripture (not merely apparent), the subordinationist reading has a strong exegetical foundation and cannot be dismissed as post-apostolic innovation. If the passages are merely functional/incarnational language, the metaphysical egalitarian reading gains exegetical advantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_subordination_authenticity, empirical, 'Whether scriptural subordination language is metaphysical or merely functional.').

omega_variable(
    identity_lock_mechanism,
    'To what extent is the subordinationist reading sustained by structural identity-fusion (theological training, community belonging, patriarchal lineage, scriptural literalism as a core identity practice) versus by genuine intellectual conviction that the reading is exegetically and logically sound?',
    'Post-suppression observational study: if subordinationist communities are prevented from teaching this reading (exile, doctrinal prohibition, educational exclusion), do intellectual defenses of the reading persist among scholars with no institutional stake? Does the reading''s frequency decline among next-generation community members, or does it continue through identity transmission?',
    'If the reading is heavily identity-locked, the suppression mechanism''s high cost reflects not just intellectual disagreement but the cost of severing identity ties. If identity-lock is minimal, suppression is working against genuine intellectual opposition, which costs more and is less stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether subordinationist commitment is identity-locked or purely intellectual.').

omega_variable(
    conciliar_authority_grounding,
    'Does the conciliar authority to define doctrine derive from apostolic succession and divine guidance, or from imperial political power enforcing uniformity? If the latter, is the reading''s persistence a sign of conciliar authority''s limits or a sign of the reading''s genuine exegetical merit?',
    'Genealogical analysis: trace the councils'' authority claims before and after imperial enforcement machinery was in place. Examine whether councils convened by imperial decree (Nicaea, Ephesus) have weaker or stronger contemporary legitimacy than councils emerging from ecclesiastical consensus without imperial coercion. Survey whether subordinationist communities accepted conciliar decisions as binding based on internal ecclesiastical authority or only under political duress.',
    'If conciliar authority rests on imperial power, the reading''s persistence is legitimate resistance to politicized doctrine. If conciliar authority rests on apostolic succession, the reading''s persistence is heresy requiring continued suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conciliar_authority_grounding, conceptual, 'Whether conciliar authority is grounded in apostolic legitimacy or imperial power.').

omega_variable(
    reading_foreclosure_versus_coexistence,
    'Is the subordinationist reading logically foreclosed by the metaphysical-equality reading, or do both readings remain coherent within different frameworks (e.g., literalist-exegetical vs. metaphysical-systematic)?',
    'Formal logical analysis: construct a model of the metaphysical-equality reading and a model of the subordinationist reading. Test whether both can be true in any possible world or framework. If both can coexist in different epistemic frames (Scripture-first vs. metaphysics-first), coexistence holds; if the readings contradict in every frame, foreclosure holds.',
    'If coexistence is possible, the engine''s classification of reading_relations should be ''coexists_with.'' If foreclosure is true, it should be ''forecloses.'' This determines whether the subordinationist reading is a persistent live option or a defeated position awaiting final suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_versus_coexistence, conceptual, 'Whether the subordinationist reading is foreclosed by the equality reading or coexists with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_nicene__subordinationist_reading, theater_ratio, 325, 0.28).
narrative_ontology:measurement_basis(homo_tr_t325, observed).
narrative_ontology:measurement(homo_tr_t350, homoousios_nicene__subordinationist_reading, theater_ratio, 350, 0.32).
narrative_ontology:measurement_basis(homo_tr_t350, observed).
narrative_ontology:measurement(homo_tr_t375, homoousios_nicene__subordinationist_reading, theater_ratio, 375, 0.37).
narrative_ontology:measurement_basis(homo_tr_t375, observed).
narrative_ontology:measurement(homo_tr_t400, homoousios_nicene__subordinationist_reading, theater_ratio, 400, 0.4).
narrative_ontology:measurement_basis(homo_tr_t400, observed).
narrative_ontology:measurement(homo_tr_t425, homoousios_nicene__subordinationist_reading, theater_ratio, 425, 0.42).
narrative_ontology:measurement_basis(homo_tr_t425, observed).
narrative_ontology:measurement(homo_tr_t451, homoousios_nicene__subordinationist_reading, theater_ratio, 451, 0.42).
narrative_ontology:measurement_basis(homo_tr_t451, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_nicene__subordinationist_reading, base_extractiveness, 325, 0.48).
narrative_ontology:measurement_basis(homo_be_t325, observed).
narrative_ontology:measurement(homo_be_t350, homoousios_nicene__subordinationist_reading, base_extractiveness, 350, 0.54).
narrative_ontology:measurement_basis(homo_be_t350, observed).
narrative_ontology:measurement(homo_be_t375, homoousios_nicene__subordinationist_reading, base_extractiveness, 375, 0.61).
narrative_ontology:measurement_basis(homo_be_t375, observed).
narrative_ontology:measurement(homo_be_t400, homoousios_nicene__subordinationist_reading, base_extractiveness, 400, 0.67).
narrative_ontology:measurement_basis(homo_be_t400, observed).
narrative_ontology:measurement(homo_be_t425, homoousios_nicene__subordinationist_reading, base_extractiveness, 425, 0.68).
narrative_ontology:measurement_basis(homo_be_t425, observed).
narrative_ontology:measurement(homo_be_t451, homoousios_nicene__subordinationist_reading, base_extractiveness, 451, 0.68).
narrative_ontology:measurement_basis(homo_be_t451, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_nicene__subordinationist_reading, suppression_requirement, 325, 0.62).
narrative_ontology:measurement_basis(homo_su_t325, observed).
narrative_ontology:measurement(homo_su_t350, homoousios_nicene__subordinationist_reading, suppression_requirement, 350, 0.68).
narrative_ontology:measurement_basis(homo_su_t350, observed).
narrative_ontology:measurement(homo_su_t375, homoousios_nicene__subordinationist_reading, suppression_requirement, 375, 0.73).
narrative_ontology:measurement_basis(homo_su_t375, observed).
narrative_ontology:measurement(homo_su_t400, homoousios_nicene__subordinationist_reading, suppression_requirement, 400, 0.77).
narrative_ontology:measurement_basis(homo_su_t400, observed).
narrative_ontology:measurement(homo_su_t425, homoousios_nicene__subordinationist_reading, suppression_requirement, 425, 0.79).
narrative_ontology:measurement_basis(homo_su_t425, observed).
narrative_ontology:measurement(homo_su_t451, homoousios_nicene__subordinationist_reading, suppression_requirement, 451, 0.79).
narrative_ontology:measurement_basis(homo_su_t451, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=325, tn=451
narrative_ontology:measurement(homo_grid_01, homoousios_nicene__subordinationist_reading, accessibility_collapse(class), 325, 0.52).
narrative_ontology:measurement(homo_grid_02, homoousios_nicene__subordinationist_reading, accessibility_collapse(class), 451, 0.68).
narrative_ontology:measurement(homo_grid_03, homoousios_nicene__subordinationist_reading, accessibility_collapse(individual), 325, 0.68).
narrative_ontology:measurement(homo_grid_04, homoousios_nicene__subordinationist_reading, accessibility_collapse(individual), 451, 0.82).
narrative_ontology:measurement(homo_grid_05, homoousios_nicene__subordinationist_reading, accessibility_collapse(organizational), 325, 0.64).
narrative_ontology:measurement(homo_grid_06, homoousios_nicene__subordinationist_reading, accessibility_collapse(organizational), 451, 0.78).
narrative_ontology:measurement(homo_grid_07, homoousios_nicene__subordinationist_reading, accessibility_collapse(structural), 325, 0.58).
narrative_ontology:measurement(homo_grid_08, homoousios_nicene__subordinationist_reading, accessibility_collapse(structural), 451, 0.72).
narrative_ontology:measurement(homo_grid_09, homoousios_nicene__subordinationist_reading, resistance(class), 325, 0.58).
narrative_ontology:measurement(homo_grid_10, homoousios_nicene__subordinationist_reading, resistance(class), 451, 0.62).
narrative_ontology:measurement(homo_grid_11, homoousios_nicene__subordinationist_reading, resistance(individual), 325, 0.48).
narrative_ontology:measurement(homo_grid_12, homoousios_nicene__subordinationist_reading, resistance(individual), 451, 0.44).
narrative_ontology:measurement(homo_grid_13, homoousios_nicene__subordinationist_reading, resistance(organizational), 325, 0.64).
narrative_ontology:measurement(homo_grid_14, homoousios_nicene__subordinationist_reading, resistance(organizational), 451, 0.54).
narrative_ontology:measurement(homo_grid_15, homoousios_nicene__subordinationist_reading, resistance(structural), 325, 0.52).
narrative_ontology:measurement(homo_grid_16, homoousios_nicene__subordinationist_reading, resistance(structural), 451, 0.48).
narrative_ontology:measurement(homo_grid_17, homoousios_nicene__subordinationist_reading, stakes_inflation(class), 325, 0.38).
narrative_ontology:measurement(homo_grid_18, homoousios_nicene__subordinationist_reading, stakes_inflation(class), 451, 0.55).
narrative_ontology:measurement(homo_grid_19, homoousios_nicene__subordinationist_reading, stakes_inflation(individual), 325, 0.55).
narrative_ontology:measurement(homo_grid_20, homoousios_nicene__subordinationist_reading, stakes_inflation(individual), 451, 0.78).
narrative_ontology:measurement(homo_grid_21, homoousios_nicene__subordinationist_reading, stakes_inflation(organizational), 325, 0.51).
narrative_ontology:measurement(homo_grid_22, homoousios_nicene__subordinationist_reading, stakes_inflation(organizational), 451, 0.72).
narrative_ontology:measurement(homo_grid_23, homoousios_nicene__subordinationist_reading, stakes_inflation(structural), 325, 0.42).
narrative_ontology:measurement(homo_grid_24, homoousios_nicene__subordinationist_reading, stakes_inflation(structural), 451, 0.64).
narrative_ontology:measurement(homo_grid_25, homoousios_nicene__subordinationist_reading, suppression(class), 325, 0.58).
narrative_ontology:measurement(homo_grid_26, homoousios_nicene__subordinationist_reading, suppression(class), 451, 0.72).
narrative_ontology:measurement(homo_grid_27, homoousios_nicene__subordinationist_reading, suppression(individual), 325, 0.7).
narrative_ontology:measurement(homo_grid_28, homoousios_nicene__subordinationist_reading, suppression(individual), 451, 0.86).
narrative_ontology:measurement(homo_grid_29, homoousios_nicene__subordinationist_reading, suppression(organizational), 325, 0.65).
narrative_ontology:measurement(homo_grid_30, homoousios_nicene__subordinationist_reading, suppression(organizational), 451, 0.81).
narrative_ontology:measurement(homo_grid_31, homoousios_nicene__subordinationist_reading, suppression(structural), 325, 0.48).
narrative_ontology:measurement(homo_grid_32, homoousios_nicene__subordinationist_reading, suppression(structural), 451, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__subordinationist_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, arian_heresy_radical_creation).

% DUAL FORMULATION NOTE:
% The homoousios_nicene kernel spawns three distinct constraint stories corresponding to three readings of the conciliar term. The subordinationist reading instantiated here models the exegetical position that homoousios is compatible with functional or ontological subordination—it permits the Son to derive being from the Father while sharing divine substance. The metaphysical-equality reading (sibling constraint) models the position that homoousios entails full ontological equality. The honorific-similarity reading (sibling constraint) models the position that homoousios signifies likeness without identity. These are not three perspectives on one constraint; they are three structurally distinct constraints arising from the same contested kernel. Their ε values differ substantially: the metaphysical-equality reading (conciliar orthodoxy) has negligible extractiveness for its institutional beneficiaries (the structure is internally consistent and widely accepted); the subordinationist reading (this one) extracts from conciliar authority by offering an exegetically defensible alternative. The three readings are linked by network.affects_constraints because subordinationism's persistence directly influences the conciliar authority's need to continually refute it (influencing the equality reading), and the honorific reading's ambiguity about what 'same substance' means feeds into the subordinationist reading's space of interpretive possibility. All three readings coexist historically, though conciliar authority works to foreclose subordinationism through successive councils.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
