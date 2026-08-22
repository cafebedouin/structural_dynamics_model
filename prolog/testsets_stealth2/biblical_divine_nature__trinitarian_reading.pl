% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Nicene Essence-Unity Settlement (Trinitarian Reading)
 *   domain: theology/religious-authority/doctrinal-history
 *
 * SUMMARY:
 *   The Trinitarian settlement requires assent to one essence shared by three
 *   really distinct persons, codified at Nicaea (325) and Constantinople
 *   (381) and maintained since through creedal subscription, licensing of
 *   teachers, and admission-to-communion control. It solves a real
 *   integration problem — monotheistic confession alongside devotion to
 *   Christ and the Spirit — while concentrating the power to define orthodoxy
 *   in the hierarchy that administers it, and it has always been maintained
 *   against identifiable dissenters: Arians in the fourth century, Socinians
 *   and Unitarians in the early modern period, Oneness Pentecostals today.
 *   This file is ONE READING of the biblical_divine_nature kernel (Rule 1):
 *   the ε referent is the standing Trinitarian arrangement itself as
 *   enforced, never the unitarian or modalist arrangements the siblings would
 *   install; the contest among readings is routed to omegas, and the siblings
 *   are separate files linked through network.affects_constraints. KEY AGENTS
 *   (by structural relationship): - ecclesiastical_hierarchy: agenda-setter
 *   (institutional/arbitrage) — defines the creed, licenses teachers,
 *   administers communion; collects adjudicative authority -
 *   trinitarian_laity: net beneficiary with conformity costs
 *   (moderate/constrained) — receives shared worship language and identity;
 *   owes creedal assent - credentialed_clergy_and_theologians:
 *   dual-positioned beneficiary/payer (moderate/identity_locked) — livelihood
 *   and standing bound to confessional allegiance -
 *   historic_arian_communities: primary target, historical
 *   (powerless/trapped) — exiled, deposed, textually erased after Nicaea -
 *   modern_non_trinitarians: primary target, contemporary (organized/mobile)
 *   — Unitarians and Oneness Pentecostals excluded from credal tables -
 *   ecumenical_bodies: analytical observer (institutional/analytical) —
 *   monitors doctrinal alignment on a Trinitarian admission basis
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.58).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.52).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Nicene Essence-Unity Settlement (Trinitarian Reading)").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theology/religious-authority/doctrinal-history").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '96ec0405-6e64-4b97-8705-f274991dbd6c').
narrative_ontology:cs_kernel_codification('96ec0405-6e64-4b97-8705-f274991dbd6c', formalized).
narrative_ontology:cs_authority_grounding('96ec0405-6e64-4b97-8705-f274991dbd6c', lineage).
narrative_ontology:cs_interpretation_layer_present('96ec0405-6e64-4b97-8705-f274991dbd6c').
narrative_ontology:cs_reading_relation('96ec0405-6e64-4b97-8705-f274991dbd6c', biblical_divine_nature__unitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('96ec0405-6e64-4b97-8705-f274991dbd6c', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('96ec0405-6e64-4b97-8705-f274991dbd6c', foundational, three_hypostases_one_ousia).
narrative_ontology:cs_axiom_status(three_hypostases_one_ousia, holdable).
narrative_ontology:cs_axiom_grounding('96ec0405-6e64-4b97-8705-f274991dbd6c', three_hypostases_one_ousia, theological).
narrative_ontology:cs_axiom('96ec0405-6e64-4b97-8705-f274991dbd6c', foundational, hypostatic_distinction_is_real).
narrative_ontology:cs_axiom_status(hypostatic_distinction_is_real, holdable).
narrative_ontology:cs_axiom_grounding('96ec0405-6e64-4b97-8705-f274991dbd6c', hypostatic_distinction_is_real, theological).
narrative_ontology:cs_axiom('96ec0405-6e64-4b97-8705-f274991dbd6c', secondary, monotheism_preserved_through_essence_unity).
narrative_ontology:cs_axiom_status(monotheism_preserved_through_essence_unity, holdable).
narrative_ontology:cs_axiom_grounding('96ec0405-6e64-4b97-8705-f274991dbd6c', monotheism_preserved_through_essence_unity, instrumental).
narrative_ontology:cs_reference_frame('96ec0405-6e64-4b97-8705-f274991dbd6c', conciliar_hypostatic_essence_unity).
narrative_ontology:cs_drift_state('96ec0405-6e64-4b97-8705-f274991dbd6c', contemporary_pluralist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96ec0405-6e64-4b97-8705-f274991dbd6c', '2026-08-10T09:15:00Z').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_laity).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, credentialed_clergy_and_theologians).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, historic_arian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, modern_non_trinitarians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, trinitarian_laity).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, credentialed_clergy_and_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Councils, synods, and magisterial offices define the creed, license teachers, and admit or bar communicants. Defining the settlement concentrates adjudicative authority here: whoever speaks for the creed speaks for the church, so the administrators of the formula are also its principal collectors of deference, office, and institutional cohesion. Exit is meaningless from this seat — the people who run the arrangement are the arrangement.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Ordinary members receive a shared worship language, a baptismal identity, and a settled answer to who God is, coordinated across every congregation they might visit. Full membership requires assent to the creed; declining it costs a person their congregation and often their extended family network. Switching to a non-creedal body is possible but rarely preserves the local community ties that made membership valuable.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_laity, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, trinitarian_laity, payer).

% Pastors, priests, and academics earn livelihood, standing, and voice inside confessional institutions that require the settlement. Years of selection and formation fuse professional identity with doctrinal allegiance; public dissent typically ends careers and forces a rebuild of self-understanding, so even privately doubtful professionals rarely test the boundary. Their benefit and their exposure are the same relationship viewed from before and after a defection.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, credentialed_clergy_and_theologians, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, credentialed_clergy_and_theologians, payer).

% Fourth-century presbyters and congregations who taught the Son as a created being. After Nicaea they faced deposition, exile, and destruction of their writings; imperial edicts made their teaching illegal inside Roman territory, and their libraries survive mainly as quotations in opponents' refutations. There was nowhere inside the empire their position could be held openly.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, historic_arian_communities, payer,
    powerless, generational, trapped, regional).

% Unitarian denominations and Oneness Pentecostal churches run their own congregations, seminaries, and missions. They are barred from Trinitarian communions' pulpits and from ecumenical bodies that require the creed as a membership condition, and they absorb recurring charges of heresy in popular and academic discourse. Their response has been to build parallel institutions rather than seek standing inside the credal ones.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, modern_non_trinitarians, payer,
    organized, generational, mobile, global).

% World Council of Churches and comparable tables admit members on a Trinitarian basis and monitor doctrinal alignment among member churches. They take testimony from the other seats, commission theological study documents, and can suspend members — the nearest thing to a standing adjudicator the settlement possesses, though its own admission rule presupposes the settlement it would adjudicate.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, ecumenical_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the monotheism-integration problem: how a faith that confesses one God can worship the incarnate Son and experience the Spirit without either dividing God into multiple gods or flattening the Father, Son, and Spirit into indistinguishable roles. The creed supplies one formula coordinating baptism, liturgy, preaching, and teaching across every congregation.
% TRANSFER_FUNCTION: Moves doctrinal assent and membership standing from all believers toward the credal standard; moves adjudicative authority and the power to define orthodoxy upward to the hierarchy; moves dissenters outward, at the cost of communion, office, and (historically) physical security.
% ABSENT_VOICES: Non-Trinitarian Christians sit outside the ecumenical tables their own history created — membership rules requiring Trinitarian confession mean the bodies negotiating consensus exclude the dissent by rule. Jewish and Muslim interlocutors, for whom the doctrine reads as compromised monotheism, are outside the frame entirely. Historically, Arian voices were physically removed (exile, deposition, destruction of texts) before the settlement's reception history was written by its victors.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, baptismal formulas, lectionaries, hymnody, seminary curricula, and the ecumenical architecture built on the creed would all require immediate renegotiation, and the status of Christ would reopen as a live question for roughly two billion adherents — the arrangements of most of the Christian world depend on it.
% FOUNDING_PROBLEM: The earliest communities confessed one God while baptizing in three names, praying to the risen Christ, and attributing their common life to the Spirit. Once Christ-devotion became inseparable from the movement, it needed an account on which honoring Son and Spirit did not divide or dilute the one God; Arius forced the issue by proposing the Son as a created intermediary, and the councils answered with shared essence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: secular historians of late antiquity document the Arian crisis as a genuine political-theological emergency rather than a hierarchy-manufactured one; the rival readings themselves (unitarian and modalist communities) exist and argue only because they take the integration problem to be live; and Jewish and Muslim scholars of Christian doctrine engage the question as substantive. The hierarchy's own attestation is excluded per the corroboration rule.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.58 at interval end: contemporary costs fall on dissenters as exclusion from office, pulpits, ecumenical tables, and credential systems rather than as violence, but they are real, recurring, and borne by named groups. Suppression is 0.52: anathemas remain formally on the books in several communions, membership gates do active work, and social enforcement is strong, but state coercion ended with disestablishment. Theater is 0.32 and rising: creedal recitation retains genuine liturgical function, but the anathema machinery increasingly marks boundaries without stakes attached, a Goodhart drift of boundary-maintenance into performance. Accessibility collapse is 0.45: inside a credal communion alternatives collapse almost completely (one cannot teach Arianism and keep the pulpit), while across the religious landscape Unitarian and Oneness institutions persist, so alternatives are suppressed locally but not globally. Resistance is 0.55: non-Trinitarian movements have re-emerged continuously for seventeen centuries despite every enforcement regime. The measurement series run on one shared time grid (points 0–24 at stride 4) with every tracked metric authored at every point; the suppression_requirement series is authored deliberately because this story specifically traces enforcement-capacity change — build-up through the imperial and medieval eras, decay after disestablishment — rather than mere shifts in extraction. Time-point mapping: 0 ≈ Nicaea era, 4 ≈ Constantinople/Theodosian consolidation, 8 ≈ medieval enforcement height, 12 ≈ Reformation-era confessionalization, 16 ≈ Enlightenment toleration, 20 ≈ modern ecumenical consolidation, 24 ≈ contemporary.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's seat the arrangement is guardianship of revealed truth and the source of its own authority to speak; from the payer seats the same structure is enforced conformity with the exits priced out. The laity seat sits nearer symmetric — genuine coordination benefit, diffuse conformity cost. Same-level divergence appears between laity and credentialed clergy: nominally the same tradition membership, but the clergy seat's identity_lock makes the boundary far more expensive to test, so identical doctrine produces different effective exposure by seat. Inter-institutionally, national hierarchies administer the settlement while ecumenical bodies presuppose it in their own admission rules, so the observer seat cannot adjudicate the settlement without circularity.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy declares as sole unambiguous beneficiary and agenda-setter: it collects the arrangement's principal yield (definitional authority), so its derived d sits near the beneficiary end. Laity and clergy carry dual declarations — beneficiary with payer secondary — placing them at low-to-moderate d, with the clergy seat's identity_lock pushing its effective exposure up sharply conditional on dissent. Historic Arians (trapped, powerless) derive near full-target d; modern non-Trinitarians derive high d with slight damping from mobile exit (their parallel institutions soften but do not remove the exclusion). Ecumenical bodies are analytical and feed no directional pull. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the global spatial scope of the enforcement network.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated, so the mismatch consumer reads status(live) × verdict(world_rearranges) — consistent, no capture/zombie flag. The classification resists two mislabels: pure snare (the coordination function is genuine — the monotheism-integration problem predates and outlasts any particular enforcement regime, and the creed does coordinate worship for billions) and piton (the function has not atrophied; theater_ratio remains below 0.5 and boundary-maintenance still organizes the largest religious communion on earth). Tangled rope holds both truths: real coordination and real asymmetric extraction through the same credal structure. The live mandatrophy risk runs the other direction — misreading enforcement decay (falling suppression_requirement) as death of function, when the decay reflects disestablishment rather than obsolescence of the integration problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates only the trinitarian_reading of the biblical_divine_nature kernel; how would the classification shift if the unitarian_reading or modalist_reading were instantiated instead?',
    'Generate the sibling stories (biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading) and compare computed classifications across the family; the victim set, enforcement structure, and ε re-derive per reading.',
    'Under the unitarian reading the enforced arrangement and its victim set differ structurally (the deviant-majority/deviant-minority relation inverts along different historical lines); under the modalist reading the personal-distinction axis replaces the deity axis as the contested element. Cross-family comparison is the only way to detect whether the measured extraction belongs to the kernel or to this reading''s enforcement history.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    hellenization_vs_articulation,
    'Is the ousia/hypostasis framework a necessary articulation of scriptural monotheism-plus-christology, or a Greek metaphysical import that manufactured the very problem it then solved?',
    'Historical-comparative study of pre-Nicene theologies and non-Hellenic Christian traditions (Semitic-language Christianity, Syriac trajectories), plus semantic analysis of how ousia and hypostasis were actually taken up in the fourth century.',
    'If import, part of the measured extraction is the price of an imposed framework rather than intrinsic to the faith — supporting attribution of suppression to institutional choice and raising the snare-side weight; if articulation, the coordination function runs deeper and the extraction is more purely enforcement overhead on a genuine settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hellenization_vs_articulation, conceptual, 'Whether the settlement''s conceptual kernel is native to the tradition or externally imposed.').

omega_variable(
    coercion_load_bearing,
    'Does the settlement command assent on its merits within the tradition, or does its persistence depend on enforcement machinery (anathema, credential control, admission gating)?',
    'Compare doctrinal retention in environments with weak enforcement capacity versus strong (pluralist religious markets versus credally gated institutions); measure voluntary assent rates where exit is cheap.',
    'If coercion is load-bearing, the contemporary suppression scalar understates the arrangement''s dependence on enforcement — the falling suppression_requirement series reflects capacity loss, not reduced need — and classification weights toward the snare side of tangled_rope. If assent survives cheap exit, the rope side is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_load_bearing, empirical, 'Whether enforcement machinery is constitutive of the settlement''s persistence or incidental to it.').

omega_variable(
    clergy_suppression_mechanism,
    'For credentialed clergy and theologians, is the suppression of dissent structural (credential revocation, employment loss, exclusion) or internalized (identity fusion with confessional allegiance)?',
    'Post-defection trajectory study: track clergy who leave Trinitarian communions for non-Trinitarian ones — if dissent capacity and self-understanding recover after exit, suppression was structural; if doubt-patterns persist, it was internalized.',
    'If substantially internalized, the identity_locked exit atom understates the seat''s effective exposure — the target carries the enforcement with them — and effective extraction for this seat runs higher than credential data suggest; the classification consequence concentrates on the clergy seat rather than the story-level type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clergy_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism for the identity-locked professional seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__trinitarian_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(bibl_tr_t4, biblical_divine_nature__trinitarian_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(bibl_tr_t8, biblical_divine_nature__trinitarian_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(bibl_tr_t12, biblical_divine_nature__trinitarian_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(bibl_tr_t16, biblical_divine_nature__trinitarian_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(bibl_tr_t20, biblical_divine_nature__trinitarian_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(bibl_tr_t24, biblical_divine_nature__trinitarian_reading, theater_ratio, 24, 0.32).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__trinitarian_reading, base_extractiveness, 0, 0.74).
narrative_ontology:measurement(bibl_be_t4, biblical_divine_nature__trinitarian_reading, base_extractiveness, 4, 0.72).
narrative_ontology:measurement(bibl_be_t8, biblical_divine_nature__trinitarian_reading, base_extractiveness, 8, 0.76).
narrative_ontology:measurement(bibl_be_t12, biblical_divine_nature__trinitarian_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(bibl_be_t16, biblical_divine_nature__trinitarian_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(bibl_be_t20, biblical_divine_nature__trinitarian_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(bibl_be_t24, biblical_divine_nature__trinitarian_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__trinitarian_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(bibl_su_t4, biblical_divine_nature__trinitarian_reading, suppression_requirement, 4, 0.82).
narrative_ontology:measurement(bibl_su_t8, biblical_divine_nature__trinitarian_reading, suppression_requirement, 8, 0.84).
narrative_ontology:measurement(bibl_su_t12, biblical_divine_nature__trinitarian_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(bibl_su_t16, biblical_divine_nature__trinitarian_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(bibl_su_t20, biblical_divine_nature__trinitarian_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(bibl_su_t24, biblical_divine_nature__trinitarian_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the doctrine of God' decomposes per the ε-invariance principle into three readings of the biblical_divine_nature kernel, each with its own ε, victim set, and enforcement structure. This file is the upstream member (highest institutional entrenchment, longest enforcement record); the unitarian and modalist readings are downstream competitors whose viability conditions this reading's enforcement machinery has historically shaped (anathema, admission gating). Each family member links the others through affects_constraints; cross-family comparison of computed classifications is the intended consumption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
