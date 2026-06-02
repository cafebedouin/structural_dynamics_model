% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Biblical Divine Nature: Unitarian Reading (Numerical Singularity of God)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The unitarian reading of biblical divine nature — that God is numerically
 *   singular, without internal relations or distinctions in the Godhead, with
 *   Father alone being God and Son/Spirit subordinate or created — is one
 *   coherent response to the biblical materials about divine monotheism and
 *   christological claims. This reading instantiates a specific constraint
 *   whose structure varies dramatically across observers. For the unitarian
 *   believer facing institutional suppression (historical context of
 *   religious establishments), the constraint is a snare: coercive
 *   enforcement with no coordination benefit. For the reformed unitarian
 *   community with institutional standing, the constraint is tangled rope:
 *   genuine coordination of flatter ecclesiology alongside asymmetric
 *   enforcement of doctrinal boundaries. For the trinitarian institutional
 *   hierarchy, the constraint is paradoxically a snare on its own authority:
 *   the existence of unitarian readings citing the same texts creates an
 *   unfalsifiable verification crisis that forces escalating enforcement.
 *   From a distance, the constraint appears as a natural law (logical
 *   inevitability of resolving monotheism-trinity tension), but the
 *   structural data reveals institutional coercion, not logical necessity.
 *   The measurement trajectory shows declining suppression over the interval
 *   (0.75 → 0.55) as religious establishment declines and pluralist
 *   frameworks expand, while theater ratio rises (0.48 → 0.58) as doctrinal
 *   claims become increasingly performative in post-Christendom contexts.
 *   This is a kernel-reading constraint: one interpretation of a contested
 *   commitment (biblical divine nature) that coexists with trinitarian and
 *   modalist readings.
 *
 * KEY AGENTS:
 *   - Unitarian Believers: Primary victim (powerless/trapped in suppression contexts) — face doctrinal prosecution, property loss, exile under institutional trinitarian orthodoxy; also primary beneficiary (moderate/constrained in reformed contexts) where unitarian community has institutional standing
 *   - Unitarian Institutional Authority: Primary beneficiary (institutional/arbitrage) — coordinates flat ecclesiology and doctrinal coherence; experiences constraint as pure coordination
 *   - Trinitarian Institutional Orthodoxy: Complex agent (institutional/constrained) — claims authority over doctrinal truth but faces extraction: must invest in suppression apparatus to maintain appearance of settled doctrine; forced into defensive coercion to protect authority structure
 *   - Reformed Unitarian Communities: Organized agents (organized/constrained) — achieve institutional recognition and build real coordination benefits alongside residual suppression mechanisms; see the constraint as temporary (scaffold logic)
 *   - Modern Ecumenical Coalition: Organized agents (organized/mobile) — treat unitarian and trinitarian readings as equally coherent theological responses; create exit pathways through pluralism and disestablishment; build alternative verification frameworks (inter-tradition dialogue) that bypass institutional doctrinal monopoly
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the constraint as inherent logical problem rather than institutional coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.38).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.62).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Biblical Divine Nature: Unitarian Reading (Numerical Singularity of God)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, 'unitarian-reading-001').
narrative_ontology:cs_kernel_codification('unitarian-reading-001', fixed_text).
narrative_ontology:cs_authority_grounding('unitarian-reading-001', extraction).
narrative_ontology:cs_interpretation_layer_present('unitarian-reading-001').
narrative_ontology:cs_reading_relation('unitarian-reading-001', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('unitarian-reading-001', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('unitarian-reading-001', foundational, monotheism_precludes_trinity).
narrative_ontology:cs_axiom_status(monotheism_precludes_trinity, holdable).
narrative_ontology:cs_axiom_grounding('unitarian-reading-001', monotheism_precludes_trinity, deontological).
narrative_ontology:cs_axiom('unitarian-reading-001', foundational, biblical_subordinationism_textually_primary).
narrative_ontology:cs_axiom_status(biblical_subordinationism_textually_primary, holdable).
narrative_ontology:cs_axiom_grounding('unitarian-reading-001', biblical_subordinationism_textually_primary, empirically_contingent).
narrative_ontology:cs_created_at('unitarian-reading-001', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_theological_traditions).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, anti_hierarchical_ecclesiology).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_institutional_orthodoxy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, hierarchical_ecclesiastical_authority).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_uniformity_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNITARIAN BELIEVER (SNARE) — In jurisdictions where trinitarian orthodoxy is institutionally enforced (historical Europe, ecclesiastical hierarchies), unitarian believers face active suppression: doctrinal prosecution, property confiscation, exile, or death. Exit from unitarian conviction would require cognitive apostasy — abandoning a reading they experience as biblically justified. The constraint extracts through coercion with minimal coordination benefit. High suppression, high extraction experienced by this agent.
constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REFORMED UNITARIAN COMMUNITY (TANGLED ROPE) — In contexts where unitarian communities have achieved institutional recognition (Transylvania 16th–18th centuries, modern liberal denominations), the constraint exhibits both genuine coordination function and asymmetric extraction. Unitarian ecclesiology coordinates flatter authority structures and participatory governance, enabling real coordination benefits. Simultaneously, the constraint still suppresses trinitarian interpretation within these communities and extracts institutional loyalty. Mixed structure — genuine function plus extraction.
constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNITARIAN INSTITUTIONAL AUTHORITY (ROPE) — The unitarian ecclesiastical hierarchy (where it has institutional power) experiences the constraint as pure coordination: maintaining doctrinal coherence, clarifying the biblically justified reading, coordinating teaching and practice. The constraint benefits this institution through clarity and internal cohesion. Arbitrage exit option reflects ability to shift reading if institutional incentives change. Net beneficiary.
constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRINITARIAN INSTITUTIONAL ORTHODOXY (SNARE) — The constraint, from the perspective of trinitarian institutional power, operates as a snare on its own doctrinal certainty. The existence of unitarian readings that cite the same biblical sources creates an unsolvable verification problem for trinitarian institutional claims of settled doctrine. Trinitarian authorities must expend enforcement resources (inquisition, doctrinal prosecution, exclusion) to maintain the appearance of unanimity that their claimed authority depends upon. The constraint is extractive from this perspective because it forces the trinitarian hierarchy to invest in suppression mechanisms that generate no internal coordination benefit — the investment is purely defensive. High suppression, no coordination function.
constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a distance, the constraint may appear to be a natural law: the tension between monotheistic claims and trinitarian metaphysics is an inherent logical contradiction that any theological system must resolve. Unitarianism resolves it toward strict numerical singularity; trinitarianism resolves it toward relational distinctions-in-unity. From this height, both readings may appear equally natural — reflecting the structure of the logical problem rather than institutional power. However, the structural data contradicts the mountain classification: the constraint's enforcement mechanisms, suppression patterns, and beneficiary/victim structure reveal it is an institutional commitment maintained through active coercion, not a logical necessity.
constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: MODERN ECUMENICAL COALITION (SCAFFOLD) — Contemporary ecumenical frameworks (interfaith dialogue, pluralist theology, post-Christendom contexts) increasingly treat unitarian and trinitarian readings as two coherent responses to the same biblical materials rather than as error and orthodoxy. This organized perspective sees the constraint as a temporary institutional artifact — one that had extractive force under religious establishment and coercive state power, but diminishes as institutional plurality and freedom of conscience expand. The sunset mechanism is disestablishment: as churches lose institutional monopoly on theological interpretation, the suppression apparatus decays and the constraint shifts from snare/tangled_rope to a residual identity marker (piton) within specific traditions.
constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biblical_divine_nature__unitarian_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits extraction mechanisms — suppression of unitarian interpretation, enforcement of orthodox teaching, coercive institutional control — but the extraction is not total and has declined over the measurement interval. Historical contexts with state-religious establishment (early modern Europe) show high extraction (0.52); modern contexts with disestablishment show lower extraction (0.38). The extractiveness reflects real suppression and enforcement costs, not just disagreement. Suppression (0.62): Moderate-high. Strong in early modern period (inquisitorial apparatus, property confiscation, exile), declining in modern pluralist contexts. The measurement trajectory (0.75 → 0.55) captures the decay of establishment religious authority and expansion of freedom of conscience. Theater ratio (0.58): Moderate-high. Trinitarian doctrinal claims (homoousios, perichoresis, divine simplicity) require increasingly abstract philosophical scaffolding to maintain coherence as empirical and hermeneutical challenges mount. Unitarian readings, by contrast, require fewer metaphysical maneuvers to align with plain textual monotheism. The rising theater ratio (0.48 → 0.58) reflects trinitarian doctrine becoming more performative as institutional establishment weakens and the appeal to apostolic authority loses force.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival logic of doctrinal constraints. The unitarian believer under suppression sees snare: coercive enforcement, no exit, no coordination benefit. The unitarian institutional leader sees rope: coordination of coherent theology and flatter governance. The trinitarian hierarchy sees snare on itself: forced to invest in suppression apparatus (inquisition, council decrees, doctrinal prosecution) to maintain the appearance of settled truth — and that appearance is the only thing holding institutional authority together. The analytical observer risks seeing mountain: treating the monotheism-trinity tension as an inherent logical problem. But the structural data contradicts this naturalization: the enforcement mechanisms, beneficiary structures, and measurement trajectories reveal the constraint as an institutional arrangement maintained through power, not a logical necessity. The unitarian reading 'works' as theological system (internally coherent, textually defensible, conceptually parsimonious); suppression is institutional coercion, not error correction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the constraint's extraction flow. Unitarian believers facing suppression (powerless/trapped) experience d ≈ 0.92 (full target), producing high f(d) ≈ 1.35, experienced chi near maximum despite moderate base extractiveness. Unitarian institutional leaders (institutional/arbitrage) experience d ≈ 0.08 (full beneficiary), producing negative f(d) ≈ -0.18, low or negative experienced chi. Trinitarian hierarchy (institutional/constrained) faces the paradox: they are nominally the beneficiary of the constraint (institutional authority over doctrinal truth) but structurally victimized by it (forced into costly enforcement). Their directionality is derived as d ≈ 0.58 (symmetric target-beneficiary), producing f(d) ≈ 0.65 (moderate experienced extraction). This asymmetry in experienced extraction across the institutional actors (beneficiary vs. constrained institutional) is diagnostic for false-summit detection on the mountain perspective: a true natural law would affect all agents symmetrically, not bifurcate institutional authority into enforcement burden vs. coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL-READING EXEMPLAR: The mandatrophy is resolved by distinguishing the reading (unitarian interpretation) from the kernel (biblical divine nature) from the institutional authority that enforces one reading as orthodoxy. The mandatrophy dissolves when we recognize: (1) The kernel is textually and logically under-determined — it admits multiple coherent readings. (2) The institutional authority's claim to settle the matter (via councils, creeds, enforcement) is not grounded in textual determinacy but in institutional power. (3) The constraint's extractiveness varies by perspective because different institutional positions experience the reading enforcement differently. (4) The constraint's classification (snare vs. rope vs. mountain) depends entirely on which institutional perspective is measuring. There is no 'true' classification — only the perspectival landscape showing how different structural positions relate to the enforcement of one reading over another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biblical_textual_sovereignty,
    'Does the biblical canon itself provide a determinate answer to whether God is numerical singularity or relational trinity, or is the text genuinely under-determined and therefore necessarily subject to interpretive authority claims?',
    'Textual analysis isolating core divine-nature passages (John 1:1, Colossians 1:15, 1 John 5:20, etc.) and determining whether they admit a single coherent interpretation or necessarily require interpretive tradition to adjudicate between alternative readings. If the texts admit multiple coherent readings, the constraint''s authority structure is dependent on tradition and institutional power, not on textual determinacy.',
    'If text is determinate: one reading is correct, the other false; the suppression of falsehood is justified (mountain-to-rope reframe). If text is under-determined: both readings are textually defensible; suppression of either reading is institutional coercion not doctrinal necessity (snare/tangled_rope classification holds). This is the foundational omega — all classification confidence depends on it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biblical_textual_sovereignty, conceptual, 'Whether biblical canon determines divine nature or requires interpretive authority').

omega_variable(
    institutional_authority_grounding,
    'What grounds trinitarian institutional authority''s claim to adjudicate the divine nature question? Textual fidelity, conciliar consensus (Nicaea 325), philosophical coherence, continuous tradition, or installed institutional power?',
    'Historical tracing of which authority source claims were operative at each stage: pre-Nicene church fathers cite tradition and interpretation; post-Nicene councils claim conciliar consensus; institutional hierarchies claim apostolic succession; modern dogmatics claim philosophical adequacy. Identify which source is doing the actual work of suppressing alternatives.',
    'If grounding is textual: challenge is exegetical (compete on interpretation). If grounding is conciliar: challenge is ecclesiological (compete on representation and consent). If grounding is institutional power: challenge is political (compete on legitimacy and coercion capacity). Different authority gradings produce different suppression mechanisms and different paths to constraint decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_authority_grounding, empirical, 'What grounds trinitarian institutional authority''s truth claim').

omega_variable(
    unitarian_reading_coherence,
    'Is the unitarian reading internally coherent as a theological system, or does it require ad hoc maneuvers to address the christological and pneumatological problems (divine agency of the Son, identity of the Spirit) that trinitarian metaphysics was constructed to solve?',
    'Theological analysis of unitarian christology (incarnation of a subordinate divine being vs. incarnation of the one God), pneumatology (status of the Holy Spirit), and soteriology (redemptive work). Compare internal tensions in unitarian systems (Arianism, Sabellianism, adoptionism, modern Unitarianism) vs. trinitarian systems (homoousios metaphysics, perichoresis doctrine, divine simplicity tensions).',
    'If unitarian reading is less coherent: trinitarian suppression may be justified on grounds of protecting against incoherent doctrine (snare reframes toward tangled_rope). If equally coherent: both readings offer internally consistent theological frameworks, and suppression is purely institutional (snare classification holds, false-summit risk for mountain perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unitarian_reading_coherence, conceptual, 'Internal coherence of unitarian theological system').

omega_variable(
    reading_kernel_under_determination,
    'The kernel ''biblical divine nature'' is itself contested — some readings take it to be the ontological status of God (singular substance vs. three hypostases), others take it to be the christological claim (nature of Christ''s divinity), still others take it to be the pneumatological claim (status of the Spirit). Which is the actual kernel, and does the reading selection change if the kernel is reframed?',
    'Trace the logical dependencies: does unitarian reading depend primarily on monotheistic consistency claims, or on christological claims about the Son''s status, or on claims about the Spirit''s agency? If a reframing of the kernel produces a different logical relationship between this reading and its siblings (e.g., christological reframing makes them coexist rather than foreclose), the kernel itself is under-determined and the reading_relations are context-dependent.',
    'If kernel is under-determined: reading_relations are conditional on which kernel framing is adopted. The omega should be recorded in both this story and all sibling stories, with mutual cross-reference. If kernel is stable: reading_relations are robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_under_determination, conceptual, 'Whether the ''biblical divine nature'' kernel is itself under-determined').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unitar_theater_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(unitar_theater_t3, biblical_divine_nature__unitarian_reading, theater_ratio, 3, 0.52).
narrative_ontology:measurement(unitar_theater_t6, biblical_divine_nature__unitarian_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(unitar_extract_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(unitar_extract_t3, biblical_divine_nature__unitarian_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(unitar_extract_t6, biblical_divine_nature__unitarian_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(unitar_suppression_req_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(unitar_suppression_req_t3, biblical_divine_nature__unitarian_reading, suppression_requirement, 3, 0.68).
narrative_ontology:measurement(unitar_suppression_req_t6, biblical_divine_nature__unitarian_reading, suppression_requirement, 6, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% This constraint (unitarian reading, ε=0.38) is part of the biblical_divine_nature kernel family. The trinitarian and modalist readings are separate constraints with potentially different ε values, reflecting their different institutional authority grounding and enforcement mechanisms. The family is linked not by causal influence but by structural interpretation of the same kernel. Decomposition follows ε-invariance: if measuring the constraint from the trinitarian institutional perspective vs. the unitarian institutional perspective produces substantially different ε values (which it does), they are separate constraint stories linked by network edges, not alternative measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__unitarian_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
