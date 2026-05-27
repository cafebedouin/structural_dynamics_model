% ============================================================================
% CONSTRAINT STORY: conception_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conception_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conception_reading
 *   human_readable: Moral Personhood Begins at Conception (Rights-Bearing Status)
 *   domain: moral_philosophy/bioethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested personhood
 *   boundary kernel — the reading that moral personhood begins at conception
 *   (fertilization). Under this reading, the zygote/embryo/fetus is a
 *   rights-bearing person from the moment of genetic fusion, entitling it to
 *   legal protection and bodily autonomy rights; abortion is homicide; the
 *   pregnant person's body is the locus of conflict between two persons'
 *   rights, with fetal rights taking priority. This reading is one of three
 *   structurally distinct constraints that share a common kernel (the
 *   personhood boundary) but instantiate different readings (conception,
 *   viability, birth) that produce different victim sets, different
 *   beneficiary structures, and different suppressiveness profiles. The
 *   conception reading exhibits tangled_rope structure: it coordinates fetal
 *   legal status into existing personhood/rights frameworks (coordination
 *   function) while simultaneously extracting from pregnant persons through
 *   subordination of bodily autonomy (asymmetric extraction). The
 *   constraint's extractiveness (0.68) has accumulated over the 50-year
 *   measurement interval as enforcement apparatus has grown (criminal
 *   abortion laws, prosecutorial practice, fetal-monitoring infrastructure)
 *   and as medical capacity to visualize fetal development has increased
 *   (ultrasound, fetal imaging) — raising the salience of fetal claims.
 *   Theater ratio remains relatively stable (0.38–0.42) because the reading's
 *   core claim (genetic continuity = personhood) is intellectually
 *   straightforward and does not require elaborate performative
 *   justification; the theater that does exist is in the regulatory/legal
 *   apparatus (fetal-protection infrastructure) rather than in the doctrine
 *   itself.
 *
 * KEY AGENTS:
 *   - Pregnant Persons: Primary victim (powerless/trapped) — body is the site of constraint enforcement; bodily autonomy is subordinated to fetal personhood claims; no exit options within the framework
 *   - Women as Collective: Secondary victim (moderate/constrained) — experience both coordination (pregnancy support, medical care) and extraction (autonomy subordination, reproductive decision-making removal); exit is constrained (contraception, migration possible but costly)
 *   - Anti-Abortion Movement and Religious Authorities: Primary beneficiary (institutional/arbitrage) — gain institutional power, legal formalization of their moral doctrine, state enforcement of their worldview; arbitrage exit is political (organizing, lobbying, state legislative change)
 *   - State Enforcement Apparatus: Institutional beneficiary/executor (institutional/constrained) — enforces the reading through criminal law, creates fetal-protection regulatory infrastructure; constrained exit (political path dependence, social cost of abandonment)
 *   - Moral Philosophers and Bioethicists: Scholarly commentators (powerful/mobile) — produce elaborate arguments for/against the reading; experience the constraint as degraded theater (arguments recycled without convergence); mobile exit through career flexibility
 *   - Reproductive Autonomy Movement: Organized opposition (organized/constrained) — building alternative frameworks (viability reading, birth reading, bodily autonomy doctrine); constructing institutional exit pathways (cross-border access, medication abortion, civil disobedience); experience constraint as temporary scaffold with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as metaphysical natural law; sees fetal organism-hood as possibly natural but ignores beneficiary structure indicating construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conception_reading, 0.68).
domain_priors:suppression_score(conception_reading, 0.78).
domain_priors:theater_ratio(conception_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conception_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(conception_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(conception_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conception_reading, tangled_rope).
narrative_ontology:human_readable(conception_reading, "Moral Personhood Begins at Conception (Rights-Bearing Status)").
narrative_ontology:topic_domain(conception_reading, "moral_philosophy/bioethics/constitutional_law").

domain_priors:requires_active_enforcement(conception_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(conception_reading, '5354b626-f639-446a-9690-a2a9a589f33d').
narrative_ontology:cs_created_at('5354b626-f639-446a-9690-a2a9a589f33d', '').
narrative_ontology:cs_kernel_codification('5354b626-f639-446a-9690-a2a9a589f33d', fixed_text).
narrative_ontology:cs_authority_grounding('5354b626-f639-446a-9690-a2a9a589f33d', lineage).
narrative_ontology:cs_interpretation_layer_present('5354b626-f639-446a-9690-a2a9a589f33d').
narrative_ontology:cs_kernel_id(conception_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('5354b626-f639-446a-9690-a2a9a589f33d', viability_reading, coexists_with).
narrative_ontology:cs_reading_relation('5354b626-f639-446a-9690-a2a9a589f33d', birth_reading, coexists_with).
narrative_ontology:cs_axiom('5354b626-f639-446a-9690-a2a9a589f33d', foundational, genetic_continuity_personhood).
narrative_ontology:cs_axiom_status(genetic_continuity_personhood, holdable).
narrative_ontology:cs_axiom_grounding('5354b626-f639-446a-9690-a2a9a589f33d', genetic_continuity_personhood, empirically_contingent).
narrative_ontology:cs_axiom('5354b626-f639-446a-9690-a2a9a589f33d', secondary, potentiality_confers_status).
narrative_ontology:cs_axiom_status(potentiality_confers_status, holdable).
narrative_ontology:cs_axiom_grounding('5354b626-f639-446a-9690-a2a9a589f33d', potentiality_confers_status, deontological).
narrative_ontology:cs_reference_frame('5354b626-f639-446a-9690-a2a9a589f33d', natural_law_ensoulment_doctrine).
narrative_ontology:cs_drift_state('5354b626-f639-446a-9690-a2a9a589f33d', contemporary_secular_pluralism, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conception_reading, fetal_moral_status_claimants).
narrative_ontology:constraint_beneficiary(conception_reading, state_enforcement_apparatus).
narrative_ontology:constraint_victim(conception_reading, pregnant_persons).
narrative_ontology:constraint_victim(conception_reading, women_reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (SNARE) — From the perspective of a pregnant person whose body is the physical site of the constraint, this reading imposes maximum extraction. Bodily autonomy is subordinated to fetal personhood claims. Exit options are trapped: gestation cannot be halted without violating the fetal person's claimed right to life; the pregnant person's biological processes are conscripted into service of the fetal entity. Suppression is extreme — legal penalties for abortion, social coercion, lack of realistic alternatives. No coordination benefit accrues to the pregnant person; the constraint is pure extraction.
constraint_indexing:constraint_classification(conception_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMEN AS COLLECTIVE (TANGLED ROPE) — Women as a group benefit from some coordination functions (pregnancy support infrastructure, medical care frameworks, legal recognition of maternal rights during gestation) while experiencing asymmetric extraction (bodily autonomy subordination, reproductive decision-making removed). Exit options are constrained: women can refuse pregnancy through contraception or migration, but face significant costs (economic, social, legal variance by jurisdiction). The constraint contains genuine coordination (pregnancy is medically and socially managed through institutions) layered with extraction (the management framework subordinates pregnant persons' autonomy to fetal claims).
constraint_indexing:constraint_classification(conception_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTI-ABORTION MOVEMENT / RELIGIOUS AUTHORITIES (ROPE) — From the perspective of institutional actors (religious organizations, pro-life advocacy groups, state enforcement apparatus that adopts this reading) this constraint is experienced as pure coordination: it formalizes and protects their interpretation of personhood boundaries, enforces their moral framework through law, and enables their institutional continuity. These beneficiaries have arbitrage options: they can exit state enforcement (through federal/state political change) but maintain ideological commitment regardless. The constraint coordinates their institutional interests with state power. Exit options remain open at the political level (lobbying, organizing), making this perspective experience the constraint as Rope rather than pure extraction.
constraint_indexing:constraint_classification(conception_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ENFORCEMENT (TANGLED ROPE) — State apparatus that enforces this reading experiences both coordination and extraction. The reading coordinates fetal legal personhood with existing criminal and tort law frameworks (extending existing person-hood protections). But the state also faces constraints: enforcing this reading requires surveillance of pregnancy and abortion, monitoring bodily autonomy, and managing the conflict between fetal and pregnant-person rights — creating administrative burden and social friction. Exit is constrained: the state cannot easily abandon this framework once institutionalized (political cost, institutional path dependence) but can modify implementation. The state both benefits from coordination (legal coherence) and bears costs (enforcement burden, social conflict).
constraint_indexing:constraint_classification(conception_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MORAL PHILOSOPHERS (PITON) — Academic engagement with personhood doctrine is substantially theatrical. Philosophers produce elaborate arguments for and against conception-as-personhood (potentiality arguments, continuity arguments, relational personhood critiques) with high intellectual content but declining real-world causal force. The academic debate persists through institutional inertia (journal publications, tenure systems, conference participation) rather than through genuine epistemic productivity — the arguments have been rehearsed for decades with no convergence. Theater ratio is high (0.68 on average): the academic apparatus produces scholarly output that maintains the appearance of unresolved philosophical debate while policy-makers increasingly ignore philosophical nuance. Philosophers have mobile exit options (career flexibility, international scholarship), so experience this as degraded rather than extractive.
constraint_indexing:constraint_classification(conception_reading, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REPRODUCTIVE AUTONOMY MOVEMENT (SCAFFOLD) — Organized agents (abortion-rights advocacy, feminist bioethicists, medical provider organizations) see this constraint as a temporary institutional configuration with real sunset dynamics. They are building alternative legitimacy frameworks (bodily autonomy doctrine, viability-based personhood readings, birth-based readings) and alternative institutional pathways (cross-border abortion access, medication abortion networks, civil disobedience infrastructure). The constraint has suppression (0.78) but organized actors experience lower effective extraction because they perceive and are constructing exit pathways. This is a genuine scaffold: suppression is real and high, but the scaffolding time-horizon (1-2 generations) reflects that alternative institutional frameworks (international law, medical autonomy, pluralistic personhood doctrine) are replacing the conception-reading's monopoly on policy authority.
constraint_indexing:constraint_classification(conception_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / METAPHYSICAL NATURALISM (MOUNTAIN) — From a civilizational/universal analytical perspective that treats personhood as a natural metaphysical fact rather than a socially constructed legal status, conception-based personhood appears as a natural law: if personhood is an intrinsic property instantiated by biological organisms, and if the zygote is a distinct biological organism with human DNA, then personhood at conception follows necessarily. This perspective treats the reading as emerging from natural metaphysical facts about organism individuation and genetic continuity. However, the structural data contradicts the mountain classification: the beneficiary/victim structure, enforcement apparatus, and suppression mechanism all indicate this is a socially constructed constraint with identifiable beneficiaries. The engine's false summit detector will identify this as naturalization of a contingent doctrinal reading.
constraint_indexing:constraint_classification(conception_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conception_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conception_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conception_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conception_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(conception_reading, TR),
    TR >= 0.70.

:- end_tests(conception_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and accumulating. The constraint extracts significantly from pregnant persons (bodily autonomy subordination, reproductive decision-making removed, legal liability for abortion) and moderately from women as a collective (medical infrastructure imposes monitoring burdens, legal risk creates behavioral suppression). The extractiveness has increased from 0.45 (1970) to 0.68 (2020) as enforcement infrastructure grew and fetal imaging technology increased salience of the fetal entity. The accumulation reflects not changes in the doctrine itself (conception as personhood boundary is stable) but changes in enforcement capacity and institutional elaboration. Suppression (0.78): Very high. Barriers to exit include criminal penalties for abortion (where enforcement is active), social coercion (family pressure, institutional shaming, healthcare provider refusal), medical infrastructure constraints (abortion access varies by jurisdiction), and legal ambiguity (some jurisdictions treat abortion as homicide). These barriers are structural and significant. However, suppression is not total globally — some jurisdictions permit abortion, medication abortion networks operate in restricted areas, and cross-border access is possible. Theater ratio (0.42): Moderate. The reading's core claim (genetic continuity = personhood) is intellectually straightforward and does not require elaborate justification. The theater that exists is in regulatory/legal apparatus (fetal-protection bureaucracy, criminal prosecution procedures, fetal-rights litigation) rather than in the doctrine itself. The reading is simpler than competitor readings (viability requires complex threshold determination; birth requires arbitrary line-drawing justification). Claimed type (Tangled Rope): Justified by presence of both coordination function (fetal personhood integrates into existing rights frameworks, creates legal coherence) and asymmetric extraction (pregnant persons bear disproportionate cost, their autonomy is subordinated). Active enforcement required (true): the reading depends on legal and social enforcement of fetal claims against pregnant-person autonomy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Pregnant persons experience Snare (pure extraction, no coordination benefit, trapped exit). Women as collective experience Tangled Rope (mixed coordination and extraction, constrained exit). Anti-abortion institutional actors experience Rope (pure coordination of their moral doctrine with state power, beneficiary position with arbitrage exit). State apparatus experiences Tangled Rope (coordination of legal frameworks plus enforcement burden). Philosophers experience Piton (scholarly theater with declining real-world force). Reproductive autonomy movement experiences Scaffold (temporary constraint with building exit pathways). Analytical observer risks Mountain (naturalizing contingent doctrine as metaphysical fact). The perspectival gap reveals that the same structural phenomenon — enforcement of conception-as-personhood doctrine — appears as beneficiary coordination, victim extraction, institutional burden, scholarly theater, and alternative-framework construction depending on position. No single type is 'correct'; the presheaf over the observation site IS the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from the agent's structural relationship to this specific constraint. Pregnant persons: beneficiary status = no (they bear costs), exit_options = trapped → d ≈ 0.95, f(d) ≈ 1.42 (maximum extracted). Women collective: mixed beneficiary/victim (coordination benefits, extraction costs), exit_options = constrained → d ≈ 0.65, f(d) ≈ 1.00 (moderate). Anti-abortion institutions: beneficiary status = yes, exit_options = arbitrage → d ≈ 0.10, f(d) ≈ -0.01 (negative effective extraction — they benefit). State apparatus: beneficiary (gains enforcement authority) and victim (enforcement burden), exit_options = constrained → d ≈ 0.50, f(d) ≈ 0.65 (moderate). The engine's derivation chain converts these d values through the sigmoid function to produce chi = ε × f(d) × σ(S). Scope (national, σ=1.0) does not amplify chi beyond the base rate. Extractiveness is then indexed to each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED (ε > 0.70 → required): This constraint demonstrates why mandatrophy resolution is necessary for high-extraction constraints. The classification appears to shift radically across perspectives: Snare → Tangled Rope → Rope → Scaffold → Piton → Mountain. The mandatrophy (apparent contradiction) is RESOLVED by recognizing that all six types are legitimate perspectival readings of the same base properties, not contradictions. The resolution follows from the indexical framework: classification IS perspectival. There is no single 'correct' type; instead, there is a presheaf of classifications indexed to (P,T,E,S) contexts. The pregnant person and institutional beneficiary do not disagree about facts — they disagree about what those facts mean from their structural position. Mandatrophy resolution documents that this perspectival plurality is the correct analysis, not a failure of classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organism_individuation_criterion,
    'What criterion determines when a biological organism begins? Is it genetic uniqueness, cellular organization, metabolic autonomy, or implantation/developmental milestone?',
    'Philosophical and biological analysis: does genetic uniqueness (achieved at fertilization) constitute organism-hood, or does organizational/metabolic autonomy (achieved later) constitute the boundary? Cross-disciplinary consensus on developmental biology.',
    'If genetic criterion is metaphysically necessary: conception reading is mountain-adjacent (natural law). If organizational criterion is correct: organism-hood begins later (viability or birth reading becomes natural law, conception reading becomes constructed constraint). If multiple criteria are equally valid: personhood boundary is fundamentally ambiguous (both readings coexist, neither forecloses).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(organism_individuation_criterion, conceptual, 'Metaphysical criterion for organism individuation at conception vs later stages').

omega_variable(
    sentience_consciousness_necessity,
    'Is sentience or consciousness metaphysically necessary for personhood, or is genetic humanity sufficient? If necessary, when does it emerge?',
    'Neuroscience of fetal development (thalamocortical connectivity, integrated neural activity). Philosophy of mind: does personhood require phenomenal consciousness, or only human genetic status? Empirical determination of fetal neurological development timeline.',
    'If sentience is necessary and absent at conception: conception reading loses core justification (personhood requires consciousness the zygote lacks). If genetic status is sufficient: conception reading is strengthened. If question is conceptually underdetermined: both readings remain coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_consciousness_necessity, empirical, 'Whether sentience/consciousness is necessary for personhood and fetal emergence timeline').

omega_variable(
    potentiality_versus_actuality,
    'Does potential personhood (what the zygote might become) confer actual personhood status, or must personhood be actualized? What is the metaphysical difference between potential and actual attributes?',
    'Philosophy of metaphysics: analysis of potentiality doctrine in Aristotelian and contemporary metaphysics. Empirical analysis: do other potential-but-not-actual statuses (a sperm cell has potential personhood if combined with egg) ground rights claims at the potentiality stage?',
    'If potentiality confers actual status: conception reading is grounded. If potentiality and actuality are metaphysically distinct: conception reading is weakened (zygote has potential personhood, not actual). If potentiality doctrine is incoherent: all readings must ground personhood in actual properties (sentience, relational capacity, birth-stage organization).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(potentiality_versus_actuality, conceptual, 'Metaphysical status of potentiality vs. actuality in personhood claims').

omega_variable(
    kernel_reading_incommensurability,
    'Are the conception reading and viability/birth readings incommensurable framings grounded in different moral foundations (deontological fetal rights vs consequentialist pregnant-person welfare vs relational personhood constitution), or do they differ only on empirical facts about development and could in principle be unified?',
    'Meta-ethical analysis: are the axioms grounding each reading (absolute_fetal_personhood, bodily_autonomy_primacy, relational_personhood_constitution) mutually foreclosing or merely competing? Empirical resolution: could all parties accept a unified framework if empirical facts about development/sentience/relational status changed?',
    'If grounded in different meta-ethics: readings coexist indefinitely (neither forecloses). If empirically resoluble: one reading becomes natural law once facts are settled. If framework-level incommensurable: kernel is fundamentally contested at the deepest structural level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether personhood-boundary readings are metaphysically incommensurable or empirically resolvable').

omega_variable(
    pregnant_person_extractive_mechanism,
    'Is the suppression of pregnant-person autonomy (0.78) an inherent consequence of fetal personhood doctrine, or is it a contingent social/legal implementation choice?',
    'Comparative constitutional law: do jurisdictions that recognize fetal personhood always suppress pregnant-person autonomy, or do some frameworks protect both fetal status AND pregnant-person bodily autonomy through institutional structures (e.g., mandatory fetal-support infrastructure, no-criminal-prosecution frameworks)? Hypothetical: could fetal personhood be instantiated without extraction from pregnant persons?',
    'If suppression is inherent: conception reading''s tangled_rope classification is strengthened (extraction is structural). If suppression is contingent: could become pure Rope if implementation changed (coordination without extraction). Implementation changes affect chi but not the underlying reading''s doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pregnant_person_extractive_mechanism, empirical, 'Whether autonomy suppression is inherent to fetal personhood or contingent implementation').

omega_variable(
    false_summit_detection_marker,
    'Is the conception reading grounded in metaphysical natural law (organism individuation, genetic continuity) or in socially constructed legal doctrine that naturalizes contingent institutional arrangements?',
    'Counterfactual: if no society had ever claimed conception-as-personhood, would the metaphysical facts compel this reading, or is the reading adopted through institutional choice? Historical analysis: is conception personhood universal across cultures or culture-specific? Philosophical analysis: do the natural-law axioms (genetic_continuity_personhood) survive cross-cultural philosophical scrutiny or reflect particular Western metaphysical traditions?',
    'If natural law: conception reading is mountain-adjacent (false summit detector should not trigger). If constructed doctrine: false summit signature fires (beneficiaries present, emergent_naturally false but claimed true) and reading reclassifies to snare/tangled_rope in engine analysis. This is the cardinal uncertainty about the reading''s metaphysical status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection_marker, conceptual, 'Whether conception personhood is natural law or naturalized institutional doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conception_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(conception_theater_1970, conception_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(conception_theater_1995, conception_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(conception_theater_2020, conception_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(conception_extractiveness_1970, conception_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(conception_extractiveness_1995, conception_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(conception_extractiveness_2020, conception_reading, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conception_reading, identity_coordination).
narrative_ontology:affects_constraint(conception_reading, viability_reading).
narrative_ontology:affects_constraint(conception_reading, birth_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three structurally distinct constraints: conception_reading (ε=0.68, Tangled Rope, high victim suppression), viability_reading (ε variable by implementation, Tangled Rope or Scaffold), birth_reading (ε variable by implementation, possibly Rope or Mountain if birth is treated as natural boundary). Each reading is a separate constraint because each has different extractiveness, different victim/beneficiary sets, different perspectival classifications. The three stories are linked via the kernel: they are competing answers to the same constitutional question. The engine's network propagation can model how shifts in one reading (e.g., viability_reading gaining institutional force) affect the others' structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
