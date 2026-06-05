% ============================================================================
% CONSTRAINT STORY: communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_communal_autonomy_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: communal_autonomy_reading
 *   human_readable: Family Law Authority as Communal Autonomy (Religious Community Tradition Reading)
 *   domain: constitutional_law/legal_pluralism
 *
 * SUMMARY:
 *   This constraint models family law authority from the communal autonomy
 *   reading: religious communities claim authority to regulate family law
 *   (marriage, divorce, child custody, inheritance) according to their own
 *   traditions, with the state role limited to recognition and enforcement of
 *   community decisions rather than regulation or override. This reading is
 *   deeply embedded in pluralist constitutional traditions (Canada, Malaysia,
 *   Israel, India) where religious communities maintain parallel family law
 *   systems. The constraint exhibits classic tangled-rope structure: communal
 *   systems provide genuine coordination functions (conflict resolution,
 *   identity preservation, community cohesion) alongside systematic
 *   extraction (authority concentrated in male/elder leadership, women's
 *   rights limited by uncodified rules, exit barriers imposed through
 *   identity lock and social stigma). The theater ratio reflects that much
 *   communal family law operates through unwritten tradition and
 *   discretionary authority—enforcement is not fully transparent, allowing
 *   leadership discretion to determine outcomes. Extractiveness has increased
 *   over the 30-year interval as secular alternatives (state civil courts,
 *   women's education, economic independence) have made exit more visible and
 *   more desirable, causing communities to tighten enforcement of traditional
 *   rules to prevent defection.
 *
 * KEY AGENTS:
 *   - Religious community leadership (institutional/arbitrage): Benefits from authority over family law; experiences constraint as pure coordination; can shift to secular system when advantageous
 *   - Women under uncodified communal law (powerless/identity_locked): Structurally mobile but identity-fused with religious community; bears full cost of extraction; cannot perceive exit from within identity frame
 *   - Members seeking exit or reform (moderate/constrained): Face high exit costs but retain some agency; experience both coordination benefits and extraction
 *   - State regulatory authority (organized/constrained): Wants to respect communal autonomy but also protect vulnerable members; constrained by political feasibility
 *   - Women's rights and human rights organizations (powerful/mobile): See communal autonomy as temporary accommodation with sunset; work to build exit pathways and internal reform
 *   - International human rights system (institutional/arbitrage): Claims universal authority over family law but lacks enforcement; maintains performative commitment to human rights while recognizing state pluralism
 *   - Analytical observer (analytical/analytical): Risks naturalizing the constitutional paradox (universal rights + pluralism are incompatible) as an irreducible structural limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(communal_autonomy_reading, 0.48).
domain_priors:suppression_score(communal_autonomy_reading, 0.62).
domain_priors:theater_ratio(communal_autonomy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(communal_autonomy_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(communal_autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(communal_autonomy_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(communal_autonomy_reading, "Family Law Authority as Communal Autonomy (Religious Community Tradition Reading)").
narrative_ontology:topic_domain(communal_autonomy_reading, "constitutional_law/legal_pluralism").

domain_priors:requires_active_enforcement(communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(communal_autonomy_reading, '18d06d06-f226-4eb5-99da-f3adc99332be').
narrative_ontology:cs_created_at('18d06d06-f226-4eb5-99da-f3adc99332be', '').
narrative_ontology:cs_kernel_codification('18d06d06-f226-4eb5-99da-f3adc99332be', distributed).
narrative_ontology:cs_authority_grounding('18d06d06-f226-4eb5-99da-f3adc99332be', lineage).
narrative_ontology:cs_interpretation_layer_present('18d06d06-f226-4eb5-99da-f3adc99332be').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(communal_autonomy_reading, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(communal_autonomy_reading, community_leadership_elites).
narrative_ontology:constraint_victim(communal_autonomy_reading, women_under_uncodified_systems).
narrative_ontology:constraint_victim(communal_autonomy_reading, exit_seeking_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER UNCODIFIED COMMUNAL LAW (SNARE) — Structurally mobile (could theoretically exit the community) but identity-locked through religious, familial, and social identity fusion. Cannot perceive exit as thinkable from within the identity frame. Experiences high suppression through cultural norms, family pressure, and stigma of apostasy/excommunication. Community authority derives legitimacy from tradition; no written rules codify rights. Extraction runs toward community leadership; bearing of costs (unequal marriage dissolution, child custody, inheritance restrictions) falls entirely on this agent.
constraint_indexing:constraint_classification(communal_autonomy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: MEMBERS SEEKING EXIT OR REFORM (TANGLED ROPE) — Face high exit costs (family rupture, community exclusion, loss of identity-anchored social networks) but retain some agency through secular law, legal aid, or exit to secular civil marriage. Experience genuine coordination benefits (communal conflict resolution, identity preservation, social stability) alongside extraction (authority structures enforce traditional rules that concentrate power in male/elder leadership). Moderate power enables some negotiation or exit but at significant cost.
constraint_indexing:constraint_classification(communal_autonomy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS COMMUNITY LEADERSHIP (ROPE) — Perceives the constraint as pure coordination: communal autonomy enables the community to govern family law according to its own traditions without state interference. Leadership benefits from authority over law-making and dispute resolution; arbitrage option enables leadership to move between communal and secular systems as advantageous. Experiences the constraint as legitimate cultural self-determination and effective coordination mechanism for preserving community identity and internal stability. No extraction burden; full beneficiary position.
constraint_indexing:constraint_classification(communal_autonomy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE REGULATORY AUTHORITY (TANGLED ROPE) — Sees the constraint as coordination (respecting communal autonomy, avoiding religious persecution, enabling cultural pluralism) PLUS extraction (state's regulatory capacity is limited; enforcement of state protections within communal systems is constrained; state cannot access internal community processes to verify compliance with minimum protections). State wants to respect autonomy but also wants to protect vulnerable members. Experiences coordination benefit (avoids explosive church-state conflict) and extraction cost (women's rights protections may not reach communities that reject state jurisdiction). Constrained by political feasibility and institutional limits.
constraint_indexing:constraint_classification(communal_autonomy_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WOMEN'S RIGHTS AND HUMAN RIGHTS ORGANIZATIONS (SCAFFOLD) — See the communal autonomy reading as a temporary accommodation with a sunset: as educational access, secular employment, and legal literacy increase within communities, women's capacity to exit and advocate for reform grows. Organizations work to establish codified communal law (bringing transparency and written rights), strengthen secular law backup (enabling exit), and build internal reform movements. Scaffold classification reflects genuine exit pathway (women organizing within communities and via secular courts) and declining effectiveness of pure community enforcement as literacy and economic independence rise. High mobility; constraints are surmountable through organizing and legal strategies.
constraint_indexing:constraint_classification(communal_autonomy_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL HUMAN RIGHTS SYSTEM (PITON) — Treaty obligations and UN declarations frame family law authority as a universal state responsibility, not a communal right. However, enforcement is substantially performative: states sign human rights conventions while protecting communal autonomy in practice; international bodies issue recommendations without enforcement mechanisms; theater persists because the international system wants both universal human rights AND cultural pluralism without resolving the contradiction. The piton reflects that international law's claim to authority over family law is increasingly theatrical—stated but unenforced, maintained through institutional inertia and rhetorical commitment rather than functional override of communal systems.
constraint_indexing:constraint_classification(communal_autonomy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IRREDUCIBLE PLURALISM VIEW (MOUNTAIN) — From a civilizational/universal perspective, this appears as an irreducible structural limit: liberal constitutional orders claim universal human rights authority AND respect pluralism and cultural self-determination, but these commitments are fundamentally incompatible when communities claim authority over family law. No meta-authority can adjudicate between state law and communal law without privileging one over the other. The analytical observer risks seeing this incompatibility as a natural law of political order itself—a mountain. However, this perspective naturalizes what may be a contingent institutional arrangement (the liberal constitutional state's simultaneous commitment to universal rights and pluralism). The false-summit detector will flag this as a potential naturalization of a constructed constraint.
constraint_indexing:constraint_classification(communal_autonomy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(communal_autonomy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(communal_autonomy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(communal_autonomy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(communal_autonomy_reading, TR),
    TR >= 0.70.

:- end_tests(communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-to-high. The communal autonomy system produces clear beneficiaries (male/elder leadership) and clear victims (women with limited legal rights, members seeking exit). However, the constraint also provides genuine coordination benefits—communal family law resolves disputes, preserves cultural identity, and maintains community cohesion. The extractiveness value reflects both the coordination function (keeping it moderate rather than high) and the asymmetric power structure (preventing it from being low). The value increased over 30 years as secular alternatives became visible, causing communities to enforce traditional rules more strictly to prevent exit. Suppression (0.62): Moderate-high. Uncodified rules give community authority discretionary power; identity-based compliance is enforced through family pressure, religious framing, and stigma of exit (apostasy, family rupture, community exclusion). However, suppression is not total—some members do exit via secular courts, secular marriage, or geographic relocation. State legal backup exists but is often invisible within communities that discourage engagement with secular law. Theater ratio (0.58): Moderate. Communal family law operates through mixture of genuine social function (conflict resolution, community cohesion) and performative ritual (emphasis on tradition, discretionary authority, written law absent or unenforced). Theater has increased as education and legal literacy have made the discretionary nature of enforcement more visible. Codification (written family law) could decrease theater by making rules explicit, but current state (mostly oral tradition with discretionary enforcement) produces moderate theatrical component.
 *
 * PERSPECTIVAL GAP:
 *   The communal autonomy reading produces the widest perspectival gap because it structurally positions different agents with radically different exit costs and power asymmetries. The leadership's arbitrage option (can engage or disengage with secular system as advantageous) stands in sharp contrast to the identity-locked agent's non-option (exit requires identity death). This contrast is the diagnostic signal that the constraint is genuinely a mixed extraction-coordination system. A pure rope (coordination only) would show much narrower perspectival gaps because all agents would perceive mutual benefit. A pure snare (extraction only) would show narrower gaps from different direction (all agents see extraction but from different power positions). The wide gap here reveals the hybrid: leadership sees only coordination; victims see only extraction; middle positions see both.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power level, exit options, and beneficiary/victim relationship. Religious leadership: institutional power + arbitrage exit + beneficiary status → d ≈ 0.10 (full beneficiary) → low effective extraction (negative χ). Women under identity lock: powerless + identity_locked exit + victim status → d ≈ 0.92 (near-full target) → high experienced extraction. Members seeking exit: moderate power + constrained exit + mixed beneficiary/victim → d ≈ 0.55 (symmetric, slight victim bias) → moderate extraction. State authority: organized power + constrained exit (political/institutional limits) + mixed beneficiary/victim → d ≈ 0.50 (symmetric) → moderate extraction. Women's rights organizations: powerful + mobile + victim advocacy → d ≈ 0.70 (target perspective) but high analytical power moderates to moderate extraction. International system: institutional power + arbitrage exit but analytical/performative role → d ≈ 0.72 (observer position) → moderate extraction. The analytical observer's mountain perspective has no directionality (analytical context is universal observation).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: This constraint resolves the mandatrophy by explicitly instantiating ONE reading of the contested family_law_authority kernel. The mandatrophy question is: 'What grounds family law authority—communal tradition, constitutional state, or hybrid negotiation?' This constraint answers: communal tradition (and no other reading). This answer is structurally coherent: if communal tradition grounds authority, then victims are those outside the community authority structure (women, exit seekers), beneficiaries are those controlling tradition (leadership elites), and state role is recognition not regulation. Different readings (constitutional supremacy, hybrid accommodation) would reclassify the victims, beneficiaries, and state role. This reading is not falsified by the piton and mountain perspectives—those perspectives represent what happens when other readings (international human rights supremacy, irreducible pluralism) become salient. The mandatrophy resolves because the presheaf of perspectives across different readings instantiates different constraint structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Is this constraint best understood as communal autonomy (this reading), constitutional supremacy of state law, or hybrid accommodation of both?',
    'Comparative institutional analysis across legal systems (Islamic family law courts in Malaysia/Indonesia, Jewish community rabbinical courts in Israel/diaspora, Christian communal law in Ethiopia/Eritrea, Hindu personal law in India). Which framing produces better predictive power for actual enforcement patterns, rights outcomes, and exit mechanisms?',
    'This reading assumes communal tradition as the legitimacy source; constitutional supremacy reading assumes state authority; hybrid accommodation reading assumes negotiated authority boundaries. Each produces different victim/beneficiary assignments and different ε values. If state constitutional supremacy is the correct reading: communal_autonomy_reading overestimates beneficiary extraction and underestimates state coercion. If hybrid accommodation is correct: this reading misses the dynamic negotiation between state and communal authorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which institutional reading of family law authority (communal autonomy vs. constitutional supremacy vs. hybrid) is most structurally accurate').

omega_variable(
    identity_lock_mechanism_scope,
    'What proportion of women''s non-exit is structural entrapment (economic dependency, legal disabilities, geographic isolation) versus internalized identity lock (religious identity fusion, self-concept constituted through communal roles)?',
    'Longitudinal study of exit patterns post-legal reform (when secular alternative becomes available). Do exit rates increase dramatically when structural barriers are removed (indicating prior identity lock was internalized), or do they remain low (indicating prior barriers were material)? Comparison: community members who received secular education vs. those without—do educational access break identity lock?',
    'If identity lock is primary: suppression rating (0.62) is high but reflects internalized mechanisms that persist even after state provides exit route; reclassify exit_options as more ''constrained'' than ''identity_locked'' if exit barriers are revealed as primarily material. If structural entrapment is primary: increase suppression rating and note that identity lock was secondary phenomenon emerging from constraint itself. Affects whether reform through codification (written communal law) or through secular legal backup is more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_scope, empirical, 'Relative weight of internalized identity lock vs. structural entrapment in non-exit patterns').

omega_variable(
    communal_enforcement_sustainability,
    'Does communal enforcement of family law persist primarily through genuine community consensus and identity-based compliance, or through coercion, stigma, and exit prevention?',
    'Ethnographic documentation of dispute resolution within communities (how conflicts are handled, what happens to dissenters, whether enforcement is consensual or coercive). Compare communities with high written law codification (clearer enforcement mechanisms) vs. those with oral tradition enforcement (more discretionary, more theater). Measure actual exit rates and barriers.',
    'If communal enforcement is consensus-based: the rope reading (pure coordination) has weight; reclassify community leadership as perceiving genuine coordination, lower suppression estimate. If enforcement is substantially coercive: increase suppression rating; reclassify as snare or pure extraction. If enforcement mixes consensus and coercion: confirm tangled_rope classification (genuine coordination plus extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(communal_enforcement_sustainability, empirical, 'Degree to which communal family law enforcement is based on genuine consensus vs. coercion and stigma').

omega_variable(
    state_recognition_asymmetry,
    'Does state recognition of communal family law actually neutrally accommodate communal autonomy, or does it systematically advantange certain authority structures (male-dominated leadership, elder control) within communities?',
    'Comparative analysis of which communal authorities the state recognizes and protects (official religious officials vs. informal community leaders vs. female judges/scholars). Analysis of whose interests state recognition serves: does it strengthen community as whole or entrench particular power structures? Examine cases where state had to choose whose authority to recognize.',
    'If state recognition is neutral: communal autonomy reading is structurally coherent. If state recognition systematically benefits male/elder authority: reclassify as state-enabled extraction mechanism (state is complicit in beneficiary group''s extraction from victims). Affects beneficiary classification: is the beneficiary ''religious community'' or ''male leadership elites''? Different classifications produce different χ computations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_recognition_asymmetry, empirical, 'Whether state recognition of communal family law neutrally accommodates autonomy or systematically entrenches particular internal power structures').

omega_variable(
    alternative_readings_classification_gap,
    'If constitutional supremacy reading or hybrid accommodation reading were instantiated as separate constraints, how would their ε and classification differ from this communal autonomy reading?',
    'Generate the sibling constraint stories (constitutional_supremacy_reading, hybrid_accommodation_reading) and compare base properties and classification type. Document where the readings diverge structurally and what structural facts would support each reading.',
    'This omega directly implements the committer frame rule: the kernel (_family_law_authority) is contested; this is ONE reading; sibling readings are OTHER constraints. The gap between readings is the signal that the constraint is a reading of a kernel, not a settled structural fact. If the three readings produce significantly different ε values (e.g., communal_autonomy ε=0.48, constitutional_supremacy ε=0.65, hybrid_accommodation ε=0.35), the kernel is genuinely contested at the structural level, not just rhetorically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_classification_gap, conceptual, 'Structural classification gap between the three family law authority readings (communal autonomy, constitutional supremacy, hybrid accommodation)').

omega_variable(
    theater_ratio_measurement_basis,
    'What counts as ''theatrical'' communal family law enforcement? Is codification (written rules) a sign of theater (legitimate function being replaced by ritual) or a sign of reduced theater (transparency and written rights reducing discretionary extraction)?',
    'Compare theater ratios across communities with high codification (family law written down, formal procedures) vs. those with oral tradition (discretionary, relational enforcement). If codification reduces theater: codified communal law is more functional. If codification creates theater: written rules mask discretionary power of authority figures. Measure by comparing stated rules to actual enforcement patterns.',
    'Theater ratio (0.58) assumes moderate performativity—some genuine coordination function but some irreducible discretionary authority. If codification research shows theater decreases sharply with written law: scaffold perspective (reform toward written law is sunset mechanism) gains strength. If codification increases theater: piton perspective (written law becomes ritual) gains strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_measurement_basis, empirical, 'Whether codification of communal family law reduces theater (increases functionality) or creates theater (replaces genuine community function with written ritual)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(communal_autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, communal_autonomy_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(comm_tr_t15, communal_autonomy_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(comm_tr_t30, communal_autonomy_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, communal_autonomy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comm_be_t15, communal_autonomy_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(comm_be_t30, communal_autonomy_reading, base_extractiveness, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(communal_autonomy_reading, constitutional_supremacy_reading).
narrative_ontology:affects_constraint(communal_autonomy_reading, hybrid_accommodation_reading).
narrative_ontology:affects_constraint(communal_autonomy_reading, religious_legal_pluralism_framework).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel has three structurally distinct readings instantiated as three separate constraints: communal_autonomy_reading (this story, ε≈0.48, tangled_rope dominant), constitutional_supremacy_reading (ε≈0.65, snare dominant from community perspective), and hybrid_accommodation_reading (ε≈0.32, rope/scaffold dominant). Each reading produces different victim/beneficiary assignments and different classification profiles. All three are linked to the parent constraint religious_legal_pluralism_framework which aggregates the kernel-level analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(communal_autonomy_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
