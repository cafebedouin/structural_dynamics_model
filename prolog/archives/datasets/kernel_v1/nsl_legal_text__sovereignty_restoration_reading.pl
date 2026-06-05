% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Constitutional Sovereignty Restoration (2019 Hong Kong Reading)
 *   domain: constitutional_law/political_sovereignty/security_governance
 *
 * SUMMARY:
 *   The National Security Law (NSL) enacted in Hong Kong in 2020 in response
 *   to 2019 pro-democracy unrest is interpreted in this story through the
 *   sovereignty_restoration_reading: the law represents a legitimate exercise
 *   of central state authority to restore constitutional order, maintain
 *   territorial integrity against foreign interference, and reestablish
 *   governance capacity after months of civil unrest that threatened
 *   stability and invited external meddling. Under this reading, the
 *   constraint targets political opposition framed as security threat,
 *   extracting political participation capacity from activists and opposition
 *   movements while providing genuine coordination benefits (rule of law
 *   restoration, end to street violence, clarity of legal boundaries) to the
 *   broader population. The reading declares that the law's vagueness in
 *   definitions (sedition, subversion, foreign collusion) reflects inherent
 *   difficulty in security law drafting, not intentional expansiveness. The
 *   reading further holds that some state capacity for internal security is
 *   structurally necessary — the constraint represents restoration of
 *   legitimate sovereign authority, not innovation in oppression. However,
 *   this reading is contested by two structural alternatives encoded in
 *   sibling constraints: the democratic_enclosure_reading (which interprets
 *   the law as targeting domestic political opposition regardless of foreign
 *   involvement) and the jurisdictional_capture_reading (which interprets the
 *   law as transferring enforcement authority away from elected institutions
 *   toward executive/security services). This story instantiates ONLY the
 *   sovereignty_restoration_reading and routes the contestation to omega
 *   variables.
 *
 * KEY AGENTS:
 *   - Central Government Authority (CPG/HKSAR): Primary beneficiary (institutional/arbitrage) — restores governing capacity, reestablishes rule of law, prevents foreign interference. Experiences constraint as legitimate coordination.
 *   - Political Opposition Activists: Primary victim (powerless/trapped) — subject to retroactive prosecution, vague definitions, chilling effect on assembly and speech. Lose political mobilization capacity.
 *   - Civil Society Organizations: Secondary victim (moderate/constrained) — self-censor to avoid vague definitions, restructure advocacy, face reputational exposure. Benefit modestly from restored order.
 *   - International Democracy Coalition: Constrained opponent (powerful/constrained) — cannot directly challenge state sovereignty, constrained to advocacy and solidarity mechanisms. Both victims and beneficiaries — their information networks are constrained but visibility maintained.
 *   - International Sovereigntist Coalition (Russia, China allies): Institutional beneficiary (institutional/arbitrage) — sovereign defense doctrine validated, precedent for security law without external review.
 *   - Analytical Observer: Neutral observer (analytical/analytical) — risks naturalizing contingent institutional choice as structural necessity of statehood.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.52).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.68).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Constitutional Sovereignty Restoration (2019 Hong Kong Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sovereignty/security_governance").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '3cebcb6f-f910-4be0-bcfc-80960f4c2981').
narrative_ontology:cs_kernel_codification('3cebcb6f-f910-4be0-bcfc-80960f4c2981', formalized).
narrative_ontology:cs_authority_grounding('3cebcb6f-f910-4be0-bcfc-80960f4c2981', extraction).
narrative_ontology:cs_interpretation_layer_present('3cebcb6f-f910-4be0-bcfc-80960f4c2981').
narrative_ontology:cs_reading_relation('3cebcb6f-f910-4be0-bcfc-80960f4c2981', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cebcb6f-f910-4be0-bcfc-80960f4c2981', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('3cebcb6f-f910-4be0-bcfc-80960f4c2981', foundational, foreign_threat_primacy).
narrative_ontology:cs_axiom_status(foreign_threat_primacy, holdable).
narrative_ontology:cs_axiom_grounding('3cebcb6f-f910-4be0-bcfc-80960f4c2981', foreign_threat_primacy, empirically_contingent).
narrative_ontology:cs_axiom('3cebcb6f-f910-4be0-bcfc-80960f4c2981', foundational, sovereign_security_necessity).
narrative_ontology:cs_axiom_status(sovereign_security_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3cebcb6f-f910-4be0-bcfc-80960f4c2981', sovereign_security_necessity, deontological).
narrative_ontology:cs_created_at('3cebcb6f-f910-4be0-bcfc-80960f4c2981', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, constitutional_order_stability).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_opposition_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protest_movement_mobilization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTEST ACTIVIST / POLITICAL OPPOSITION (SNARE) — Trapped without exit: subject to retroactive application, vague sedition standards, and prosecution risk that did not exist pre-2019. Maximum suppression. Cannot organize mass protest, emigrate without abandonment costs, or exercise prior political freedoms. The law's vague definitions (sedition, subversion, foreign collusion) create chilling effect. High experienced extraction — political participation capacity eliminated.
constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CIVIL SOCIETY ORGANIZATION / CONSTRAINED ADVOCACY (TANGLED ROPE) — Constrained by vague standards and enforcement uncertainty, but also benefits from the stability and rule-of-law restoration claimed by the reading. Some coordination function exists (clear legal boundaries for permitted speech, end to street conflict), but extraction is asymmetric: CSOs self-censor to avoid prosecution risk. Moderate exit cost (restructure activities, avoid certain topics, reputational exposure). Mixed experience of constraint and coordination.
constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL GOVERNMENT AUTHORITY / CPG-HKSAR (ROPE) — Beneficiary with arbitrage options. Experiences the constraint as pure coordination: restoring capacity to govern, reestablishing constitutional order, ending civil unrest. Can exit if domestic order is restored (sunset via success). The authority frames the constraint as solving a coordination problem (anarchic protest behavior, foreign interference) rather than extracting from citizens. Net beneficiary — the constraint aligns with authority's institutional interests.
constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL ORDER / SOVEREIGNTIST COALITION (ROPE) — From the perspective of state sovereignty doctrine, the constraint represents coordination among states defending the principle that domestic security is a sovereign matter not subject to external review. International allies of this reading (Russia, China coalition states) see this as legitimate restoration of sovereign control. Low extraction, high coordination function for this coalition. Arbitrage available (exit if security threat remediates).
constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL DEMOCRACY COALITION / CONSTRAINED OPPOSITION (TANGLED ROPE) — Constrained by state sovereignty doctrine (cannot directly intervene) but benefits from information networks and solidarity mechanisms that sustain opposition visibility. Coordination function (international human rights norms, information flow) exists but is asymmetric: powerful democracy advocates have less direct enforcement capacity than the security apparatus. Moderate extraction and coordination simultaneously — the constraint forces them into constrained advocacy rather than direct support.
constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely analytical standpoint on state theory, some capacity to suppress internal threats is a structural necessity of any functioning state. A sovereign authority that cannot prevent armed rebellion or foreign-backed insurgency cannot maintain order. This perspective sees the constraint as an immutable property of statecraft itself — all states require security apparatus with extraction capacity. However, this reading naturalizes what is actually a reading-dependent institutional choice about WHICH threats and WHICH response mechanisms. The false summit flag applies: this framing conceals the actual contingent design choices embedded in the security law.
constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsl_legal_text__sovereignty_restoration_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading targets political opposition (activists, protest networks) specifically, extracting their capacity to mobilize mass opposition. However, the extraction is not applied to the general population — merchants, workers, professionals outside the opposition continue normal activities under the law. The moderate level reflects that the constraint's target is narrow (opposition) rather than universal. The reading claims legitimate security rationale (foreign interference prevention, order restoration), not pure extraction for its own sake. Base extractiveness would be higher (~0.65+) if applied universally; it is moderate because this reading limits it to opposition. Suppression (0.68): High. Multiple suppression mechanisms: retroactive application prevents legal defense; vague definitions create prosecution uncertainty; enforcement by security apparatus (less transparent than courts) increases unpredictability; emigration carries abandonment costs. The suppression is not total (some opposition continues underground, some flee), but structural barriers are substantial. Theater ratio (0.58): Moderate-high. The law's implementation includes both genuine rule-of-law elements (written statute, judicial proceedings, legal representation) and performative elements (press conferences announcing prosecutions, public security briefings, patriotic education campaigns). The reading's legitimacy claim depends on rule-of-law framing, so theater is actively managed — excessive theater would undermine the 'constitutional restoration' narrative. The trajectory from 0.42 to 0.58 reflects increasing emphasis on public security messaging as initial prosecutions stabilize.
 *
 * PERSPECTIVAL GAP:
 *   The reading creates maximum perspectival divergence between beneficiary and victim perspectives. The central authority (rope) sees coordination and legitimate restoration of governing capacity. The activist (snare) sees pure extraction and political capacity elimination. The CSO (tangled rope) sees both — the constraint disrupts their activities but also provides stability from months of street violence. The international coalition has split perspectives: sovereigntist states see rope (coordination around sovereignty doctrine), democracy advocates see tangled rope (constrained advocacy alongside information networks). The analytical observer sees the false summit risk: the 'necessary state authority' framing naturalizes what is actually a choice about WHICH threats to target (foreign vs. domestic opposition) and HOW MUCH vagueness is acceptable. The reading's internal coherence depends on the threat interpretation — if the security apparatus targets political opposition regardless of foreign involvement evidence, the reading's legitimacy framing collapses and the constraint shifts toward snare/democratic_enclosure reading. If the apparatus focuses on documented foreign interference, the sovereignty_restoration reading stands.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness (0.52) scaled by directionality f(d) and scope σ(S). The powerless activist (powerless/trapped/regional) experiences maximum χ — they derive d ≈ 0.95 from victim status and trapped exit, producing f(d) ≈ 1.42, amplified by regional scope (σ=0.9). The institutional beneficiary (institutional/arbitrage/regional) experiences negative χ — they derive d ≈ 0.00 from beneficiary status and arbitrage exit, producing f(d) ≈ -0.12, dampened by regional scope. The moderate CSO (moderate/constrained/regional) experiences moderate χ — they derive d ≈ 0.65 from victim status and constrained exit, producing f(d) ≈ 1.00, scaled by regional scope (σ=0.9). The international democracy coalition (powerful/constrained/global) experiences moderate χ — they derive d ≈ 0.55 from victim status (their capacity for direct action is constrained) but powerful status reduces experienced extraction; their global scope (σ=1.2) amplifies χ relative to regional actors. The analytical observer (analytical/analytical/global) experiences baseline χ — canonical d ≈ 0.73, f(d) ≈ 1.15, global scope (σ=1.2), yielding χ ≈ 1.01 — which translates to analytical skepticism about the reading's natural-law framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is resolved by explicit kernel-reading decomposition. The NSL statute admits multiple structurally coherent readings — sovereignty_restoration (this story), democratic_enclosure, and jurisdictional_capture — because the statute contains vague definitions (sedition, subversion, foreign collusion) and broad enforcement discretion. Rather than ask 'which type is the NSL?' the framework asks 'which reading of the contested kernel is instantiated?' This story answers: sovereignty_restoration. The mandatrophy is not a defect but a diagnostic signal that the kernel is under-determined by its statutory language. The reading's extractiveness (0.52) and claim type (tangled rope) hold GIVEN the reading's interpretation premises. An alternative reading (democratic_enclosure) would produce higher extractiveness (~0.68+, Snare) and would classify the same statute differently because it would interpret 'sedition' broadly and 'foreign collusion' narrowly. The mandate-resolving move is to recognize that BOTH readings are coherent with the statute's language — the contest is over which interpretation the authority should adopt, not which type the statute 'really is.' The reading relations (coexists_with) reflect this: all three readings remain live options for different political actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_legitimacy_threshold,
    'What level and type of protest-driven disruption constitutes genuine security threat vs. normal democratic contestation that should NOT trigger emergency security response?',
    'Comparative analysis: protest-triggered security law deployment across democracies (France 2005, Chile 2019, Thailand) vs. non-deployment (Germany, Canada) — what differentiates threat from dissent?',
    'If threshold is low (above-normal street disruption): reading classifies as extraction mechanism targeting political opposition (Snare/Tangled Rope dominates). If threshold is high (armed insurrection only): reading classifies as legitimate security coordination (Rope dominates). Threshold location determines whether the reading''s legitimacy claim is coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_legitimacy_threshold, conceptual, 'Threshold distinguishing security threat from normal democratic contestation').

omega_variable(
    retroactivity_legitimacy_gap,
    'Can a security law applying retroactively to conduct that was legal when performed maintain the rule-of-law claim that legitimates this reading?',
    'Legal philosophy analysis: does retroactive application violate Lon Fuller''s eight principles of legality? Do any democracies deploy retroactive security law in comparable circumstances?',
    'If retroactivity is incoherent with rule-of-law: reading''s core legitimacy claim (constitutional order restoration) is self-contradicting. If retroactivity is permissible under sovereignty doctrine: the reading stands but its ''rule of law'' framing is narrower than claimed. This is the crux omega — it determines whether the reading''s axiom set is internally consistent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retroactivity_legitimacy_gap, conceptual, 'Whether retroactive security law is compatible with rule-of-law legitimacy').

omega_variable(
    definition_vagueness_enforcement_space,
    'Do the vague definitions in the security law (sedition, subversion, foreign collusion) create structural space for selective prosecution of political opponents, or do they reflect inevitable ambiguity in security doctrine?',
    'Empirical: track prosecution patterns post-law — are charges concentrated on pro-democracy activists, or distributed across actual security threats? Compare definitional vagueness to comparable security laws in democracies (UK TERRORISM act, US PATRIOT Act). Assess whether alternative statutory formulations were considered.',
    'If selective prosecution is documented: vagueness is intentional design choice enabling extraction (Snare logic dominates). If prosecutions track actual threats: vagueness is inherent to security law and the reading''s legitimacy stands. If alternative statutory formulations existed but were rejected: indicates choice of vagueness enables this reading''s extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_vagueness_enforcement_space, empirical, 'Whether NSL vagueness enables selective prosecution or reflects necessary security ambiguity').

omega_variable(
    sibling_reading_determination_mechanism,
    'What structural evidence would distinguish which sibling reading of the NSL kernel is correct: sovereignty_restoration (this reading), democratic_enclosure, or jurisdictional_capture?',
    'Temporal signature analysis: sovereignty_restoration predicts security apparatus focuses on foreign-linked threats (foreign interference patterns). Democratic_enclosure predicts apparatus targets domestic political opposition regardless of foreign links. Jurisdictional_capture predicts apparatus transfers enforcement authority away from elected bodies toward executive/security services. Track prosecution targets, appeals outcomes, and authority delegation patterns post-law.',
    'If foreign interference documented + prosecution follows: sovereignty_restoration reading is empirically validated. If domestic opposition targeted regardless of foreign evidence: democratic_enclosure reading is validated. If executive/security authority expanded relative to legislature: jurisdictional_capture is validated. Multiple patterns may coexist — the reading may be partially correct alongside sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_determination_mechanism, empirical, 'Mechanism for determining which NSL reading is empirically dominant').

omega_variable(
    exit_pathway_decomposition_ambiguity,
    'Does the NSL contain any credible exit pathway (sunset clause, threat-dependent duration, success-condition termination) or is it permanent?',
    'Statutory analysis: does the law include sunset provisions, threat-review mechanisms, or termination conditions? Compare to temporary emergency security laws (France ÉTAT D''URGENCE, Chile COVID emergency). If permanent, does the reading acknowledge the constraint''s permanence or assume temporary restoration logic without basis?',
    'If no exit pathway: the constraint is not temporary restoration but permanent security architecture, shifting the reading from Scaffold-adjacent to permanent Snare-infrastructure hybrid. This undermines the ''restoration'' framing — the constraint is institutional remaking, not restoration. If exit pathway exists but is narrow: the reading''s sunset logic is conditional on contested threat definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_pathway_decomposition_ambiguity, empirical, 'Whether NSL contains credible exit pathway or is permanent security architecture').

omega_variable(
    kernel_reading_contest_itself,
    'Is the contest over the NSL''s reading inherent to the law''s vague statutory language, or is it a strategic disagreement about WHICH reading the law should instantiate?',
    'Textual analysis: does the NSL statute contain sufficient specificity that only ONE reading is textually coherent? Or does statutory language admit multiple coherent interpretations? Compare to sibling-reading statutory instantiations (democratic_enclosure and jurisdictional_capture) — would the same text cohere with different interpretive authority?',
    'If the statute is under-determined: the reading contest is genuine kernel ambiguity (the cs_structure interpretation_layer_present flag applies). If the statute is specific but authorities choose to interpret it expansively: the contest is strategic (different parties reading the same text differently for political advantage). This distinction determines whether the reading is a defensible statutory interpretation or a motivated reading of an ambiguous instrument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_itself, conceptual, 'Whether NSL reading contest reflects statutory ambiguity or strategic interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_sov_tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nsl_sov_tr_t2, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2, 0.52).
narrative_ontology:measurement(nsl_sov_tr_t4, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(nsl_sov_be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nsl_sov_be_t2, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(nsl_sov_be_t4, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 4, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(nsl_sov_su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(nsl_sov_su_t2, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2, 0.64).
narrative_ontology:measurement(nsl_sov_su_t4, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, hong_kong_protest_mobilization_capacity).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, international_sanctions_regime_reciprocal).

% DUAL FORMULATION NOTE:
% The NSL statute admits three structurally distinct readings with different extractiveness values: sovereignty_restoration (ε=0.52, Tangled Rope — legitimate security with asymmetric cost distribution), democratic_enclosure (ε=0.68, Snare — targets opposition as security threat), jurisdictional_capture (ε=0.60, Tangled Rope — legitimate security with institutional authority transfer). Each reading is a separate constraint story linked via network.affects_constraints. The shared statute creates a kernel; the readings partition it. Empirical tracking of prosecution patterns and enforcement authority will determine which reading dominates in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__sovereignty_restoration_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
