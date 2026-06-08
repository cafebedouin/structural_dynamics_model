% ============================================================================
% CONSTRAINT STORY: shafi_i_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shafi_i_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: shafi_i_reading
 *   human_readable: Shafi'i Jurisprudential Hierarchy: Formalized Source Methodology
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Shafi'i jurisprudential method represents one instantiation of a
 *   contested kernel: how should Islamic law derive rulings from foundational
 *   sources? The Shafi'i reading formalizes source hierarchy (Quran >
 *   authentic Sunnah > ijma > qiyas) and articulates explicit methodology
 *   (usul al-fiqh) for each derivation stage. This constraint exhibits the
 *   full range of DR classifications depending on observer position: lay
 *   believers without training experience it as identity-locked snare;
 *   practicing jurists experience coordination benefits but under career
 *   constraints (tangled rope); institutional jurists benefit from
 *   systematization (rope); alternative methodological traditions experience
 *   competitive constraint (tangled rope); training institutions see
 *   pedagogical function with eventual obsolescence pressure (scaffold);
 *   institutional maintenance apparatus experiences the formalized system as
 *   degraded theater (piton); and civilizational analysis risks naturalizing
 *   a contingent institutional arrangement as immutable legal law (false
 *   mountain). The constraint's extractiveness (0.28) reflects moderate
 *   career asymmetry and access barriers; suppression (0.42) reflects
 *   training gatekeeping; theater ratio (0.35) reflects that significant
 *   scholarly activity is performative chain-of-authority maintenance rather
 *   than genuine jurisprudential reasoning.
 *
 * KEY AGENTS:
 *   - Lay Believers: Primary victims (powerless/identity_locked) — mandated to follow derived law without capacity to verify derivations; identity fused with faith tradition prevents exit
 *   - Practicing Jurists: Secondary agents (moderate/constrained) — benefit from systematic method but career constrained by tradition-bound progression and apprenticeship requirements
 *   - Institutional Jurists (Muftis/Qadis): Primary beneficiaries (institutional/arbitrage) — establish authority legitimacy and institutional position through formalized methodology; leverage arbitrage options
 *   - Alternative Methodological Schools: Secondary victims (organized/constrained) — constrained by institutional preference for formalized Shafi'i approach; competitive pressure reduces their institutional legitimacy
 *   - Jurisprudential Training Institutions: Secondary agents (institutional/mobile) — maintain pedagogical function but face obsolescence pressure from digital alternatives and distributed learning
 *   - Institutional Maintenance Apparatus: Institutional actor (institutional/constrained) — sustains formalized system through waqf endowments, state patronage, scholarly lineage claims; increasingly theatrical
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable legal requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shafi_i_reading, 0.28).
domain_priors:suppression_score(shafi_i_reading, 0.42).
domain_priors:theater_ratio(shafi_i_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shafi_i_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shafi_i_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shafi_i_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shafi_i_reading, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(shafi_i_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shafi_i_reading, tangled_rope).
narrative_ontology:human_readable(shafi_i_reading, "Shafi'i Jurisprudential Hierarchy: Formalized Source Methodology").
narrative_ontology:topic_domain(shafi_i_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(shafi_i_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shafi_i_reading, '411e0bf7-ba09-4971-993a-a6e4a29937c8').
narrative_ontology:cs_kernel_codification('411e0bf7-ba09-4971-993a-a6e4a29937c8', formalized).
narrative_ontology:cs_authority_grounding('411e0bf7-ba09-4971-993a-a6e4a29937c8', lineage).
narrative_ontology:cs_interpretation_layer_present('411e0bf7-ba09-4971-993a-a6e4a29937c8').
narrative_ontology:cs_reading_relation('411e0bf7-ba09-4971-993a-a6e4a29937c8', shafi_i_reading__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('411e0bf7-ba09-4971-993a-a6e4a29937c8', shafi_i_reading__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('411e0bf7-ba09-4971-993a-a6e4a29937c8', shafi_i_reading__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('411e0bf7-ba09-4971-993a-a6e4a29937c8', foundational, formal_hierarchy_prevents_arbitrariness).
narrative_ontology:cs_axiom_status(formal_hierarchy_prevents_arbitrariness, holdable).
narrative_ontology:cs_axiom_grounding('411e0bf7-ba09-4971-993a-a6e4a29937c8', formal_hierarchy_prevents_arbitrariness, instrumental).
narrative_ontology:cs_axiom('411e0bf7-ba09-4971-993a-a6e4a29937c8', foundational, systematic_methodology_legitimizes_authority).
narrative_ontology:cs_axiom_status(systematic_methodology_legitimizes_authority, holdable).
narrative_ontology:cs_axiom_grounding('411e0bf7-ba09-4971-993a-a6e4a29937c8', systematic_methodology_legitimizes_authority, conventional).
narrative_ontology:cs_reference_frame('411e0bf7-ba09-4971-993a-a6e4a29937c8', systematic_source_hierarchy_discipline).
narrative_ontology:cs_drift_state('411e0bf7-ba09-4971-993a-a6e4a29937c8', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('411e0bf7-ba09-4971-993a-a6e4a29937c8', '2026-02-26T18:00:00Z').
narrative_ontology:cs_kernel_id(shafi_i_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shafi_i_reading, trained_jurists).
narrative_ontology:constraint_beneficiary(shafi_i_reading, institutional_authority_structures).
narrative_ontology:constraint_victim(shafi_i_reading, lay_believers_without_training).
narrative_ontology:constraint_victim(shafi_i_reading, alternative_methodological_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY BELIEVER (SNARE) — Cannot independently access the constraint's methodology; religious obligation requires compliance with derived law, but interpretation capacity is locked behind training barrier. Identity fused with faith tradition prevents exit. Maximum extraction: mandatory following without capacity to verify or challenge derivations.
constraint_indexing:constraint_classification(shafi_i_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: PRACTICING JURIST WITH CONSTRAINED AUTHORITY (TANGLED ROPE) — Receives genuine coordination benefit from systematic methodology: clear hierarchy enables consistent jurisprudential reasoning and inter-judge consistency. But also bears extraction: advancement requires mastery of the formalized system, which concentrates authority. Career path is constrained by tradition-bound progression. Mixed experience.
constraint_indexing:constraint_classification(shafi_i_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED INSTITUTIONAL JURIST (ROPE) — Net beneficiary. Systematic methodology legitimizes institutional authority and provides repeatable decision framework. Can leverage arbitrage: travel, trade in legal opinions, institutional advancement. Experiences constraint as coordination — formalized methodology enables their institutional function.
constraint_indexing:constraint_classification(shafi_i_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE METHODOLOGICAL TRADITIONS (TANGLED ROPE) — Experience formalized Shafi'i hierarchy as competitive constraint that reduces their institutional legitimacy while simultaneously creating pressure to formalize their own methodologies to maintain parity. Constrained by resource distribution and institutional preference for one formalized system over multiple open traditions. Organized but under extraction.
constraint_indexing:constraint_classification(shafi_i_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: JURISPRUDENTIAL TRAINING INSTITUTION (SCAFFOLD) — Sees formalized methodology as temporary institutional requirement serving a pedagogical function: training the next generation of jurists requires systematic transmission. Mobile exit options exist (alternative training modalities, digital transmission, distributed learning). Theater ratio is moderate — training ritual is partly pedagogical necessity, partly performative gatekeeping.
constraint_indexing:constraint_classification(shafi_i_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL MAINTENANCE APPARATUS (PITON) — The constraint persists through institutional inertia: waqf endowments, madrasah succession, state patronage systems, scholarly lineage claims. The original function was to prevent arbitrary jurisprudence; the contemporary function is largely to maintain institutional prestige and credential barriers. Theater ratio is high — much scholarly activity is dedicated to maintaining the appearance of systematic rigor in contexts where real discretion exists.
constraint_indexing:constraint_classification(shafi_i_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED AUTHORITY VIEW (MOUNTAIN) — From a civilizational perspective, some hierarchical authority structure may appear immutable to legal systems: all legal traditions require some method to resolve disputes, and formal methodology emerges as natural solution. However, the structural data reveals this as false summit: the specific Shafi'i hierarchy is contingent institutional choice, not inherent requirement.
constraint_indexing:constraint_classification(shafi_i_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shafi_i_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shafi_i_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shafi_i_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(shafi_i_reading, TR),
    TR >= 0.70.

:- end_tests(shafi_i_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The Shafi'i hierarchy does create career asymmetry and access barriers, but the extraction is not severe because genuine coordination benefits exist: the formalized method reduces arbitrary jurisprudence and enables consistent inter-judge reasoning. The extraction is concentrated in the training barrier and the lay believer's mandatory compliance without verification capacity, not in ongoing rent extraction. Suppression (0.42): Moderate. Access barriers are real but surmountable: madrasah training is difficult but available, and some self-study remains possible. The constraint does not rely on coercive enforcement (no sovereign force prevents lay believers from consulting alternative jurists) but rather on institutional gatekeeping and identity fusion. Theater ratio (0.35): Low-moderate. The Shafi'i methodology does contain genuine systematic reasoning at its core, but significant theater exists in chain-of-authority maintenance, scholarly credential rituals, and the institutional apparatus's performance of methodological rigor. The measurement trajectory shows gradual increase over time as institutional maintenance becomes more theatrical and less functionally necessary (alternative transmission modes reduce reliance on formal institutional ritual).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single formalized methodology appears as coordination mechanism to its beneficiaries but as extraction and gatekeeping to those excluded from authority structures. The institutional jurist sees rope (legitimate authority structure). The lay believer sees snare (mandatory compliance without verification). The alternative school sees tangled rope (competitive constraint alongside coordination pressure). The analytical observer risks seeing mountain (naturalized authority) until structural data reveals it as false summit. The perspectival gap reflects the constraint's embedding in institutional power structures: those with training and authority experience the formalization as enabling; those without experience it as disabling.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position. Lay believers (powerless/identity_locked) = high d (near 1.0): mandatory compliance, no exit, no verification capacity. Practicing jurists (moderate/constrained) = medium d (0.4-0.6): benefits from methodology but career constrained. Institutional jurists (institutional/arbitrage) = low d (near 0.0): net beneficiaries, mobile exit, authority capture. Alternative schools (organized/constrained) = high-medium d (0.5-0.7): constrained by institutional preference, competitive pressure. Training institutions (institutional/mobile) = medium-low d (0.2-0.4): pedagogical benefit, mobile alternatives. The constraint's overall χ (effective extraction) is modulated by scope: regional scope for lay believers (lower verification complexity), national for jurists (professional jurisdiction), global for institutional authority (larger scale amplifies enforcement requirements). Suppression is unscaled: the 0.42 base suppression applies equally across scopes, representing the inherent gatekeeping in the training requirement and the institutional structure's capacity to exclude alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   SHAFI'I READING MANDATROPHY: The original mandate was to prevent arbitrary jurisprudence through systematic source methodology — a genuine coordination function addressing a real problem in early Islamic jurisprudence (8th-9th centuries). The constraint remains partially functional: systematic methodology does reduce arbitrary reasoning and enables inter-judge consistency. However, mandatrophy is partial: (1) the constraint has rigidified beyond its original scope — it now governs not just jurisprudential reasoning but also credential gatekeeping and institutional hierarchy; (2) alternative technologies for verification now exist (digital source databases, distributed legal scholarship, autonomous reasoning systems) that bypass the constraint's gatekeeping function; (3) the institutional apparatus maintaining the constraint has increasingly shifted toward theater — waqf endowments, scholarly lineages, and formal credentials now sustain the system more than genuine jurisprudential necessity. The mandatrophy is resolved through: recognizing the genuine coordination function that persists (systematic methodology is still valuable), acknowledging the extraction that has accumulated (credential gatekeeping, institutional rent-seeking), and understanding that the constraint now serves multiple functions (some coordination, some extraction, some institutional maintenance theater). Classification remains tangled_rope because both functions are real and both streams of benefit/cost are significant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodology_vs_discretion,
    'Does the Shafi''i formalized hierarchy genuinely constrain jurist discretion, or does it merely displace discretion into different stages (source selection, precedent interpretation, qiyas reasoning)?',
    'Comparative analysis of jurisprudential output variation: measure consistency of rulings from different jurists following Shafi''i method vs alternative traditions; analyze where consensus breaks and which stages generate variation',
    'If hierarchy genuinely constrains: method functions as described, coordination benefit is real, extraction is moderate. If discretion is displaced: method is theater, extraction is higher, snare classification applies more broadly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_vs_discretion, empirical, 'Whether formalized hierarchy constrains discretion or displaces it').

omega_variable(
    training_accessibility_actual,
    'What is the actual cost and time barrier to jurisprudential training in the contemporary period? Does formalized Shafi''i methodology genuinely require institutional madrasah training, or can systematic self-study provide equivalent competence?',
    'Empirical comparison of autonomous jurists vs institutionally trained jurists; measurement of institutional barrier height vs actual knowledge barrier; analysis of contemporary digital/distributed learning alternatives',
    'If institutional training is necessary: suppression value (0.42) is accurate and justified. If training is accessible outside institutions: suppression is overstated, identity_locked exit is overstated, lay believer snare classification may not hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(training_accessibility_actual, empirical, 'Accessibility of jurisprudential training outside institutional gatekeepers').

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the Shafi''i hierarchy a natural development that any mature legal tradition must converge on, or a contingent institutional construction that reflects 9th-century Arab social organization and could have developed differently?',
    'Historical comparative analysis: do non-Islamic legal traditions that independently developed require similar hierarchies? Counterfactual analysis: what alternative methodologies were available and plausible to Shafi''i''s intellectual context? Analysis of Shafi''i''s own framing vs later institutionalization.',
    'If natural/convergent: mountain classification more defensible, false summit analysis weakens. If contingent construction: false summit is real, extraction classification holds, boundary between mountain and tangled_rope definitively resolves to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether Shafi''i methodology is natural legal convergence or contingent construction').

omega_variable(
    kernel_reading_stakes,
    'Which kernel reading (Shafi''i, Hanafi, Maliki, Hanbali) correctly interprets the fundamental commitment of Islamic jurisprudence regarding source authority and methodological rigor?',
    'Textual analysis of foundational sources (Quran, early Hadith, companions'' practice); historical reconstruction of jurisprudential disputes; contemporary sectarian and scholarly consensus indicators',
    'Different readings may be identified as more or less normatively justified within the tradition, but classification structure remains independent of this normative judgment. This omega documents irreducible reading contestation, not classification uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stakes, conceptual, 'Which jurisprudential reading correctly instantiates the Islamic legal tradition''s fundamental commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shafi_i_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shafi_tr_t0, shafi_i_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(shafi_tr_t3, shafi_i_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(shafi_tr_t6, shafi_i_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(shafi_be_t0, shafi_i_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(shafi_be_t3, shafi_i_reading, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(shafi_be_t6, shafi_i_reading, base_extractiveness, 6, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(shafi_su_t0, shafi_i_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(shafi_su_t3, shafi_i_reading, suppression_requirement, 3, 0.4).
narrative_ontology:measurement(shafi_su_t6, shafi_i_reading, suppression_requirement, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shafi_i_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(shafi_i_reading, 0.12).
narrative_ontology:affects_constraint(shafi_i_reading, hanafi_reading).
narrative_ontology:affects_constraint(shafi_i_reading, maliki_reading).
narrative_ontology:affects_constraint(shafi_i_reading, hanbali_reading).
narrative_ontology:affects_constraint(shafi_i_reading, madrasah_gatekeeping_system).
narrative_ontology:affects_constraint(shafi_i_reading, qadi_appointment_hierarchy).

% DUAL FORMULATION NOTE:
% The Shafi'i jurisprudential hierarchy is one reading of the contested kernel of Islamic legal methodology. Sibling readings (Hanafi, Maliki, Hanbali approaches) are structurally distinct constraints with different ε values and institutional arrangements. This story documents the Shafi'i reading specifically; alternative readings are documented in separate constraint stories. The network relationships indicate institutional interference: Shafi'i formalization influenced the other schools to formalize defensively, creating resource competition and methodology-based credential hierarchies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shafi_i_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
