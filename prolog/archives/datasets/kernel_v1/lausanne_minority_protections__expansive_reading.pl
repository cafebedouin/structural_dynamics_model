% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections (Expansive Reading) — Treaty Guarantee of Religious Institutional Continuity
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Lausanne Treaty (1923) and its successor protocols guarantee minority
 *   religious institutions—primarily Greek Orthodox, Armenian Apostolic, and
 *   Jewish communities—functional continuity of pre-1923 governance
 *   structures within successor states of the Ottoman Empire. This reading
 *   (expansive_reading) interprets the guarantee as substantive institutional
 *   autonomy: minority institutions retain self-administration of internal
 *   affairs, property management, and clergy formation via theological
 *   schools without state interference. The constraint exhibits low
 *   extractiveness (0.32) and moderate suppression (0.42) characteristic of
 *   pure coordination mechanisms—the treaty solves a genuine collective
 *   action problem (how to protect minority religious continuity across state
 *   formation) with low coercive overhead. However, this reading is contested
 *   by two alternatives: a restrictive_reading that emphasizes state police
 *   power over minority institutions and a guarantor_reading that emphasizes
 *   guarantor state sovereignty. The expansive_reading assumes goodwill
 *   guarantor enforcement and institutional autonomy; competing readings
 *   assume strategic guarantor behavior and state subordination of minority
 *   governance. This story generates the expansive_reading constraint; the
 *   alternatives are separate stories in the same constraint family.
 *
 * KEY AGENTS:
 *   - Minority Religious Institutions (Orthodox, Armenian, Jewish): Primary beneficiary (moderate/constrained) — depend on treaty guarantees for institutional continuity and self-administration; lack independent state power but have legal standing
 *   - Guarantor States (Greece, Serbia, Romania, Bulgaria): Secondary beneficiary (institutional/arbitrage) — fulfill treaty obligations to manage minority populations and enhance diplomatic legitimacy; have enforcement discretion
 *   - Host States (non-guarantor nations where minorities reside): Institutional coordination (institutional/mobile) — operate under treaty constraints but choose compliance as more efficient than unilateral minority policy
 *   - Individual Minority Members: Secondary victim if extraction occurs (powerless/trapped) — nominally protected by institutional autonomy but dependent on institutions not extracting their theological autonomy
 *   - Rival Theological Factions: Organized secondary victim (organized/constrained) — coordinate with institutions but may face extraction if institutional interpretation freezes one faction as official
 *   - International Legal Order: Analytical observer (analytical/analytical) — sees constraint as temporary scaffold to more durable human-rights regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.32).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.42).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections (Expansive Reading) — Treaty Guarantee of Religious Institutional Continuity").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, 'bab149e6-0366-4cf1-93f7-21fc3e795cae').
narrative_ontology:cs_kernel_codification('bab149e6-0366-4cf1-93f7-21fc3e795cae', formalized).
narrative_ontology:cs_authority_grounding('bab149e6-0366-4cf1-93f7-21fc3e795cae', lineage).
narrative_ontology:cs_interpretation_layer_present('bab149e6-0366-4cf1-93f7-21fc3e795cae').
narrative_ontology:cs_reading_relation('bab149e6-0366-4cf1-93f7-21fc3e795cae', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('bab149e6-0366-4cf1-93f7-21fc3e795cae', lausanne_minority_protections__guarantor_reading, coexists_with).
narrative_ontology:cs_axiom('bab149e6-0366-4cf1-93f7-21fc3e795cae', foundational, institutional_autonomy_substantive).
narrative_ontology:cs_axiom_status(institutional_autonomy_substantive, holdable).
narrative_ontology:cs_axiom_grounding('bab149e6-0366-4cf1-93f7-21fc3e795cae', institutional_autonomy_substantive, deontological).
narrative_ontology:cs_axiom('bab149e6-0366-4cf1-93f7-21fc3e795cae', foundational, guarantor_enforcement_good_faith).
narrative_ontology:cs_axiom_status(guarantor_enforcement_good_faith, holdable).
narrative_ontology:cs_axiom_grounding('bab149e6-0366-4cf1-93f7-21fc3e795cae', guarantor_enforcement_good_faith, conventional).
narrative_ontology:cs_reference_frame('bab149e6-0366-4cf1-93f7-21fc3e795cae', treaty_protected_institutional_autonomy).
narrative_ontology:cs_drift_state('bab149e6-0366-4cf1-93f7-21fc3e795cae', contemporary_securitization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bab149e6-0366-4cf1-93f7-21fc3e795cae', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_religious_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, treaty_guarantor_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY RELIGIOUS INSTITUTION (ROPE) — Depends on treaty guarantees for institutional self-administration, property rights, and clergy formation. No extraction mechanism; genuine coordination function: treaty ensures continuity across state boundaries and generational transitions. Constrained by dependence on guarantor compliance but benefits from coordination that enables transnational religious governance.
constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: GUARANTOR STATE (ROPE) — Experiences the constraint as coordination function: treaty obligation to protect minority institutions serves both minority interests and guarantor diplomatic legitimacy. No asymmetric extraction. The guarantor has arbitrage options (enforcing or ignoring treaty terms) but the treaty provides legitimate framework for state action. Pure coordination mechanism with low overhead.
constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HOST STATE (NON-GUARANTOR) (ROPE) — State wherein minority institutions operate under treaty obligation. Experiences constraint as coordination: treaty provides liability framework and legitimacy for minority rights protection. Has mobility options (can ratify or withdraw treaty compliance) but chooses coordination framework as more efficient than unilateral minority policy.
constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: INDIVIDUAL MINORITY MEMBER (TANGLED ROPE) — Individual adherents have genuine coordination benefit (access to clergy, institutional continuity, cultural transmission) but face asymmetric extraction if the institutional reading diverges from individual prerogatives. Trapped by dependence on institutional gatekeeping. The treaty nominally protects the institution; whether it protects individual dissent or theological diversity is the extraction vector.
constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: RIVAL RELIGIOUS FACTION (TANGLED ROPE) — Organized competitors within the same confessional tradition who may interpret 'institutional self-administration' differently. Coordination benefit: treaty protects religious pluralism and prevents state-favoring of one faction. Extraction cost: the treaty's guarantees of 'the institution' may freeze one faction's interpretation as official, excluding rival readings. Organized power enables exit via schism or appeal to guarantor, but constrained by costs.
constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL OBSERVER (SCAFFOLD) — Treaty architecture is a temporary fix to a generational conflict: the Lausanne protections were designed to manage minority rights during the post-Ottoman state formation period. From a civilizational view, the constraint is sunset: as nation-states mature and pluralism norms strengthen, treaty-based minority protection becomes scaffolding to a more durable international human-rights regime. The extraction mechanism decays as alternatives mature.
constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lausanne_minority_protections__expansive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low-moderate. The expansive reading assumes no systematic extraction—the treaty is pure coordination that enables minority institutional continuity. The 0.32 value reflects residual uncertainty about whether institutional gatekeeping (who controls seminary curriculum, who selects clergy) constitutes latent extraction. Under strict expansive reading, these are institutional prerogatives with no extraction cost. Under more skeptical analysis, institutional hierarchies do extract theological conformity. The 0.32 reflects the optimistic reading; omega variables document where extraction might emerge. Suppression (0.42): Moderate. State-level compliance with treaty guarantees is the suppression mechanism—if states choose not to enforce minority property rights or allow institutional autonomy to erode, minority institutions face substantial barriers. But suppression is not total because the treaty provides legal standing and international recourse. Theater ratio (0.55): Moderate-high, rising slightly over time. Early Lausanne era (1923) showed more genuine institutional autonomy; contemporary era shows more ceremonial compliance and state encroachment. The rise from 0.40 to 0.55 reflects growing disconnect between treaty language (institutional self-administration) and practice (increasing state oversight via secular accreditation and property law).
 *
 * PERSPECTIVAL GAP:
 *   The expansive reading produces homogeneous rope classification across beneficiary perspectives (institutions and guarantor states) because both parties perceive the constraint as genuine coordination with low overhead. However, perspectives from powerless agents (individual members, rival factions) diverge toward tangled_rope and scaffold, revealing that the 'pure coordination' reading depends on institutional good faith. The individual member perspective shows that institutional gatekeeping creates latent extraction—the individual benefits from institutional continuity but bears extraction cost if theological dissent is suppressed. The rival faction perspective shows that 'institutional self-administration' may freeze one faction's reading as official, excluding alternatives. The scaffold perspective reveals that the entire constraint is generational sunset—as national human-rights regimes mature and pluralism norms strengthen, the treaty's coordinating function decays and the constraint moves toward piton (ceremonial maintenance of international legal architecture).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural power, exit options, and relationship to the treaty guarantee. Minority institutions have moderate power (legal standing but no enforcement capacity) and constrained exit (cannot abandon institutional continuity without losing cultural transmission), producing d ≈ 0.55–0.65 and rope classification via low effective extraction. Guarantor states have institutional power and arbitrage options (can enforce or ignore treaty), producing d ≈ 0.15–0.25 and rope classification via beneficiary directionality. Individual members are trapped with no exit, but the guarantor mechanism provides structural mobility (appeal to guarantor state), producing d ≈ 0.70–0.80 and tangled_rope rather than snare. The analytical observer at civilizational scope sees the constraint as sunset mechanism, producing scaffold classification—this is not a failure of rope classification but a temporal shift in what the constraint does (coordination → scaffolding).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy (disambiguation of coordination vs extraction in treaty-based minority protection) by declaring the beneficiary structure transparent: minority institutions and guarantor states benefit from the coordination mechanism; individual members and rival factions experience potential extraction. The rope classification is defensible IF (a) guarantor states enforce the treaty with genuine commitment to minority autonomy, (b) minority institutions do not use institutional gatekeeping to extract theological conformity, and (c) the treaty's scope is interpreted expansively to include individual conscience protections within institutional frameworks. Each of these assumptions is contestable—hence the competing readings (restrictive_reading, guarantor_reading) and the omegas documenting where extraction might emerge. The mandatrophy is not 'is this rope or snare?' but 'which assumptions about guarantor behavior and institutional good faith underpin this reading?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_versus_individual_scope,
    'Does ''religious institutional self-administration'' protect only institutional prerogatives or also individual theological dissent and minority doctrine within the tradition?',
    'Case law analysis of disputes between minority institutions and member factions claiming theological innovation; comparison of expansive (individual conscience protected) vs restrictive (institutional orthodoxy protected) interpretations in guarantor jurisprudence',
    'Expansive reading: constraint is pure rope (coordination between institution and state). Restrictive reading: constraint becomes tangled_rope with extraction of individual theological autonomy by institutional gatekeeping. This omega locates the critical interpretive boundary within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_versus_individual_scope, conceptual, 'Scope of protection: institutional governance vs individual theological freedom').

omega_variable(
    guarantor_enforcement_mechanism,
    'What enforcement mechanism ensures guarantor states actually protect minority institutions? Treaty language guarantees but external enforcement is weak.',
    'Historical analysis of cases where guarantor states violated minority protections without penalty; identification of effective enforcement pathways (International Court of Justice, diplomatic pressure, sanctions) vs theatrical compliance',
    'If enforcement effective: rope classification holds. If enforcement weak: theater_ratio rises sharply, constraint degrades to piton (ceremonial protection with inertial institutional maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_enforcement_mechanism, empirical, 'Effectiveness of treaty enforcement mechanisms').

omega_variable(
    reading_interpretive_contest,
    'This reading (expansive_reading) asserts treaty guarantees functional continuity of pre-1923 governance. Will guarantor states and minority institutions converge on this interpretation, or will competing readings (restrictive_reading, guarantor_reading) win interpretive authority?',
    'Future jurisprudence by International Court of Justice, guarantor state courts, and minority institution leadership councils; analysis of which reading is cited most frequently and applied most consistently in real disputes',
    'If expansive reading holds: constraint remains rope with genuine coordination function. If restrictive reading wins: extraction mechanisms tighten (institutional orthodoxy enforced, individual dissent suppressed). If guarantor reading wins: constraint becomes extractive of minority institutional autonomy in service to state interests. This is a committer-axis omega documenting the reading contest itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_interpretive_contest, conceptual, 'Which reading of the Lausanne kernel will dominate future interpretation').

omega_variable(
    theological_school_formation_autonomy,
    'Can minority institutions freely determine theological curriculum and ordination standards (''clergy formation via theological schools''), or must guarantor state approval or secular accreditation constraints apply?',
    'Documentary analysis of theological school charters, guarantor state educational regulations, and dispute resolution precedents involving theology curriculum control; examination of whether ''self-administration'' includes curriculum autonomy',
    'If autonomous: rope classification holds (genuine institutional self-governance). If constrained by state accreditation: tangled_rope (coordination benefit of institutional continuity paired with extraction of theological autonomy to secular standards).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_school_formation_autonomy, empirical, 'Whether clergy formation is self-administered or subject to state oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_exp_theater_1923, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(laus_exp_theater_1973, lausanne_minority_protections__expansive_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(laus_exp_theater_2023, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(laus_exp_extract_1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(laus_exp_extract_1973, lausanne_minority_protections__expansive_reading, base_extractiveness, 50, 0.3).
narrative_ontology:measurement(laus_exp_extract_2023, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel has three structurally distinct constraint readings. This story (expansive_reading) assumes institutional autonomy and low extraction (ε=0.32, rope). The restrictive_reading (separate story) assumes state regulatory authority and moderate extraction (ε≈0.45, tangled_rope). The guarantor_reading (separate story) assumes guarantor state sovereignty over minorities and high extraction (ε≈0.60, snare). All three readings ground their legitimacy in the same treaty text; the ε-invariance principle requires separate stories because the observable (how 'institutional self-administration' and 'guarantor enforcement' are interpreted) determines whether the constraint is coordination or extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
