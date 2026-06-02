% ============================================================================
% CONSTRAINT STORY: lived_experience_epistemology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lived_experience_epistemology, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: lived_experience_epistemology
 *   human_readable: Lived Experience Epistemology in Knowledge Production
 *   domain: epistemology/social_knowledge/identity
 *
 * SUMMARY:
 *   Lived experience epistemology emerged as a legitimate epistemic claim in
 *   the late 20th century: marginalized communities asserted that their
 *   situated knowledge—accumulated through navigating systems designed
 *   without their interests in mind—constitutes a valid knowledge source
 *   inaccessible to those outside the category. This claim resolved a real
 *   epistemic problem: hegemonic knowledge frameworks had systematically
 *   excluded, ignored, or pathologized the experiences and frameworks of
 *   marginalized groups. However, institutional adoption of lived experience
 *   epistemology has produced a structural constraint with contradictory
 *   properties. It simultaneously redistributes epistemic legitimacy to
 *   marginalized voices AND locks those voices into identity categories as
 *   their primary epistemic credential. Institutional gatekeepers
 *   (universities, journals, platforms, foundations) have adopted lived
 *   experience requirements that appear to democratize knowledge production
 *   while maintaining structural control over which voices count, which
 *   categories are legible, and which frameworks are authentic. The
 *   constraint exhibits both genuine coordination (communities building
 *   knowledge together) and significant extraction (identity categories
 *   becoming enforced epistemic credentials, alternative frameworks
 *   suppressed, performative theater replacing actual authority
 *   redistribution).
 *
 * KEY AGENTS:
 *   - Marginalized Identity Communities: Primary beneficiary AND victim (powerless/trapped) — gain epistemic legitimacy through lived experience standing but become locked into identity categories as epistemic credentials; trapped because exiting the category requires abandoning their only institutional voice
 *   - Solidarity Allies Within Communities: Secondary agent (moderate/identity_locked) — structurally constrained with identity fusion; experience both genuine knowledge coordination within community frameworks and pressure to validate increasingly narrow identity categories
 *   - Knowledge Legitimacy Gatekeepers (Universities, Journals, Platforms): Primary beneficiary (institutional/arbitrage) — maintain control over legitimacy allocation while appearing to democratize through lived experience requirements; net beneficiary from the extraction flow
 *   - Hegemonic Knowledge Communities: Secondary beneficiary (powerful/arbitrage) — experience both coordination benefits (pluralism as appearance, expanded research agendas) and extraction costs (defense of dominance); maintain structural advantages while appearing to share authority
 *   - Alternative Epistemic Frameworks: Primary victim (powerless/analytical) — suppressed as 'lacking authenticity' when they don't align with dominant lived experience frameworks; cannot argue for themselves within the constraint structure
 *   - Institutional Authenticity Theater: Institutional actor performing degraded ritual (institutional/arbitrage) — maintains appearance of inclusivity while gatekeeping remains unchanged; high theater ratio indicates primary function has been replaced by ritual
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent institutional arrangement (identity-category validation as epistemic necessity) as immutable law of knowledge production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lived_experience_epistemology, 0.52).
domain_priors:suppression_score(lived_experience_epistemology, 0.58).
domain_priors:theater_ratio(lived_experience_epistemology, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lived_experience_epistemology, extractiveness, 0.52).
narrative_ontology:constraint_metric(lived_experience_epistemology, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(lived_experience_epistemology, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lived_experience_epistemology, tangled_rope).
narrative_ontology:human_readable(lived_experience_epistemology, "Lived Experience Epistemology in Knowledge Production").
narrative_ontology:topic_domain(lived_experience_epistemology, "epistemology/social_knowledge/identity").

domain_priors:requires_active_enforcement(lived_experience_epistemology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lived_experience_epistemology, marginalized_identity_communities).
narrative_ontology:constraint_beneficiary(lived_experience_epistemology, academic_legitimacy_gatekeepers).
narrative_ontology:constraint_victim(lived_experience_epistemology, knowledge_commons_coherence).
narrative_ontology:constraint_victim(lived_experience_epistemology, alternative_epistemic_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMICALLY MARGINALIZED (SNARE) — Structurally trapped. Marginalized communities must perform lived experience validation to achieve institutional recognition, but this performance (1) reproduces identity categorization as the primary epistemic asset, (2) forecloses other modes of knowing they might employ, (3) makes their credibility contingent on maintaining the identity category itself. Exit requires abandoning the epistemic resource (lived experience standing) that provides their only institutional legitimacy. Maximum suppression: alternative frameworks are delegitimized as 'lacking authenticity.'
constraint_indexing:constraint_classification(lived_experience_epistemology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SOLIDARITY ALLY WITHIN COMMUNITY (TANGLED ROPE) — Structurally constrained with identity fusion. This agent may have institutional power or mobility (constrained-class resources) but their identity is constituted through the lived experience framework — abandoning it would mean severance from community affiliation, professional solidarity networks, and the narrative framework through which they understand their own historical position. Experiences both coordination (genuine mutual aid, shared knowledge-building within community frameworks) and extraction (pressure to validate increasingly narrow identity categories, vulnerability to co-optation by institutional actors).
constraint_indexing:constraint_classification(lived_experience_epistemology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: KNOWLEDGE LEGITIMACY GATEKEEPER (ROPE) — Institutional actor (universities, journals, foundations, platforms) experiences the lived experience requirement as pure coordination: it solves the allocation problem of which voices get institutional megaphone. The constraint distributes legitimacy through identity categories, creating what appears to be a neutral, fair epistemic mechanism. Net beneficiary — the institutional actor can claim moral authority while maintaining structural control over legitimacy allocation. Extraction flows toward this agent, but the agent experiences only the coordination function.
constraint_indexing:constraint_classification(lived_experience_epistemology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HEGEMONIC KNOWLEDGE COMMUNITY (TANGLED ROPE) — Powerful institutional actors (dominant epistemic traditions, majority-authored scholarship, disciplinary orthodoxy) experience the constraint as a coordination mechanism that legitimates alternative frameworks (coordination benefit) while simultaneously requiring them to defend epistemological dominance through debate with newly-legitimated voices (extraction cost). The constraint creates the appearance of pluralism while maintaining structural advantages: hegemonic frameworks still set the terms of debate, establish research agendas, control resources. Mixed coordination (legitimating pluralism as appearance) and extraction (maintaining power while appearing to share it).
constraint_indexing:constraint_classification(lived_experience_epistemology, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL AUTHENTICITY THEATER (PITON) — The lived experience framework has degraded into performative authenticity rituals. Universities hire diversity officers and center lived experience in syllabi while maintaining identical power distributions and funding allocations; journals publish lived experience narratives as gesture toward legitimacy while gatekeeping peer review remains dominated by hegemonic frameworks; platforms amplify authenticity markers while their algorithmic amplification follows engagement metrics that privilege conflict and certainty. The primary function (genuine knowledge-sharing from marginalized frameworks) has been replaced by ritual (token authenticity that immunizes against structural criticism). Theater ratio high; functional coordination nearly absent.
constraint_indexing:constraint_classification(lived_experience_epistemology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational perspective, some reliance on lived experience is epistemically necessary: all knowledge is situated knowledge; all frameworks embed perspectives; the attempt to produce view-from-nowhere knowledge is itself a perspective claim. Therefore, lived experience epistemology represents an immutable feature of how knowledge works — not a constraint structure but a law of epistemic reality. However, the base properties contradict this reading. Theater ratio (0.64) and suppression (0.58) reveal this as naturalizing what is actually a contingent institutional arrangement: the *particular forms* of lived experience epistemology (identity-category validation, authenticity performance, voice hierarchy) are not epistemically necessary, even if *some* attention to situatedness is.
constraint_indexing:constraint_classification(lived_experience_epistemology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lived_experience_epistemology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lived_experience_epistemology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lived_experience_epistemology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lived_experience_epistemology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lived_experience_epistemology, TR),
    TR >= 0.70.

:- end_tests(lived_experience_epistemology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from two directions: (1) It extracts from marginalized communities by requiring identity-category validation as the price of epistemic standing — they must maintain the identity category to maintain the credential. (2) It extracts from the knowledge commons by suppressing alternative frameworks (quantitative analysis, mathematical abstraction, cross-cultural synthesis, systems modeling in marginalized-group contexts) as 'lacking authenticity' — the framework becomes epistemically hegemonic while claiming to represent pluralism. The value of 0.52 reflects that genuine knowledge coordination does happen (communities building together), but this coordination is asymmetric — marginalized communities coordinate while having their frameworks validated or invalidated by institutional gatekeepers. Suppression (0.58): Moderate-high. Multiple suppression mechanisms operate: (1) Material barriers to alternative knowledge production (research funding prioritizes lived experience narratives; quantitative research on marginalized groups faces skepticism as 'extractive'); (2) Cognitive suppression (students in marginalized communities are discouraged from developing frameworks that diverge from community consensus as 'inauthentic'); (3) Institutional suppression (hiring and promotion in relevant fields heavily weighted toward lived experience credentials). Theater ratio (0.64): High. Institutional adoption of lived experience epistemology has become substantially performative. Universities hire diversity officers and center lived experience in syllabi while maintaining identical power distributions and resource allocations; journals publish lived experience narratives alongside unchanged peer review hierarchies; platforms amplify authenticity markers while their algorithmic amplification privileges engagement metrics that reward conflict and certainty. The primary coordination function (genuine knowledge-sharing from marginalized frameworks) has been layered with performance (authenticity rituals that immunize institutions against structural criticism while changing nothing about actual authority distribution). The measurement trajectory shows this degradation over time: extractiveness rose from 0.28 to 0.52 as institutional adoption scaled; theater ratio rose from 0.35 to 0.64 as institutions shifted from genuine framework integration to performative authenticity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The marginalized epistemically-standing agent sees Snare — they are trapped; exit requires abandoning their epistemic credential. The solidarity ally sees Tangled Rope — genuine community knowledge-building (coordination) mixed with pressure to validate narrow identity categories (extraction) and identity fusion that makes exiting impossible without identity dissolution. The knowledge gatekeeper sees Rope — they experience the constraint as solving the allocation problem of which voices get legitimacy, with no awareness of asymmetric extraction. The hegemonic framework community sees Tangled Rope from the opposite direction — they experience both the coordination benefit (pluralism legitimates new research agendas) and extraction cost (having to defend their dominance through debate). The institutional theater sees Piton — they maintain the ritual of authenticity performance while the primary coordination function has degraded. The analytical observer risks seeing Mountain — naturalizing identity-situated knowledge as an immutable epistemic law — but the structural data reveals this as a false summit: some reliance on situated perspectives is epistemically necessary, but the *particular forms* (identity-category validation, authenticity performance, voice hierarchy) are contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the agent's structural position: their power level, exit options, and relationship to the extraction flow. Marginalized communities classified as powerless/trapped derive d ≈ 0.95 → high f(d) → high experienced extraction. Solidarity allies with identity_locked exit derive d ≈ 0.89 from their victim status plus identity fusion, producing high f(d) but lower than trapped because identity_locked agents retain some theoretical mobility (they *could* shift identity frames, even if doing so would be psychologically/socially catastrophic). Institutional gatekeepers classified as institutional/arbitrage derive d ≈ 0.05 → negative f(d) → negative experienced extraction (they benefit; the constraint subsidizes their position). The powerful hegemonic community derives d ≈ 0.48 from their mixed position — they bear some coordination costs (defending dominance) but maintain structural advantages, producing near-symmetric experienced extraction. The analytical observer with analytical/analytical context derives d ≈ 0.72, producing analytical-level extraction that reveals the constraint's false mountain claim.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION THROUGH DECOMPOSITION: Lived experience epistemology is not one constraint but at least two, and possibly three, structurally distinct claims with different ε values: (1) 'Some attention to situated knowledge is epistemically necessary' (ε ≈ 0.08, Mountain) — this is true and important. (2) 'Identity-category validation should be the primary epistemic credential for knowledge about marginalized groups' (ε ≈ 0.52, Tangled Rope) — this is the institutional constraint documented here. (3) 'Alternative frameworks (quantitative analysis, mathematical abstraction, systems modeling) are illegitimate when applied to marginalized groups' (ε ≈ 0.68, Snare) — this is the suppression mechanism. The mandatrophy is resolved by recognizing that institutional actors have conflated claim 1 (epistemically sound) with claims 2 and 3 (structurally extractive). Institutions use the legitimacy of claim 1 to justify the extraction mechanisms of claims 2 and 3. The false summit at the analytical perspective arises from exactly this conflation — it naturalizes the institutional arrangement (claims 2+3) as if it were the epistemological necessity (claim 1). Separating the claims reveals the extraction mechanism: institutions use situated knowledge legitimacy as the cover story for maintaining gatekeeper control over which voices and frameworks count.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_essentialism_threshold,
    'At what point does insistence on identity-authentic voice collapse into epistemological essentialism — the claim that only members of a category can know about that category?',
    'Comparative analysis of knowledge claims: can non-marginalized actors produce valid knowledge about marginalized experiences through rigorous inquiry? Longitudinal tracking of who gets cited as experts on marginalized topics in institutional contexts.',
    'If threshold low (strict essentialism enforced): authenticity requirement is extractive mechanism preventing broader knowledge-sharing. If threshold high (essentialism rejected): lived experience becomes one valid epistemic source among others, reducing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_essentialism_threshold, conceptual, 'Boundary between legitimate voice hierarchy and epistemological essentialism').

omega_variable(
    institutional_good_faith_ambiguity,
    'When institutions center lived experience, are they genuinely redistributing epistemic authority or performing the appearance of redistribution while maintaining structural control?',
    'Track actual resource allocation (funding, hiring, curriculum authority): Do institutions allocate research grants and teaching authority proportionally to lived experience voice, or only platform/amplification? Do lived experience frameworks reshape institutional priorities or remain parallel to unchanged orthodoxy?',
    'If genuine redistribution: constraint is Rope with coordination benefits. If performative: constraint is Piton with theater-based degradation. This ambiguity maps directly to whether suppression operates structurally (structural barriers to knowledge production) or through institutional theater (appearance of openness masking unchanged gatekeeping).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_good_faith_ambiguity, empirical, 'Whether institutional centering represents actual epistemic authority redistribution or performance').

omega_variable(
    identity_lock_sustainability,
    'Is identity-locked commitment to lived experience epistemology frameworks sustainable as an epistemic strategy, or does it collapse when identity categories shift, communities split, or individual agents develop perspectives that diverge from community consensus?',
    'Longitudinal tracking of agents who entered marginalized-community knowledge production through lived experience frameworks: Do they maintain framework commitment if they (1) develop critical perspectives on community narratives, (2) move between communities, (3) experience category redefinition or dissolution? Do they retain epistemic standing if they diversify knowledge sources?',
    'If sustainable: identity_locked is stable and the framework is a genuine epistemic infrastructure. If unstable: identity_locked represents brittle cognitive capture vulnerable to collapse under category stress, suggesting the constraint is more extractive (Snare) than coordination-based (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_sustainability, empirical, 'Whether identity-locked commitment to lived experience frameworks sustains under category flux').

omega_variable(
    alternative_framework_suppression_mechanism,
    'Is the observed suppression of alternative epistemic frameworks (quantitative data, mathematical abstraction, cross-cultural synthesis, evolutionary biology, systems modeling in marginalized-group contexts) a functional feature of lived experience epistemology or a contingent institutional sideeffect?',
    'Comparative analysis across institutional contexts: Do sites that center lived experience while actively integrating alternative frameworks show better knowledge production (more predictive power, more practical utility, fewer internal contradictions)? Or is framework pluralism actually undermined by the institutional enforcement of lived experience hierarchy?',
    'If functional suppression: the constraint''s asymmetry is more severe (victims include legitimate alternative knowledge frameworks, not just institutional convenience). If contingent sideeffect: suppression reflects institutional conservatism rather than logical structure of lived experience, and could be decoupled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_suppression_mechanism, empirical, 'Whether suppression of alternatives is functional to lived experience epistemology or contingent institutional behavior').

omega_variable(
    nested_identity_collapse,
    'What happens to lived experience epistemology when agents occupy multiple marginalized identities with conflicting situated perspectives? Does the framework accommodate this complexity or force artificial unity?',
    'Case analysis: marginalized agents with intersecting identities (race + class + gender + disability + sexuality + nationality combinations); track whether lived experience framework supports holding multiple, sometimes contradictory, situated knowledge positions or enforces a single ''authentic'' position per identity marker. Measure coherence of knowledge produced when intersectional agents are expected to validate multiple identity narratives simultaneously.',
    'If framework accommodates complexity: lived experience epistemology can be a genuine pluralism mechanism. If framework collapses under complexity: the constraint is more extractive because it produces an impossible epistemic demand on multiply-marginalized agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nested_identity_collapse, empirical, 'Whether lived experience frameworks accommodate or collapse under intersectional identity complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lived_experience_epistemology, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liveexp_tr_t0, lived_experience_epistemology, theater_ratio, 0, 0.35).
narrative_ontology:measurement(liveexp_tr_t8, lived_experience_epistemology, theater_ratio, 8, 0.5).
narrative_ontology:measurement(liveexp_tr_t16, lived_experience_epistemology, theater_ratio, 16, 0.64).
narrative_ontology:measurement(liveexp_tr_t4, lived_experience_epistemology, theater_ratio, 4, 0.42).

% Extraction over time
narrative_ontology:measurement(liveexp_be_t0, lived_experience_epistemology, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(liveexp_be_t8, lived_experience_epistemology, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(liveexp_be_t16, lived_experience_epistemology, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(liveexp_be_t4, lived_experience_epistemology, base_extractiveness, 4, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lived_experience_epistemology, identity_coordination).
narrative_ontology:affects_constraint(lived_experience_epistemology, epistemic_authority_redistribution).
narrative_ontology:affects_constraint(lived_experience_epistemology, marginalized_knowledge_production_systems).
narrative_ontology:affects_constraint(lived_experience_epistemology, institutional_authenticity_performance).

% DUAL FORMULATION NOTE:
% Lived experience epistemology decomposes into at least three structurally distinct constraints: (1) situated_knowledge_necessity (ε≈0.08, Mountain) — the epistemically sound claim that all knowledge is situated; (2) lived_experience_epistemology (ε≈0.52, Tangled Rope, this story) — the institutional constraint that makes identity-category validation the primary credential; (3) alternative_framework_suppression (ε≈0.68, Snare) — the mechanism that delegitimizes non-identity-based knowledge production about marginalized groups. Each has different ε values, different measurement trajectories, and different resolution pathways. The three are linked by institutional conflation: gatekeepers use the legitimacy of claim 1 to justify the extraction of claims 2 and 3.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lived_experience_epistemology, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
