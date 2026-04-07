% ============================================================================
% CONSTRAINT STORY: institutional_ontology_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_ontology_gatekeeping, []).

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
 *   constraint_id: institutional_ontology_gatekeeping
 *   human_readable: Institutional Ontology Gatekeeping
 *   domain: epistemology/institutional_power/knowledge_production
 *
 * SUMMARY:
 *   Institutional ontology gatekeeping is the enforcement of accepted
 *   categorical frameworks and conceptual distinctions by established
 *   institutions, suppressing alternative ontological proposals unless they
 *   conform to institutional legitimacy standards. This constraint operates
 *   across all knowledge-producing domains: academia, science, professions,
 *   policy institutes. The gating mechanisms include peer review, credential
 *   requirements, citation hierarchies, conference acceptance, journal
 *   publication, and institutional affiliation. The constraint exhibits a
 *   declining extractiveness trajectory (0.68 → 0.58) as alternative channels
 *   (preprints, social media, open collaboration) mature, but theater ratio
 *   increases (0.35 → 0.64) as institutions intensify performative
 *   enforcement of ontological standards despite declining actual gatekeeping
 *   power. The Tangled Rope classification reflects genuine coordination
 *   functions (shared conceptual frameworks enable collaboration) alongside
 *   asymmetric extraction (innovators must suppress heterodoxy or lose
 *   access). The Piton perspective captures institutional credential systems
 *   that persist through inertia despite weakening functional gatekeeping.
 *
 * KEY AGENTS:
 *   - Excluded Conceptual Innovators: Primary victims (powerless/trapped) — cannot reframe ontology without institutional credentials; bear full suppression cost
 *   - Early-Career Scholars: Secondary victims (moderate/constrained) — benefit from institutional epistemic frameworks but suppressed if proposing heterodoxy; high cost to propose alternatives
 *   - Established Institutions: Primary beneficiaries (institutional/arbitrage) — maintain ontological authority and gatekeeping power; coordinate through shared categorical frameworks
 *   - Epistemic Dissident Coalition: Organized victims (organized/mobile) — build alternative knowledge systems; face suppression but have exit options (preprints, independent publishing, community validation)
 *   - Disciplinary Credential System: Institutional actor (institutional/arbitrage) — maintains performative gatekeeping through PhD programs, journals, conferences; declining binding power but increasing theatrical enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional gatekeeping as inherent to organized knowledge rather than contingent enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_ontology_gatekeeping, 0.58).
domain_priors:suppression_score(institutional_ontology_gatekeeping, 0.68).
domain_priors:theater_ratio(institutional_ontology_gatekeeping, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_ontology_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_ontology_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_ontology_gatekeeping, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_ontology_gatekeeping, tangled_rope).
narrative_ontology:human_readable(institutional_ontology_gatekeeping, "Institutional Ontology Gatekeeping").
narrative_ontology:topic_domain(institutional_ontology_gatekeeping, "epistemology/institutional_power/knowledge_production").

domain_priors:requires_active_enforcement(institutional_ontology_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_ontology_gatekeeping, established_institutions).
narrative_ontology:constraint_beneficiary(institutional_ontology_gatekeeping, credentialed_arbiters).
narrative_ontology:constraint_victim(institutional_ontology_gatekeeping, alternative_framers).
narrative_ontology:constraint_victim(institutional_ontology_gatekeeping, non_institutional_knowledge_producers).
narrative_ontology:constraint_victim(institutional_ontology_gatekeeping, conceptual_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CONCEPTUAL INNOVATOR (SNARE) — Cannot reframe institutional ontology without credentials and institutional backing. Trapped by certification requirements, publication gatekeeping, and epistemic authority structures. Experiences pure extraction: innovation is suppressed unless it legitimates existing institutional categories. Zero exit options.
constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER SCHOLAR (TANGLED ROPE) — Benefits from access to institutional channels for validation and publication, but extraction occurs through heterodox work requiring suppression of novel framings. High cost to propose alternative ontologies; also benefits from established epistemological categories that enable collaboration. Constrained exit — can exit by abandoning academic career, but carries significant cost.
constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED INSTITUTION (ROPE) — Experiences the ontology gatekeeping as coordination mechanism: maintains epistemic standards, validates knowledge claims, prevents categorical confusion. Net beneficiary from gatekeeping — consolidates institutional authority. Experiences constraint as pure coordination with high arbitrage options (can shift accepted categories to maintain relevance).
constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMIC DISSIDENT COALITION (TANGLED ROPE) — Organized agents (heterodox schools, independent researchers, alternative knowledge systems) benefit from shared alternative frameworks but face suppression from institutional gatekeeping. Mobile exit options (can publish via preprints, alternative journals, social media) mean extraction is partial rather than total. Active enforcement required to contain the alternative framings.
constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DISCIPLINARY CREDENTIAL SYSTEM (PITON) — The credential system (PhD programs, peer review, citation hierarchies) maintains ontological gatekeeping through theatrical enforcement: the ritual of credential validation persists even when its gatekeeping function is weakened by alternative channels (open repositories, social media scholarship, interdisciplinary synthesis). Theater ratio high because enforcement is largely performative (credentials have declining binding power) but institutions continue performing gatekeeping rituals.
constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some ontological gatekeeping is inevitable: any knowledge system must make distinctions between valid and invalid conceptual frameworks, which requires standards and authority. This perspective naturalizes institutional gatekeeping as inherent to organized knowledge. However, the structural data contradicts this — the institutional gatekeeping examined here is contingent on specific power asymmetries and enforcement mechanisms, not natural law.
constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_ontology_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_ontology_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_ontology_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_ontology_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(institutional_ontology_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Institutional gatekeeping suppresses heterodox ontologies and captures innovation value through credential/publication systems. However, extractiveness is declining (0.68 → 0.58 over interval) as alternative publication and validation channels (arXiv, social media, independent researchers, citizen science) bypass institutional gatekeeping. The extraction is not total because alternatives exist and are maturing. Suppression (0.68): High. Barriers to heterodox ontological work include publication gatekeeping, credential requirements, funding dependence on institutional validation, social cost of dissent, and epistemic authority concentrated in institutions. Suppression operates through both structural mechanisms (you cannot publish without credentials) and internalized mechanisms (scholars internalize institutional ontological standards as objective truth). Theater ratio (0.64): Moderately high. Institutional enforcement increasingly emphasizes performative gatekeeping — credential validation rituals, peer review theater, citation hierarchies — even as the actual gatekeeping power declines. Institutions continue gatekeeping performances to maintain authority, not because they effectively control knowledge circulation anymore.
 *
 * PERSPECTIVAL GAP:
 *   Six distinct classifications from a single constraint structure reveal how institutional ontology gatekeeping operates asymmetrically across power positions. The powerless excluded innovator sees a Snare (no exit, pure extraction). The early-career scholar sees a Tangled Rope (partial exit via career change, mixed coordination/extraction). The institutional beneficiary sees Rope (pure coordination, low extraction). The organized dissidents see Tangled Rope (exit exists but constrained, extraction is real). The credential system sees its own Piton status (performative, losing function). The civilizational observer risks a false summit Mountain (natural law). The perspectival gap is diagnostic: if all views collapsed to Mountain, the gatekeeping would be truly immutable. The six-type spread indicates the constraint is enforced asymmetry, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (established institutions, credentialed arbiters) have institutional power and arbitrage exit options, yielding low or negative directionality values (d ≈ 0.05–0.20): they experience the constraint as coordination and low effective extraction. Victims (alternative framers, excluded innovators) have powerless or moderate power with trapped or constrained exit, yielding high directionality values (d ≈ 0.80–0.95): they experience maximum effective extraction. The dissident coalition's organized power with mobile exit options yields moderate d (≈0.55), placing them at the boundary between Snare and Tangled Rope. The early-career scholar's moderate power with constrained exit (can exit by leaving academia but at high career cost) yields high-moderate d (≈0.70–0.75), making Tangled Rope accurate for their perspective. Directionality differentiates institutional actors (established vs captured, credentialed vs excluded) through their power + exit relationship to the gatekeeping extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that institutional ontology gatekeeping combines genuine coordination functions (shared frameworks enable collaboration) with asymmetric extraction (control of ontological authority is monopolized). The Tangled Rope classification from multiple perspectives confirms this hybrid structure. The declining extractiveness (0.68 → 0.58) combined with increasing theater (0.35 → 0.64) suggests the coordination function is stable while the extraction function is being eroded by alternative channels. If extractiveness continued declining while theater remained high, the constraint would eventually meet the Piton threshold (theater ≥ 0.70, ε ≤ 0.25), indicating complete degradation to purely performative institutional ritual. The mandatrophy is resolved by: (1) confirming the tangled structure (coordination + extraction coexist), (2) measuring the extraction decline (alternative channels reducing binding power), (3) tracking theater increase (institutions performing gatekeeping as power erodes), (4) recognizing the false summit (naturalizing enforcement as natural law). The analytical observer must resist the mountain classification by noting that institutional gatekeeping is contingent on power asymmetries, not inherent to knowledge systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_vs_quality_control,
    'To what extent does institutional ontology gatekeeping function as quality control versus pure rent extraction?',
    'Comparative analysis: track the fate of heterodox concepts that were initially rejected by institutions but later adopted; measure adoption lag for valid innovations; assess whether alternatives channels (preprints, social media) produce equivalent or superior epistemic outputs',
    'If primarily quality control (high legitimate gatekeeping function): perspectives should shift toward Rope and Tangled Rope. If primarily extraction (low legitimate function): classifications hold as Snare-dominant, suggesting false certification of expertise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_quality_control, empirical, 'Whether gatekeeping functions as quality control or rent extraction').

omega_variable(
    ontological_commensurability,
    'Are alternative ontological frameworks fundamentally incommensurable with institutional standards, or is institutional gatekeeping suppressing alternatives that would be recognized as valid if evaluated on their own terms?',
    'Meta-analysis of rejected proposals: classify rejections as (a) incommensurable paradigm differences vs (b) suppressible alternative framings; assess whether hetero-institutional evaluation produces different verdicts',
    'If fundamentally incommensurable: some gatekeeping function is necessary (mountain element is real). If suppressible: extraction is pure suppression of valid knowledge (Snare is accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_commensurability, conceptual, 'Whether alternative ontologies are incommensurable or suppressible').

omega_variable(
    credential_cascade_binding,
    'How much of the institutional ontology gatekeeping depends on cascading credential validation (each institution trusts credentials from higher-status institutions) versus direct epistemic evaluation?',
    'Track citation trajectories and adoption patterns for ideas from non-credentialed sources that receive institutional credentials through intermediaries; assess decoupling between institutional status and epistemic validity in heterodox high-citation works',
    'If heavily credential-cascaded: gatekeeping is contingent on status signaling and can collapse if alternative validation channels mature (Piton trajectory, Scaffold sunset). If epistemic evaluation dominates: gatekeeping has real coordination function (Rope/Mountain element).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_cascade_binding, empirical, 'Degree to which credential cascading drives gatekeeping versus direct evaluation').

omega_variable(
    identity_lock_in_academia,
    'To what extent is the suppression of heterodox ontologies driven by identity-locked institutional actors versus structural incentive misalignment?',
    'Comparative analysis: institutions with identity-fusion to traditional categories vs those with flexible self-concept; measure openness to conceptual innovation by institutional type; track whether leadership transitions produce ontological shifts',
    'If identity-locked dominates: gatekeeping persists even when structural incentives change, and exit from institutional affiliation becomes necessary (Piton perspective shifts toward identity_locked exit). If incentive-driven: perspective shifts with institutional incentive alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_academia, empirical, 'Whether suppression is driven by institutional identity lock or structural incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_ontology_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iog_tr_t0, institutional_ontology_gatekeeping, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iog_tr_t10, institutional_ontology_gatekeeping, theater_ratio, 10, 0.5).
narrative_ontology:measurement(iog_tr_t20, institutional_ontology_gatekeeping, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(iog_be_t0, institutional_ontology_gatekeeping, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(iog_be_t10, institutional_ontology_gatekeeping, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(iog_be_t20, institutional_ontology_gatekeeping, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_ontology_gatekeeping, identity_coordination).
narrative_ontology:affects_constraint(institutional_ontology_gatekeeping, academic_credentialism).
narrative_ontology:affects_constraint(institutional_ontology_gatekeeping, interdisciplinary_gatekeeping).
narrative_ontology:affects_constraint(institutional_ontology_gatekeeping, scientific_paradigm_entrenchment).

% DUAL FORMULATION NOTE:
% Institutional ontology gatekeeping is the parent constraint. Downstream constraints include discipline-specific manifestations (academic credentialism in academia, scientific paradigm entrenchment in physics/biology) and cross-cutting constraints (interdisciplinary gatekeeping, which reflects gatekeeping applied to boundary-crossing frameworks). Each downstream constraint has its own extractiveness value reflecting domain-specific enforcement intensity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_ontology_gatekeeping, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
