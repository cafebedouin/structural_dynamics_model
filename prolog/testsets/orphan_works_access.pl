% ============================================================================
% CONSTRAINT STORY: orphan_works_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orphan_works_access, []).

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
 *   constraint_id: orphan_works_access
 *   human_readable: Orphan Works Access Constraint
 *   domain: intellectual_property/cultural_commons
 *
 * SUMMARY:
 *   Orphan works are copyrighted creative works whose authors or rights
 *   holders cannot be identified or located despite diligent searching. The
 *   constraint creates a structural tension between intellectual property
 *   protection (which incentivizes creation) and cultural access (which
 *   requires use of existing works). The same institutional arrangement —
 *   copyright ownership over undiscoverable creators — appears as pure
 *   extraction (Snare) to institutions trying to digitize cultural materials,
 *   as a temporary problem with a generational sunset (Scaffold) to
 *   open-access advocates, as degraded performative infrastructure (Piton)
 *   when filtering through collecting societies' actual distribution rates,
 *   as immutable attribution limits (Mountain) from a civilizational view,
 *   and as legitimate coordination benefit (Rope) for active copyright
 *   holders. The extractiveness has increased over the measurement interval
 *   (0.35 → 0.58) as copyright term extensions have created larger cohorts of
 *   works whose creators are dead and records lost, while simultaneous
 *   digitization pressure has increased the friction cost of accessing these
 *   works. Theater ratio (0.48) is moderate: while the legal framework
 *   presents itself as protecting creators' interests, most orphan works
 *   revenue flows to collective organizations claiming representation of
 *   unlocatable creators rather than to actual creators, creating
 *   performative extraction.
 *
 * KEY AGENTS:
 *   - Orphan Work User (Powerless/Trapped): Archives, researchers, digital humanities scholars facing legal liability for use; cannot locate rights holders; cannot exit without abandoning work
 *   - Cultural Institution (Moderate/Constrained): Libraries, museums, historical societies; benefit from copyright incentives but bear disproportionate cost of orphan works compliance; face resource barriers to diligent search procedures
 *   - Active Copyright Holder (Institutional/Arbitrage): Known creators and their heirs; benefit from copyright protection; experience orphan works constraint only as abstract market competition, not as direct extraction
 *   - Collective Rights Organization (Institutional/Arbitrage): Collecting societies that manage licensing revenue; benefit from orphan works licensing; perform performative representation of absent creators
 *   - Open Access Coalition (Organized/Constrained): Wikipedia, Project Gutenberg, Creative Commons advocates; organized to build alternative pathways (public domain donations, extended collective licensing); see constraint as temporary
 *   - Analytical Observer (Analytical/Analytical): Civilizational perspective that risks naturalizing orphaning as inevitable; structural data suggests orphaning is accelerating due to policy choices, not fixed limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orphan_works_access, 0.58).
domain_priors:suppression_score(orphan_works_access, 0.65).
domain_priors:theater_ratio(orphan_works_access, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orphan_works_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(orphan_works_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(orphan_works_access, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orphan_works_access, tangled_rope).
narrative_ontology:human_readable(orphan_works_access, "Orphan Works Access Constraint").
narrative_ontology:topic_domain(orphan_works_access, "intellectual_property/cultural_commons").

domain_priors:requires_active_enforcement(orphan_works_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orphan_works_access, original_copyright_holders).
narrative_ontology:constraint_beneficiary(orphan_works_access, collective_rights_societies).
narrative_ontology:constraint_victim(orphan_works_access, cultural_institutions).
narrative_ontology:constraint_victim(orphan_works_access, researchers).
narrative_ontology:constraint_victim(orphan_works_access, public_domain_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORPHANED WORK USER (SNARE) — Archives, researchers, and digital humanities scholars cannot use works whose creators are unknown or untraceable without risking infringement liability. Exit is structurally barred: legal use requires either finding the rights holder (often impossible) or obtaining permission (impossible if holder cannot be located). Maximum suppression and no exit path.
constraint_indexing:constraint_classification(orphan_works_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CULTURAL INSTITUTION (TANGLED ROPE) — Libraries and archives genuinely benefit from copyright structure (it drives creation of new works that enter their collections). But orphan works create asymmetric extraction: institutions must maintain expensive diligent search procedures and risk liability even when efforts are sincere. Mixed coordination benefit with embedded extraction cost.
constraint_indexing:constraint_classification(orphan_works_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: KNOWN/ACTIVE COPYRIGHT HOLDER (ROPE) — Experiences the constraint as pure coordination: copyright terms enable commercial licensing and control over derivative works. The orphan works problem is not their problem — they actively maintain their works' copyright status. Net beneficiary with no extraction experienced.
constraint_indexing:constraint_classification(orphan_works_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLLECTIVE RIGHTS MANAGEMENT ORGANIZATIONS (PITON) — Orphan works create a substantial portion of revenue for collecting societies through presumed licensing of works whose creators cannot be located. The constraint is performative: societies claim to represent orphan works but cannot distribute proceeds without claimants. Theater derives from the fiction that collecting societies adequately represent absent creators. Extractive mechanism persists through institutional inertia.
constraint_indexing:constraint_classification(orphan_works_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN ACCESS ADVOCATES (SCAFFOLD) — Copyright exceptions (fair use, orphan works licensing, extended collective licenses) represent temporary scaffolding toward a broader public domain. The constraint has a built-in sunset: works published before 1928 are entering public domain in the US; European systems allow extended collective licensing with time limits. Organized actors see the constraint as temporary with declining coercive force.
constraint_indexing:constraint_classification(orphan_works_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ATTRIBUTION LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, some orphaning is structurally inevitable: media decay, record loss, institutional transitions, and the sheer volume of cultural output mean that perfect attribution tracking is impossible. The constraint appears as an immutable limit on knowledge recovery. However, the base properties suggest this is a false summit — the orphaning rate is accelerating due to policy choices (extended copyright terms, broken metadata standards), not fixed by natural law.
constraint_indexing:constraint_classification(orphan_works_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orphan_works_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(orphan_works_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orphan_works_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(orphan_works_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(orphan_works_access, TR),
    TR >= 0.70.

:- end_tests(orphan_works_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. The constraint creates genuine extraction: orphan works remain locked away while potential creators of value are prevented from using them; collecting societies retain orphan licensing revenue without meaningful distribution. But it is not as severe as a pure snare (>0.70) because fair use exceptions exist, some copyright terms are expiring (public domain growth), and cultural institutions can navigate the constraint through expensive but functional diligent search procedures. The value reflects accumulated extraction over time — initial copyright regime (0.35) was more coordination-focused, but extended terms and metadata standardization failures have shifted it toward extraction. Suppression (0.65): High. Legal liability risk, practical barriers to rights holder location, and asymmetric enforcement costs suppress legitimate uses. Institutional actors face real barriers (expensive search procedures, ongoing liability risk) even when acting in good faith. Trapped agents face near-total suppression — they cannot use the works without solving an impossible location problem or accepting legal risk. Theater ratio (0.48): Moderate. The constraint includes genuine functional components — copyright does incentivize creation — but includes substantial performative elements, particularly in collective rights management, where orphan licensing revenue is collected but not distributed, and legal exceptions (fair use) are presented as sufficient when they provide uncertain protection.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon generates radically different classifications depending on the observer's position. The powerless orphan work user experiences it as a snare: insurmountable legal barrier with no exit. The moderate cultural institution experiences mixed coordination (copyright incentives for future works) and extraction (orphan works costs), producing tangled rope. The active copyright holder experiences pure coordination — copyright works for them — producing rope. The collecting society experiences a profitable revenue stream (piton) maintained through performative representation of absent creators. Open-access advocates see a temporary problem with a generational sunset (scaffold) as copyright terms expire and new exceptions emerge. The civilizational observer risks naturalizing the constraint as inherent to knowledge recovery, but the extractiveness trajectory (rising from 0.35 to 0.58) reveals policy-driven accumulation, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from power level, exit options, and structural position relative to extraction flows. Trappers (orphan work users) with powerless/trapped status experience maximum d, producing high experienced extractiveness. Constrained institutional actors (cultural institutions) with victim status experience moderate d. Beneficiary institutions with arbitrage options (known copyright holders, collecting societies) experience low or negative d. The constraint's beneficiaries are not primarily creators (they often cannot be located to benefit) but copyright-holding corporations and collective rights organizations that can monetize the uncertainty. This mismatch — between stated beneficiaries (creators) and actual beneficiaries (legal entities controlling unlocatable works) — is the core structural dynamic that distinguishes this as tangled rope rather than pure rope.
 *
 * MANDATROPHY ANALYSIS:
 *   DECOMPOSITION OPPORTUNITY: Orphan works should potentially decompose into two related constraints: (1) Copyright Term Extension (a snare that locks works away through extended protection periods), and (2) Metadata Loss (a mountain that makes attribution recovery structurally harder regardless of policy). The current story conflates them. The extractiveness value (0.58) reflects the hybrid: some works are orphaned due to policy choices (term extension, inadequate metadata standards) and some due to inevitable record loss. Resolving the mandatrophy would require separating these mechanisms. For this integrated story, the mandatrophy is resolved by the multi-perspectival approach: all six types are legitimate readings, and their coexistence shows that the constraint is genuinely a hybrid coordination-extraction mechanism, not a false designation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orphaning_cause_decomposition,
    'What proportion of orphaning is caused by natural attrition (media decay, institutional collapse) versus policy choices (extended copyright terms, metadata standards failure)?',
    'Longitudinal dataset of works with known creation dates tracking metadata preservation and accessibility over time; comparison of orphaning rates across different copyright regimes',
    'If primarily policy-driven (>60%): constraint classification drops to pure Tangled Rope or Snare; the ''immutable'' framing is false naturalization. If primarily attrition-driven (>60%): mountain classification is more defensible; the policy choices matter less than physical limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orphaning_cause_decomposition, empirical, 'Relative contribution of natural attrition vs policy choices to orphaning').

omega_variable(
    diligent_search_efficacy,
    'How effective are standardized diligent search procedures at actually locating rights holders for works with verifiable creation records?',
    'Empirical study of successful diligent searches compared to total searches; analysis of what factors predict location vs failure to locate',
    'If success rate >70% for recorded works: suppression metric should be downgraded; the trap is avoidable. If success rate <30%: suppression and extraction are severe; current legal frameworks are performative barriers with minimal functional value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diligent_search_efficacy, empirical, 'Actual success rates of diligent search procedures').

omega_variable(
    collecting_society_claimant_ratio,
    'What percentage of orphan works licensing revenue collected by rights management organizations is actually distributed to verified rights holders versus retained as administrative overhead?',
    'Financial audits of major collecting societies; tracking of claimant identification rates and distribution periods',
    'If >50% distributed: collecting societies function as genuine coordination intermediaries. If <30% distributed: the piton classification is conservative — the constraint is primarily extractive with minimal coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collecting_society_claimant_ratio, empirical, 'Proportion of orphan licensing revenue actually reaching rights holders').

omega_variable(
    fair_use_cultural_adequacy,
    'Do existing fair use/fair dealing exceptions provide sufficient practical space for cultural institutions to digitize and preserve orphan works, or are legal uncertainty and liability risks sufficiently severe to function as de facto prohibition?',
    'Survey of cultural institutions documenting digitization activity with and without explicit legal exceptions; analysis of litigation risk for good-faith orphan works use',
    'If fair use is functionally adequate: suppression is lower than 0.65; the constraint is constraint not snare. If fair use provides insufficient legal certainty: suppression is accurate or conservative; the constraint binds severely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_cultural_adequacy, empirical, 'Practical adequacy of fair use protections for orphan works digitization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orphan_works_access, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orphan_tr_t0, orphan_works_access, theater_ratio, 0, 0.32).
narrative_ontology:measurement(orphan_tr_t15, orphan_works_access, theater_ratio, 15, 0.4).
narrative_ontology:measurement(orphan_tr_t30, orphan_works_access, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(orphan_be_t0, orphan_works_access, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orphan_be_t15, orphan_works_access, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(orphan_be_t30, orphan_works_access, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orphan_works_access, information_standard).
narrative_ontology:affects_constraint(orphan_works_access, copyright_term_extension).
narrative_ontology:affects_constraint(orphan_works_access, metadata_preservation_standards).

% DUAL FORMULATION NOTE:
% Orphan works access is downstream of copyright term extension policy (which creates larger cohorts of untraceable creators) and upstream of metadata preservation standards (which determine how many works become orphaned through record loss). The constraint family spans policy (term lengths), institutional (metadata standards), and technical (searchability) domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orphan_works_access, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
