% ============================================================================
% CONSTRAINT STORY: strict_scrutiny_tier__narrow_tailoring_mechanics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strict_scrutiny_tier__narrow_tailoring_mechanics, []).

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
 *   constraint_id: strict_scrutiny_tier__narrow_tailoring_mechanics
 *   human_readable: Narrow Tailoring Doctrine: Means Inquiry as Operational Filter in Strict Scrutiny
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   Narrow tailoring doctrine is the operational machinery of strict scrutiny
 *   that determines whether a race-conscious government action survives
 *   constitutional challenge. The doctrine does not address whether the
 *   government's interest is compelling (that is the first tier), but rather
 *   whether the classification fits the interest tightly, whether
 *   race-neutral alternatives have been exhausted, and whether the program's
 *   duration is appropriately limited. This reading of the
 *   strict_scrutiny_tier kernel isolates narrow tailoring as a distinct
 *   structural constraint: while the compelling interest inquiry asks 'is the
 *   goal worth pursuing?', narrow tailoring asks 'is this the
 *   least-restrictive way to pursue it?' Most race-conscious programs fail at
 *   the narrow-tailoring stage, not because the government's interest fails,
 *   but because the doctrine's machinery — the exhaustion requirement, the
 *   fitness scrutiny, the durational limit — creates cumulative gatekeeping
 *   that blocks programs that would survive if any single element were
 *   removed. The constraint exhibits the signature of a tangled_rope at
 *   institutional levels (genuine coordination function for preventing
 *   overreach, genuine extraction preventing sustained remedies) and snare at
 *   the level of powerless agents (trapped between contradictory legal
 *   commands). The measurements show increasing suppression and
 *   extractiveness from 1978 (Regents v. Bakke, where narrow tailoring first
 *   crystallized) through 2023 (post-SFFA era), tracking the doctrine's
 *   gradual hardening.
 *
 * KEY AGENTS:
 *   - Remedial Program Designers: Primary victim (powerless/trapped) — face irreconcilable constraints from exhaustion requirement, fitness scrutiny, and durational limits. Exit blocked by disparate impact liability on one side and narrow-tailoring vulnerability on the other.
 *   - Constitutional Challengers: Primary beneficiary (institutional/arbitrage) — use narrow-tailoring machinery to displace race-conscious policies without bearing remedial responsibility. High arbitrage because victory on means inquiry is often easier than on interest inquiry.
 *   - Civil Rights Organizations: Organized victim (organized/constrained) — seek race-conscious remedies but face organized opposition using narrow-tailoring gatekeeping. Constrained exit: cannot move to state policy (dormant Commerce issues), cannot fully abandon race-conscious strategy (discrimination continues unaddressed), cannot easily challenge doctrine (courts are gatekeepers).
 *   - Affected Discrimination Groups: Generational victim (powerless/trapped) — each generation faces renewed means inquiry despite persistent effects of prior discrimination. Trapped in perpetual remedial limbo.
 *   - Courts as Doctrinal Custodians: Institutional arbiters (institutional/arbitrage) — maintain narrow-tailoring doctrine's legitimacy as rigorous scrutiny; benefit from appearance of principled review; arbitrage through application flexibility (can apply rigorously or deferentially depending on political valence).
 *   - Government Remedial Authorities: Mixed institutional position (powerful/mobile) — experience coordination benefit (doctrine prevents overreach) and extraction cost (doctrine limits their remedial options). Mobile because they can shift strategy, challenge durational extensions, or abandon race-conscious approach.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strict_scrutiny_tier__narrow_tailoring_mechanics, 0.58).
domain_priors:suppression_score(strict_scrutiny_tier__narrow_tailoring_mechanics, 0.72).
domain_priors:theater_ratio(strict_scrutiny_tier__narrow_tailoring_mechanics, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strict_scrutiny_tier__narrow_tailoring_mechanics, extractiveness, 0.58).
narrative_ontology:constraint_metric(strict_scrutiny_tier__narrow_tailoring_mechanics, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(strict_scrutiny_tier__narrow_tailoring_mechanics, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strict_scrutiny_tier__narrow_tailoring_mechanics, tangled_rope).
narrative_ontology:human_readable(strict_scrutiny_tier__narrow_tailoring_mechanics, "Narrow Tailoring Doctrine: Means Inquiry as Operational Filter in Strict Scrutiny").
narrative_ontology:topic_domain(strict_scrutiny_tier__narrow_tailoring_mechanics, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(strict_scrutiny_tier__narrow_tailoring_mechanics).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(strict_scrutiny_tier__narrow_tailoring_mechanics, '122b3643-16b7-46b2-97ab-b8a468c15709').
narrative_ontology:cs_kernel_codification('122b3643-16b7-46b2-97ab-b8a468c15709', formalized).
narrative_ontology:cs_authority_grounding('122b3643-16b7-46b2-97ab-b8a468c15709', lineage).
narrative_ontology:cs_interpretation_layer_present('122b3643-16b7-46b2-97ab-b8a468c15709').
narrative_ontology:cs_reading_relation('122b3643-16b7-46b2-97ab-b8a468c15709', strict_scrutiny_tier__compelling_interest_jurisprudence, influences).
narrative_ontology:cs_reading_relation('122b3643-16b7-46b2-97ab-b8a468c15709', strict_scrutiny_tier__fatal_in_fact_trajectory, coexists_with).
narrative_ontology:cs_axiom('122b3643-16b7-46b2-97ab-b8a468c15709', foundational, means_inquiry_separable_from_interest).
narrative_ontology:cs_axiom_status(means_inquiry_separable_from_interest, holdable).
narrative_ontology:cs_axiom_grounding('122b3643-16b7-46b2-97ab-b8a468c15709', means_inquiry_separable_from_interest, deontological).
narrative_ontology:cs_axiom('122b3643-16b7-46b2-97ab-b8a468c15709', foundational, proportionality_constraint_legitimate).
narrative_ontology:cs_axiom_status(proportionality_constraint_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('122b3643-16b7-46b2-97ab-b8a468c15709', proportionality_constraint_legitimate, deontological).
narrative_ontology:cs_reference_frame('122b3643-16b7-46b2-97ab-b8a468c15709', strict_scrutiny_as_rigorous_means_review).
narrative_ontology:cs_drift_state('122b3643-16b7-46b2-97ab-b8a468c15709', post_sffa_contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('122b3643-16b7-46b2-97ab-b8a468c15709', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(strict_scrutiny_tier__narrow_tailoring_mechanics, strict_scrutiny_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strict_scrutiny_tier__narrow_tailoring_mechanics, constitutional_challengers).
narrative_ontology:constraint_beneficiary(strict_scrutiny_tier__narrow_tailoring_mechanics, opposing_counsel).
narrative_ontology:constraint_beneficiary(strict_scrutiny_tier__narrow_tailoring_mechanics, institutional_skeptics_of_race_conscious_remedies).
narrative_ontology:constraint_victim(strict_scrutiny_tier__narrow_tailoring_mechanics, race_conscious_remedial_programs).
narrative_ontology:constraint_victim(strict_scrutiny_tier__narrow_tailoring_mechanics, flexible_implementation_approaches).
narrative_ontology:constraint_victim(strict_scrutiny_tier__narrow_tailoring_mechanics, durational_extension_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REMEDIAL PROGRAM DESIGNER (SNARE) — Faces irreconcilable constraints: race-conscious remedies trigger strict scrutiny; race-neutral alternatives are presumed available (exhaustion requirement); any temporal extension invites duration scrutiny; any flexibility in implementation is vulnerability to narrow-tailoring attack. Suppression is maximal: the designer must simultaneously prove necessity, demonstrate unavailability of less-restrictive alternatives, maintain tight fit, and justify duration — typically on a record that was compiled before narrow-tailoring doctrine matured. Exit is blocked: failure to race-consciously remedy invites disparate impact liability; race-conscious remedy invites narrow-tailoring challenge. Trapped in a binary with no structural exit.
constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL CHALLENGER (ROPE) — Constitutional challenger experiences narrow tailoring as a coordination mechanism solving a legitimate doctrinal problem: ensuring that classifications based on race are genuinely necessary (not convenient) and genuinely proportional (not excessive). The machinery of means inquiry — exhaustion of alternatives, fitness of design, durational limitation — addresses the coordination goal of preventing overreach while permitting remedies. Challenger has arbitrage: victory on narrow-tailoring grounds displaces race-conscious policy without bearing ongoing remedial responsibility. Net coordination, not extraction, from this institutional position.
constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized groups seeking race-conscious remedies experience both coordination and extraction. The doctrine coordinates legitimate scrutiny of remedial fit — excess or perpetual remedies should face serious judicial review. But the doctrine also extracts: the burdens of proof (challenger must prove necessity, unavailability of alternatives, fitness) are asymmetric; racial remedies bear perpetual vulnerability while classifications benefiting majority groups face minimal scrutiny; the exhaustion requirement presumes race-neutral alternatives exist without forcing government to prove their actual effectiveness. Constrained exit: moving to state/local policy faces dormant Commerce Clause and full faith/credit issues; moving to non-racial remedies often fails to address root discrimination; judicial appeal faces strict scrutiny gatekeepers. Both genuine coordination function and real extraction present.
constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT BENEFICIARY / MAJORITY INTERESTS (TANGLED ROPE) — Dominant groups benefit from narrow tailoring's gatekeeping: the doctrine prevents sustained, flexible, adaptive race-conscious remedies from becoming institutionalized. But the beneficiary also experiences coordination: narrow tailoring ensures remedies truly address identified harm rather than becoming proxy for broad social reengineering. Mobile exit: majority interests can shift remedial strategies, challenge durational extensions, file counter-suits arguing reverse discrimination. Experienced as mixed because the doctrine serves stability interests (coordination) while also protecting against displacement (extraction).
constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: AFFECTED GROUP / DISCRIMINATION VICTIMS (SNARE) — Generational perspective reveals the long-term extraction: narrow tailoring's doctrinal machinery, applied recursively across decades, prevents remedial programs from achieving intergenerational remediation. Each generation faces renewed means inquiry; each program redesign must re-justify necessity; durational limits force sunset despite persistent effects of discrimination. The group experiences the doctrine as a perpetual gauntlet that locks in disadvantage: trapped because exit requires either (a) abandoning race-conscious remedies (accepting discrimination continues unaddressed) or (b) challenging narrow tailoring itself (systemically implausible given judicial doctrine). Generational suppression is the binding mechanism.
constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: JURISPRUDENTIAL ARCHIVE / DOCTRINAL SYSTEM (PITON) — At the civilizational/archival level, narrow tailoring appears as a largely performative doctrinal mechanism. The theater: repeated invocation of means inquiry produces the appearance of rigorous case-by-case scrutiny while actual outcomes follow predictable patterns (race-conscious remedies are struck down at rates orders of magnitude higher than other classifications). The function atrophies: doctrinal invocations of 'narrow tailoring' become formulaic; courts do not deeply investigate whether race-neutral alternatives are genuinely available or why durationally-limited remedies achieve remediation. The doctrine persists through institutional inertia and professional legitimacy (it appears rigorous) rather than because it effectively coordinates the doctrinal goals it claims. Theater ratio (0.48) reflects that while narrow tailoring retains some functional scrutiny, significant performativity surrounds its application.
constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a pure logical standpoint, narrow tailoring might appear to be an immutable principle: any constitutional remedy must be proportional to the injury; excess or perpetuity in remedy is inherently problematic; race-conscious classifications require special scrutiny. This perspective sees narrow tailoring as a logical necessity akin to proportionality in all legal remedies. However, the structural data reveals this as a false summit: narrow tailoring's suppression (0.72) is not a logical law but an institutional choice about WHERE suppression is executed (in the means inquiry rather than elsewhere) and WHO bears the burden of proof (remedial program designers, not challengers). The 'naturalness' of narrow tailoring masks what is actually a doctrinal distribution of epistemic labor.
constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strict_scrutiny_tier__narrow_tailoring_mechanics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strict_scrutiny_tier__narrow_tailoring_mechanics, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strict_scrutiny_tier__narrow_tailoring_mechanics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strict_scrutiny_tier__narrow_tailoring_mechanics, TR),
    TR >= 0.70.

:- end_tests(strict_scrutiny_tier__narrow_tailoring_mechanics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Narrow tailoring permits race-conscious remedies in principle but extracts by distributing approval rights to challengers: the remedial program must prove necessity, prove unavailability of alternatives, prove fitness, and defend durational limits. This distribution of burdens is asymmetric and extractive — benefiting challengers while burdening program designers. The value reflects that extraction is real but not total: some race-conscious remedies do survive (though at low rates, 5-15% in recent data); the constraint is not a blanket prohibition but a gatekeeping mechanism that makes remedies expensive to maintain. Suppression (0.72): High. The doctrine creates multiple suppressive mechanisms: (1) exhaustion of alternatives presumes race-neutral solutions exist without empirical verification; (2) fitness inquiry requires tight fit, blocking flexible, adaptive remedies; (3) durational limits force sunset despite persistent effects of discrimination; (4) the cumulative burden creates epistemic suppression — many remedies never reach implementation because the doctrinal gauntlet is too costly. The measurement trajectory shows hardening: suppression increased from 0.62 (late 1970s Bakke era) to 0.72 (post-SFFA 2020s). Theater ratio (0.48): Moderate. The doctrine retains genuine scrutiny — courts do sometimes engage in substantive means inquiry — but shows increasing performativity: means inquiry often becomes formulaic invocation of exhaustion without deep investigation of whether alternatives actually exist; fitness is asserted without careful empirical analysis. The trajectory (0.35 → 0.48) shows increasing theater, consistent with observations that narrow tailoring has become a weaponized doctrinal form with predictable gatekeeping outcomes despite appearance of rigorous case-by-case review.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why single-perspective classification fails catastrophically. The institutional challenger sees coordination (rope) — a legitimate mechanism ensuring remedies are proportional, not excessive. The remedial program designer sees extraction (snare) — irreconcilable constraints that trap all strategies. The civil rights coalition sees mixed extraction-and-coordination (tangled rope) — real gatekeeping against overreach alongside real suppression of effective remedies. The affected discrimination groups see generational entrapment (snare) — perpetual vulnerability to renewed challenge. The jurisprudential system sees its own degradation (piton) — doctrine persists through inertia despite decreasing functional scrutiny. The analytical observer risks seeing natural law (mountain) — proportionality as a logical necessity — but the structural data reveals this is a false summit: narrow tailoring's suppression is a choice about WHERE suppression is executed (in the means inquiry), not a logical inevitability. The perspectival gap reveals that narrow tailoring functions as an extractive gatekeeping mechanism disguised as neutral scrutiny.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies across perspectives based on structural position and exit options. Institutional challengers with arbitrage exit have low d (around 0.20) — they benefit and can exit easily — producing negative f(d) around -0.08, dampening their experienced χ. Remedial designers (powerless/trapped) have high d (around 0.90) — they bear costs and cannot exit — producing f(d) around 1.35, amplifying their experienced χ. Organized civil rights groups (organized/constrained) have moderate-high d (around 0.55) — mixed position with constrained exit — producing f(d) around 0.75, giving them moderate-high experienced χ. The scope modifier σ(S) is 1.0 (national scale), so χ = ε × f(d) × 1.0. For the most suppressed agent (remedial designers), χ ≈ 0.58 × 1.35 × 1.0 = 0.78, which correctly reflects near-snare-level extraction for that perspective despite the base_properties claiming tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through perspectival plurality: narrow tailoring is tangled_rope at the institutional level (genuine coordination + genuine extraction coexist) AND snare at the level of program designers. No single type is correct; the full classification space (snare, tangled_rope, rope, piton) captures different agent experiences. The constraint resolves mandatrophy by showing that coordination and extraction are genuinely entangled in the doctrine's structure: ensuring remedies are proportional (coordination goal) is operationalized through burden-shifting and gatekeeping (extraction mechanism). The machinery cannot separate them without ceasing to function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_availability_presumption,
    'Are race-neutral alternatives genuinely available for remedying identified discrimination, or does the exhaustion requirement impose an empirically false premise?',
    'Comparative analysis of remedial outcomes: race-neutral programs vs race-conscious programs in addressing the same discrimination; longitudinal study of whether race-neutral alternatives, when implemented, achieve equivalent remediation',
    'If alternatives genuinely available: narrow tailoring enforces legitimate proportionality constraint. If alternatives unavailable or ineffective: exhaustion requirement is performative gatekeeping that blocks effective remedies. Current jurisprudence presumes availability without empirical verification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_availability_presumption, empirical, 'Whether race-neutral alternatives actually exist for remedying identified discrimination').

omega_variable(
    durational_sufficiency_paradox,
    'What temporal duration is sufficient for a race-conscious remedy to achieve generational remediation while remaining limited enough to pass narrow-tailoring scrutiny?',
    'Empirical study of remediation timelines: how long do effects of discrimination persist? How long do remedial programs need to operate to address root causes? Whether courts'' actual durational limits align with genuine remediation timelines or reflect skepticism toward remedies generally.',
    'If durational limits align with remediation timelines: temporal constraint is functional. If durational limits are shorter than remediation timelines: narrow tailoring forces sunset before remedy achieves effect — becomes pure extraction mechanism preventing generational remediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durational_sufficiency_paradox, empirical, 'Whether narrow-tailoring durational limits permit achievement of remedial goals').

omega_variable(
    means_inquiry_asymmetry,
    'Does strict scrutiny''s means inquiry impose asymmetric burdens on race-conscious programs vs. burdens on race-neutral programs or programs benefiting majority groups?',
    'Doctrinal audit: compare judicial treatment of alternatives in race-conscious cases vs. other strict scrutiny contexts vs. rational basis review; track which party bears burden of proof and what standard of proof applies across classification types',
    'If asymmetry exists and is acknowledged: narrow tailoring is revealed as an extractive gatekeeping mechanism, not neutral scrutiny. If asymmetry is structural but unacknowledged: the doctrine naturalizes bias as logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(means_inquiry_asymmetry, conceptual, 'Whether narrow-tailoring scrutiny is applied asymmetrically across classification types').

omega_variable(
    contested_reading_kernel_status,
    'Is this the narrow_tailoring_mechanics reading of the strict_scrutiny_tier kernel, or is narrow tailoring better understood as a distinct kernel from compelling interest?',
    'Jurisprudential analysis: are narrow tailoring and compelling interest genuinely separable doctrinal inquiries, or does narrow tailoring presume (in practice) specific views about what interests are compelling? Do challengers systematically use narrow tailoring to collapse interests that satisfy the first-tier test?',
    'If truly separable: the reading correctly isolates narrow tailoring as a distinct constraint with its own ε and suppression values. If doctrinally entangled: the constraint should be reframed as a sub-doctrine within compelling interest, with different ε reflecting the merged doctrine''s actual gatekeeping power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_reading_kernel_status, conceptual, 'Whether narrow tailoring is a separable doctrine or entangled with compelling interest within strict scrutiny').

omega_variable(
    post_sffa_doctrine_stability,
    'After Students for Fair Admissions v. Harvard, is narrow tailoring doctrine stable or in transition toward greater gatekeeping?',
    'Tracking post-SFFA jurisprudence: whether narrow tailoring is applied with greater rigor, whether courts explicitly or implicitly adopt fatal-in-fact trajectory, whether durational limits or exhaustion requirements harden',
    'If stable: current ε=0.58 reflects equilibrium. If transitioning toward greater gatekeeping: ε should increase toward 0.65+, narrowing the window for functional race-conscious remedies. If transitioning toward greater acceptance: ε decreases, narrow tailoring becomes more permissive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_sffa_doctrine_stability, empirical, 'Post-SFFA trajectory of narrow-tailoring doctrine (stable, hardening, or softening)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strict_scrutiny_tier__narrow_tailoring_mechanics, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(narrowtail_theater_t0, strict_scrutiny_tier__narrow_tailoring_mechanics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(narrowtail_theater_t15, strict_scrutiny_tier__narrow_tailoring_mechanics, theater_ratio, 15, 0.42).
narrative_ontology:measurement(narrowtail_theater_t30, strict_scrutiny_tier__narrow_tailoring_mechanics, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(narrowtail_extract_t0, strict_scrutiny_tier__narrow_tailoring_mechanics, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(narrowtail_extract_t15, strict_scrutiny_tier__narrow_tailoring_mechanics, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(narrowtail_extract_t30, strict_scrutiny_tier__narrow_tailoring_mechanics, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(narrowtail_suppress_t0, strict_scrutiny_tier__narrow_tailoring_mechanics, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(narrowtail_suppress_t15, strict_scrutiny_tier__narrow_tailoring_mechanics, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(narrowtail_suppress_t30, strict_scrutiny_tier__narrow_tailoring_mechanics, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strict_scrutiny_tier__narrow_tailoring_mechanics, enforcement_mechanism).
narrative_ontology:affects_constraint(strict_scrutiny_tier__narrow_tailoring_mechanics, strict_scrutiny_tier__compelling_interest_jurisprudence).
narrative_ontology:affects_constraint(strict_scrutiny_tier__narrow_tailoring_mechanics, strict_scrutiny_tier__fatal_in_fact_trajectory).
narrative_ontology:affects_constraint(strict_scrutiny_tier__narrow_tailoring_mechanics, strict_scrutiny_doctrinal_gatekeeping).
narrative_ontology:affects_constraint(strict_scrutiny_tier__narrow_tailoring_mechanics, race_conscious_affirmative_action_sustainability).

% DUAL FORMULATION NOTE:
% Narrow tailoring is decomposed from compelling interest and fatal-in-fact trajectory as a kernel reading that isolates where suppression is actually executed — in the means inquiry rather than at the interest-identification stage. The three readings share a kernel (strict scrutiny tier) but have different ε values reflecting different structural gatekeeping power. Narrow tailoring's ε=0.58 reflects moderate-high extraction through procedural gatekeeping; compelling interest's ε likely differs (lower if the interest gate is genuinely permissive); fatal-in-fact's ε differs (reflecting historical trajectory rather than current doctrine). All three are linked through network.affects_constraints for constraint-family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strict_scrutiny_tier__narrow_tailoring_mechanics, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
