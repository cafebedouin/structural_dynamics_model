% ============================================================================
% CONSTRAINT STORY: identity_prestige_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_identity_prestige_trap, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: identity_prestige_trap
 *   human_readable: Identity-Prestige Trap in Book Publishing
 *   domain: writing_psychology/creative_labor/professional_identity
 *
 * SUMMARY:
 *   The identity-prestige trap in book publishing creates a structural
 *   mismatch between writers' motivations (achieving the cultural identity of
 *   'author') and the actual fit between their work and book format. This
 *   constraint is a canonical example of identity_locked exit options:
 *   writers are structurally mobile (could write essays, blog posts, serial
 *   work with lower barriers and faster feedback) but functionally trapped by
 *   identity fusion — their self-concept as a 'serious writer' is constituted
 *   through the goal of publishing a book. The constraint exhibits the full
 *   interpersonal-to-institutional scaling pattern: the same identity-lock
 *   mechanism that operates in abusive relationships (identity fused with the
 *   relationship) operates here at the professional level (identity fused
 *   with the book-author role). Gwern's distinction between 'wanting to have
 *   published a book' (extrinsic, identity-driven) vs. 'wanting to write'
 *   (intrinsic, process-driven) is the diagnostic observable. The
 *   theater_ratio (0.68) reflects that much of the book production process is
 *   performative: the 18-24 month lag, the proposal ritual, the agent search,
 *   the editing process that often degrades serial-native work by forcing
 *   narrative arc onto exploratory material. The constraint is downstream of
 *   opportunity_cost_asymmetry (the rope-classified coordination mechanism
 *   that makes books more discoverable than essays) but adds its own
 *   extractive layer: the prestige asymmetry creates identity-lock that
 *   persists even when the opportunity cost differential narrows.
 *
 * KEY AGENTS:
 *   - Identity-Locked Writer: Primary victim (powerless/identity_locked) — professional identity constituted through book-author status; structurally mobile but functionally trapped; experiences maximum extraction through years of misaligned labor
 *   - Ambivalent Professional: Secondary victim (moderate/constrained) — recognizes prestige asymmetry but constrained by career incentives; experiences mixed coordination benefit and extractive overhead
 *   - Cultural Gatekeepers: Primary beneficiary (institutional/arbitrage) — publishers, prize committees, tenure committees; experience pure coordination; prestige asymmetry concentrates writers' labor into formats they control
 *   - Digital-Native Coalition: Organized agents (organized/mobile) — Substack writers, independent researchers, long-form bloggers building alternative prestige pathways with scaffold logic
 *   - Publishing Ritual: Institutional actor (institutional/arbitrage) — multi-year production process is substantially theatrical; persists through inertia and prestige signaling
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination value (archival stability, library systems) alongside asymmetric extraction (prestige-driven identity-lock)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(identity_prestige_trap, 0.48).
domain_priors:suppression_score(identity_prestige_trap, 0.62).
domain_priors:theater_ratio(identity_prestige_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(identity_prestige_trap, extractiveness, 0.48).
narrative_ontology:constraint_metric(identity_prestige_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(identity_prestige_trap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(identity_prestige_trap, tangled_rope).
narrative_ontology:human_readable(identity_prestige_trap, "Identity-Prestige Trap in Book Publishing").
narrative_ontology:topic_domain(identity_prestige_trap, "writing_psychology/creative_labor/professional_identity").

domain_priors:requires_active_enforcement(identity_prestige_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(identity_prestige_trap, cultural_gatekeepers).
narrative_ontology:constraint_beneficiary(identity_prestige_trap, publishing_industry).
narrative_ontology:constraint_beneficiary(identity_prestige_trap, literary_institutions).
narrative_ontology:constraint_victim(identity_prestige_trap, writers_misaligned_with_book_format).
narrative_ontology:constraint_victim(identity_prestige_trap, essay_native_writers).
narrative_ontology:constraint_victim(identity_prestige_trap, serial_form_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-LOCKED WRITER (SNARE) — Writer whose professional identity is constituted through the goal of 'being an author' (book-haver status) rather than through the practice of writing. Cannot exit the book project because abandoning it would mean abandoning the identity they've constructed around it. Structurally mobile (could write essays, blog posts, serial work) but functionally trapped by identity fusion. Experiences maximum extraction: years of misaligned labor, motivation/execution mismatch, project abandonment or completion followed by regret.
constraint_indexing:constraint_classification(identity_prestige_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: AMBIVALENT PROFESSIONAL (TANGLED ROPE) — Writer who recognizes the prestige asymmetry and feels the pull toward book projects but retains enough critical distance to see the mismatch. Constrained by career incentives (grants, tenure, speaking invitations favor book authors) but not identity-locked. Experiences mixed extraction: genuine coordination benefit (book format does solve some problems — archival stability, library acquisition, course adoption) alongside extractive overhead (forcing serial-native work into book structure, enduring multi-year publication lag, sacrificing iterative revision).
constraint_indexing:constraint_classification(identity_prestige_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CULTURAL GATEKEEPER (ROPE) — Publishers, literary prize committees, tenure committees, grant panels. Experience the constraint as pure coordination: the book format provides a legible unit for evaluation, a stable object for library acquisition, a clear signal of sustained effort. Net beneficiary — the prestige asymmetry concentrates writers' labor into the format gatekeepers control, and the identity-lock ensures writers continue pursuing book projects even when the format doesn't serve their work.
constraint_indexing:constraint_classification(identity_prestige_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL-NATIVE COALITION (SCAFFOLD) — Substack writers, independent researchers, long-form bloggers, serial publishers building alternative prestige pathways. See the book-prestige asymmetry as a temporary coordination problem with a sunset: as digital-native work accumulates citations, grants, and career outcomes, the book's monopoly on 'serious work' status erodes. Gwern's essays, Wait But Why's explanations, and ACX's research summaries demonstrate that long-form rigor doesn't require book packaging. Estimated sunset: 10-20 years for norms to shift in academia and literary culture.
constraint_indexing:constraint_classification(identity_prestige_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLISHING RITUAL (PITON) — The multi-year book production process (proposal, agent search, publisher acquisition, editing, production, marketing) is substantially theatrical for many contemporary nonfiction works. The 18-24 month production lag adds no value for time-sensitive topics; the editorial process often degrades serial-native work by forcing narrative arc onto exploratory material; the print run and distribution infrastructure are vestigial for works whose primary audience is digital. The ritual persists through institutional inertia and prestige signaling, not because it optimally serves knowledge transmission.
constraint_indexing:constraint_classification(identity_prestige_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the book format provides genuine coordination value (archival stability, library systems, citation infrastructure, course adoption) but also embeds significant extraction (prestige asymmetry that misaligns writer motivation with project fit, identity-lock that traps writers in unsuitable formats, opportunity cost of multi-year projects that could have been iterative serial work). The constraint is a hybrid: real coordination function + asymmetric extraction + active enforcement through prestige allocation.
constraint_indexing:constraint_classification(identity_prestige_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(identity_prestige_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(identity_prestige_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(identity_prestige_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(identity_prestige_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(identity_prestige_trap, TR),
    TR >= 0.70.

:- end_tests(identity_prestige_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The prestige asymmetry between books and essays creates identity-lock for writers whose self-concept depends on 'author' status, leading to years of misaligned labor on projects that don't fit their work. But extraction is not maximal — some writers are genuinely well-served by book format, and the coordination benefits (archival stability, library acquisition, course adoption) are real. The value reflects that the career asymmetry is partly extractive (identity-driven project selection) and partly coordinative (books do solve some problems essays don't). Suppression (0.62): Moderate-high. Significant barriers include internalized prestige hierarchy (identity-lock), career gatekeeping (grants and tenure favor book authors), and institutional inertia (libraries and courses are organized around books). But suppression is not total — digital-native alternatives are forming, and some writers successfully build careers through serial work. The suppression has both structural components (material career barriers) and internalized components (identity fusion with book-author role). Theater ratio (0.68): High. The multi-year book production process is substantially performative for many contemporary nonfiction works: the proposal ritual, agent search, 18-24 month production lag, and editorial process often add no value or actively degrade serial-native work. The theater has increased over the interval as digital distribution has made print infrastructure vestigial for many works.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the identity_locked exit option's diagnostic power. The identity-locked writer sees a snare (trapped by identity fusion, maximum extraction). The ambivalent professional sees tangled rope (mixed coordination and extraction, constrained but not identity-locked). Cultural gatekeepers see rope (pure coordination from their position as beneficiaries). The digital-native coalition sees scaffold (temporary problem with sunset as alternative prestige pathways form). The publishing ritual sees its own degradation as piton (performative process maintained by inertia). The analytical observer sees tangled rope (genuine coordination function + asymmetric extraction). The perspectival gap between the identity-locked writer's snare and the gatekeeper's rope is the constraint's core dynamic: what appears as natural career progression from the gatekeeper's position is experienced as identity-constituted entrapment from the writer's position. The gap is not a measurement error — it's the structure of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity-locked writer is a victim (bears cost of misaligned labor) with identity_locked exit options (structurally mobile but functionally trapped by identity fusion). The engine derives high d from victim status + identity_locked exit, producing high experienced extraction. The ambivalent professional is also a victim but with constrained exit (recognizes the mismatch, faces career barriers but not identity-lock), producing moderate experienced extraction. Cultural gatekeepers are beneficiaries with arbitrage exit (control prestige allocation, can shift between formats costlessly), producing low or negative experienced extraction. The digital-native coalition is organized agents with mobile exit (building alternative pathways, can choose between book and serial formats), producing low experienced extraction despite being partly victims of the prestige asymmetry. The piton classification for the publishing ritual derives from the theater gate (theater_ratio ≥ 0.70 at endpoint) rather than from high experienced extraction — the ritual is degraded, not extractive from the institution's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the book format provides genuine coordination value (archival stability, library systems, citation infrastructure, course adoption) while simultaneously embedding asymmetric extraction (prestige hierarchy that creates identity-lock, opportunity cost of multi-year projects, theatrical production process). The coordination function is real: books solve problems that essays don't (stable objects for library acquisition, clear units for tenure evaluation, archival permanence). The extraction is also real: the prestige asymmetry drives writers to pursue book projects for identity reasons rather than format-fit reasons, creating motivation/execution mismatch and years of misaligned labor. The constraint is not 'really' a rope (pure coordination) or 'really' a snare (pure extraction) — it is structurally a tangled rope, with the classification varying by perspective. The identity-locked writer's snare classification is their genuine structural experience; the gatekeeper's rope classification is their genuine structural experience; the analytical observer's tangled rope classification integrates both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_vs_structural_suppression,
    'What proportion of the measured suppression (0.62) is identity-based (internalized prestige hierarchy) vs. structural (career gatekeeping that materially rewards books over essays)?',
    'Longitudinal tracking of writers who exit book projects: does their career trajectory improve (suggesting structural barriers were overstated) or collapse (confirming material gatekeeping)? Survey data on writers'' private vs. public justifications for book projects.',
    'If suppression is primarily identity-based (>70%), the constraint''s effective suppression is higher than structural measures suggest — writers carry the lock with them even when structural barriers fall. If primarily structural (<40%), the digital-native coalition''s scaffold logic is premature — alternative prestige pathways haven''t actually formed yet.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_vs_structural_suppression, empirical, 'Proportion of suppression that is internalized vs. structural').

omega_variable(
    format_fit_threshold,
    'What proportion of writers pursuing book projects are actually well-served by book format vs. misaligned?',
    'Post-publication satisfaction surveys; comparison of stated pre-project motivations (''want to be an author'' vs. ''this work needs book form'') with post-project assessments; tracking of writers who abandon book projects and report relief vs. regret.',
    'If <30% are well-served, the constraint is primarily extractive (snare from more perspectives). If >60% are well-served, the constraint is primarily coordinative (rope from more perspectives), and the ''identity trap'' framing overstates the problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(format_fit_threshold, empirical, 'Proportion of book projects that genuinely fit the writer''s work').

omega_variable(
    alternative_prestige_trajectory,
    'Are digital-native prestige pathways (Substack subscriptions, citation counts for blog posts, grant funding for serial work) actually forming, or is the scaffold perspective aspirational?',
    'Longitudinal career outcome tracking: do writers who build audiences through serial digital work achieve tenure, grants, speaking invitations at rates comparable to book authors? Do hiring committees and grant panels actually credit long-form blog posts as equivalent to book chapters?',
    'If alternative pathways are forming, the scaffold sunset is real and the constraint''s extraction is declining. If not, the book-prestige monopoly is stable and the identity-lock will persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_prestige_trajectory, empirical, 'Whether alternative prestige pathways are structurally forming').

omega_variable(
    motivation_execution_mismatch_severity,
    'How severe is the motivation/execution mismatch for identity-locked writers? Does ''wanting to have published'' vs. ''wanting to write'' predict project abandonment, completion-with-regret, or quality degradation?',
    'Comparison of project completion rates and post-publication satisfaction for writers with extrinsic motivations (prestige, identity, ''should write a book'') vs. intrinsic motivations (format fit, iterative development, audience need). Tracking of abandoned book projects and writers'' retrospective assessments.',
    'If mismatch strongly predicts abandonment or regret, the identity-lock is a major extraction mechanism and the snare classification is warranted. If mismatch is weakly predictive, the ''trap'' framing overstates the harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(motivation_execution_mismatch_severity, empirical, 'Severity of motivation/execution mismatch as predictor of outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(identity_prestige_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipt_tr_t0, identity_prestige_trap, theater_ratio, 0, 0.5).
narrative_ontology:measurement(ipt_tr_t5, identity_prestige_trap, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ipt_tr_t10, identity_prestige_trap, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ipt_be_t0, identity_prestige_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ipt_be_t5, identity_prestige_trap, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ipt_be_t10, identity_prestige_trap, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(identity_prestige_trap, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of opportunity_cost_asymmetry (the rope-classified coordination mechanism that makes books more discoverable than essays). The upstream constraint has its own extractiveness value (low, reflecting genuine coordination benefit of book format for discoverability). This constraint adds an additional extractive layer: the prestige asymmetry creates identity-lock that persists even when the opportunity cost differential narrows. The two constraints are structurally distinct and should not be conflated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
