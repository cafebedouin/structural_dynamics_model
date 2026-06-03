% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefinition Excluding Violence (Contraction Reading)
 *   domain: legal_anthropology/commitment_systems/honor_codes
 *
 * SUMMARY:
 *   This constraint captures the specific reading that dueling became
 *   structurally UNTHINKABLE as honor itself was redefined to exclude
 *   violence as a legitimate response to insult. This is one of three
 *   possible framings of the historical decline of dueling in Europe (roughly
 *   1750-1900). The contraction reading claims that the honor concept
 *   underwent semantic narrowing: violence was conceptually expelled from the
 *   set of honor-preserving acts. Previously, a refusal to duel was
 *   dishonorable; afterward, dueling itself became dishonorable. This is not
 *   merely a cost-driven reduction in dueling frequency (the drop reading)
 *   nor an overdetermined collapse caused by external costs AND conceptual
 *   shift simultaneously (the composite reading). Rather, it is the claim
 *   that the legitimacy framework itself contracted: the conceptual space of
 *   what counts as honorable action shrank to exclude violence entirely. The
 *   constraint exhibits Tangled Rope structure: the state and commercial
 *   bourgeoisie benefit from eliminating elite violence (coordination
 *   function) while simultaneously extracting the aristocracy's traditional
 *   status markers (asymmetric extraction, suppressed alternatives). The
 *   aristocratic duelist is identity-locked: structurally mobile but
 *   perceptually trapped by a redefined honor system that now condemns what
 *   their internalized identity demands.
 *
 * KEY AGENTS:
 *   - State Apparatus (institutional/arbitrage): Primary beneficiary — monopolizes legitimate violence, eliminates rival status hierarchies, stabilizes contract enforcement. Defines new honor frame.
 *   - Commercial Bourgeoisie (institutional/arbitrage): Secondary beneficiary — benefits from predictability and elimination of elite violence disrupting trade; defines honor as creditworthiness and commercial reliability.
 *   - Aristocratic Duelist (powerless/identity_locked): Primary victim — structurally mobile but identity-fused with honor-through-combat; redefinition makes dueling unthinkable within their internalized frame.
 *   - Dueling Subculture (moderate/constrained): Secondary victim — practices are criminalized; continuation requires secrecy or emigration; constrained but not fully trapped.
 *   - Unreformed Nobility (powerful/constrained): Powerful but constrained — reject the redefinition, continue dueling, face prosecution; exit requires emigration or complete loss of privilege.
 *   - Honor Reform Movement (organized/mobile): Organized agents driving the redefinition through legal, cultural, and social campaigns; mobile and low-theater because actively building alternatives.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent state consolidation as inevitable social law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.38).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.65).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefinition Excluding Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "legal_anthropology/commitment_systems/honor_codes").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'ee1d88ff-90b5-4744-ba8d-b3cbf05ee377').
narrative_ontology:cs_kernel_codification('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', fixed_text).
narrative_ontology:cs_authority_grounding('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', extraction).
narrative_ontology:cs_interpretation_layer_present('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377').
narrative_ontology:cs_reading_relation('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', foundational, honor_excludes_violence_principle).
narrative_ontology:cs_axiom_status(honor_excludes_violence_principle, holdable).
narrative_ontology:cs_axiom_grounding('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', honor_excludes_violence_principle, deontological).
narrative_ontology:cs_axiom('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', secondary, legitimacy_frame_contraction_necessity).
narrative_ontology:cs_axiom_status(legitimacy_frame_contraction_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', legitimacy_frame_contraction_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', honor_as_violent_response_legitimacy).
narrative_ontology:cs_drift_state('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', post_enlightenment_reform_period, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ee1d88ff-90b5-4744-ba8d-b3cbf05ee377', '2026-02-26T14:30:00Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, commercial_bourgeoisie).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_honor_practitioners).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, dueling_subculture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARISTOCRATIC DUELIST (SNARE) — Structurally mobile (could refuse challenge, could emigrate), but identity-fused with honor-through-combat. The redefinition of honor itself makes exit unthinkable from within the duelist's identity frame. Dueling is no longer a legitimate response to insult; the duelist is now morally condemned for defending what was previously their sacred duty. Maximum extraction: the agent's identity is deemed dishonorable by the very honor system they internalized.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSITIONAL ARISTOCRAT/REFORMER (TANGLED ROPE) — Partially accepts the redefinition; sees new honor in rejecting dueling. Experiences mixed costs and benefits: loss of traditional status through old-honor markers, but gain in prestige through new-honor alignment with state and commercial interests. Constrained exit because abandoning dueling still carries social cost from unreformed peers, but the cost is surmountable through institutional advancement.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE AUTHORITY (ROPE) — Benefits from the redefining constraint: monopolizes legitimate violence, eliminates rival status hierarchies, stabilizes commercial contracts. Experiences this as pure coordination — defining what honor means is how the state mediates social cohesion. No exit cost; the state is the authority defining the new frame.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMERCIAL BOURGEOISIE (ROPE) — Benefits from eliminating unpredictable elite violence that disrupts contracts and trade. The redefinition of honor to exclude violence is experienced as coordination: reputational honor (creditworthiness, contract reliability, business acumen) replaces violent honor. No extraction experienced because the beneficiary is defining the new legitimacy frame.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: OLD NOBILITY/UNREFORMED (TANGLED ROPE) — Powerful agents who reject the redefinition and continue dueling. Experience the constraint as extraction (loses legitimacy, faces state prosecution) but also as coordination (must internally settle disputes with fellow unreformed nobles). Constrained exit because full rejection requires emigration or loss of all aristocratic privilege, but partial accommodation (dueling in secret, reduced frequency) is possible.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HONOR REFORM MOVEMENT / ORGANIZED REFORMERS (SCAFFOLD) — Sees the constraint as a temporary coordination problem with a sunset: old honor must be replaced with new honor (cultural campaigns, legal enforcement, social incentives for non-dueling). Mobile exit because reformers can shift strategies if redefinition fails; low theater because the movement is actively building alternative legitimacy structures rather than merely performing compliance.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: EUROPEAN HONOR CODE / CIVILIZATIONAL VIEW (PITON) — From a long-term continental view, honor codes persist through theatrical maintenance long after their functional content decays. The redefinition persists as a formal commitment decades after dueling has actually ceased, maintained by institutional inertia and legal threat rather than active enforcement. High theater because the legitimacy claim ('honor now means X') is preserved formally while the actual practice (dueling) has been abandoned through external costs and generational drift, not through conceptual conversion.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks viewing the honor-violence redefinition as an inevitable law of social development: all sufficiently complex societies must eventually exclude elite violence and define honor through non-violent means. This perspective naturalizes what is actually a historically contingent rewriting of legitimacy. The engine's false-summit detector identifies this as false naturalization — the constraint has identifiable beneficiaries (state, bourgeoisie) and is actively enforced, making it contingent rather than necessary.
constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(honor_violence_legitimacy__contraction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, TR),
    TR >= 0.70.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts aristocratic status and autonomy in favor of state and bourgeois authority. However, the extraction is not maximized because it offers something in return — new honor (respectability, commercial standing, institutional advancement) replaces old honor (dueling prestige). The asymmetry is real but not total. Suppression (0.65): Moderate-high. The alternatives to accepting the redefinition are substantially suppressed: dueling is criminalized; duelists face prosecution, social ostracism, and loss of inheritance; emigration is costly; secret continuation is risky. But suppression is not maximal (0.95) because some agents can and do refuse the redefinition and accept the legal consequences. Theater ratio (0.58): Moderate. The legitimacy claim ('honor now excludes violence') requires active enforcement and cultural campaign; it is not merely performative. But the theater increases over time (0.35 → 0.58) as the genuine enforcement costs decline and the claim becomes institutional inertia. By generation 2-3, the constraint persists partly through theater (formal commitment to new honor) and partly through internalized acceptance (duelists genuinely rare).
 *
 * PERSPECTIVAL GAP:
 *   The contraction reading produces maximal perspectival divergence because it claims the legitimacy FRAME itself changed. The beneficiary sees coordination (new honor is rational and desirable). The identity-locked duelist sees snare (dueling is now forbidden by the very honor system they internalized). The unreformed nobility see tangled rope (powerful but constrained by prosecution). The reform movement sees scaffold (temporary problem with a planned sunset as new honor becomes generational norm). The piton perspective reveals institutional degradation (by generation 2-3, the 'new honor' is maintained through legal threat and formality rather than genuine commitment). The mountain perspective risks false naturalization (developing societies inevitably move from violent to non-violent honor concepts). The perspectival gap reveals the constraint's structure: what looks like coordination to the state looks like extraction to the duelist, and what looks like necessary progress to the analytical observer looks like institutional coercion to the identity-locked agent.
 *
 * DIRECTIONALITY LOGIC:
 *   The contraction reading's directionality dynamics turn on which agent you measure from. The state and bourgeoisie are beneficiaries with arbitrage options: they can define honor however they prefer (d ≈ 0.05-0.15, low f(d), effectively negative χ). The unreformed nobility are powerful but constrained by prosecution risk (d ≈ 0.65, moderate f(d)). The aristocratic duelist is powerless and identity-locked (d ≈ 0.88, high f(d)) — the highest experienced extraction. The identity-lock is the critical structural feature: the duelist is not trapped (they could refuse the challenge, emigrate, or secede), but their internalized honor frame makes exit unthinkable. This is a perspectival trap, not a material one. The scope modifier σ(S) = 1.0 for national context; the constraint's reach is state-level authority, not local or universal.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading resolves mandatrophy by specifying the kernel's content change: the honor concept contracted (violence was removed from the legitimate set). This reading can coexist with the drop_reading (dueling remained legitimate but became rare) only if the two readings occupy different parties' frameworks — some reformers genuinely believe honor excludes violence (contraction), while some unreformed duelists believe dueling remains honorable despite legal prohibition (drop). The oracle gap (Theorem 4) instantiates here: the state's redefinition creates a new empirical fact (the legal and social prohibition), but this does not settle whether the conceptual change is real (identity-locked agents genuinely internalized new honor) or performative (they comply under coercion while rejecting the redefinition). The contraction reading requires that internalization succeeded at some level; the drop reading denies this. This is an irreducible ambiguity routed to the conceptual omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_conversion_vs_coercion,
    'Does the redefinition succeed because aristocrats genuinely internalize new honor values, or does it succeed because state coercion makes old-honor response impossible regardless of internalization?',
    'Historical analysis of generational transition: measure rates of voluntary acceptance (letters, memoirs, reform advocacy) vs. compliance under threat (prosecutions, social exclusion, inheritance penalties). Compare jurisdictions with strong state enforcement vs. weak enforcement.',
    'If conversion: constraint is a rope (genuine coordination via shifted identity). If coercion: constraint is snare or tangled_rope (extraction sustained by suppression). The classification hinges on which mechanism drives the redefinition''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_conversion_vs_coercion, empirical, 'Whether honor redefinition succeeds through identity conversion or state coercion').

omega_variable(
    reading_ambiguity_contraction_vs_drop,
    'Does this constraint represent a genuine semantic contraction of the honor concept (dueling becomes logically incoherent with honor), or does it represent merely a cost-driven drop in dueling frequency with honor semantics unchanged?',
    'Textual analysis of period legal and philosophical sources: do they argue dueling is dishonoring (semantic contraction) or merely that dueling is imprudent/illegal (pragmatic drop)? Track whether duelists pre-1800 and post-1850 actually disagree on what honor IS or merely on whether dueling is a permissible honor response.',
    'If contraction: this reading is correct — the kernel''s content changed. If drop: the drop_reading is the better frame — dueling remained legitimate but became rare. This omega flags the irreducible ambiguity between the two sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_ambiguity_contraction_vs_drop, conceptual, 'Whether the redefinition is semantic contraction or pragmatic cost-driven drop in frequency').

omega_variable(
    geographic_and_jurisdictional_variance,
    'Did the honor redefinition occur uniformly across Europe, or did it fragment into regional and class-specific honor codes (nobility vs bourgeoisie, France vs Germany vs Russia)?',
    'Comparative legal history: track dueling laws, honor codes, and honor-crime prosecutions across jurisdictions 1800-1900. Identify whether a unified ''new honor'' emerged or whether multiple honor frameworks coexisted.',
    'If unified: the contraction reading describes a single kernel change across the European commitment system. If fragmented: the kernel itself may have split into multiple competing kernels rather than contracting to a new definition. This affects whether a single reading can characterize the change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_and_jurisdictional_variance, empirical, 'Geographic and class variance in honor redefinition across Europe').

omega_variable(
    false_summit_natural_law_risk,
    'Is the analytical observer''s mountain classification identifying a true natural law of social development (all complex societies eventually define honor non-violently), or is this a false naturalization of historically contingent state power consolidation?',
    'Comparative analysis: examine non-European honor systems and elite violence patterns in societies that did NOT experience the European state-centered redefinition. Identify whether the honor-violence transition is universal or region-specific.',
    'If universal natural law: mountain classification holds. If contingent: the constraint is a false summit — identifiable beneficiaries and coercive enforcement reveal it as contingent institutional change, not natural law. The engine''s FSM detector will flag this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'Whether honor-violence redefinition is universal natural law or contingent institutional change').

omega_variable(
    aristocratic_internalization_threshold,
    'What fraction of aristocrats must genuinely accept the new honor definition (rather than merely comply) for the constraint to be classified as successfully internalized rope rather than as sustained snare?',
    'Generational data: measure acceptance rates by birth cohort. Track voluntary participation in honor-reform institutions (honor courts, reputation bodies, professional societies), voluntary non-dueling among non-prosecuted duelists, and positive statements about new honor in private correspondence.',
    'If threshold ≥ 70% acceptance by generation 2: constraint is rope. If < 50% acceptance but externally enforced: constraint is snare. Threshold value determines whether the contraction reading''s claim about ''redefining honor'' has actually occurred or merely stated as policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aristocratic_internalization_threshold, empirical, 'Threshold for genuine aristocratic internalization of honor redefinition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_contr_theater_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(honor_contr_theater_t25, honor_violence_legitimacy__contraction_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(honor_contr_theater_t50, honor_violence_legitimacy__contraction_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(honor_contr_extract_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(honor_contr_extract_t25, honor_violence_legitimacy__contraction_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(honor_contr_extract_t50, honor_violence_legitimacy__contraction_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(honor_contr_supp_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(honor_contr_supp_t25, honor_violence_legitimacy__contraction_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(honor_contr_supp_t50, honor_violence_legitimacy__contraction_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, aristocratic_identity_and_legal_modernization).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the honor_violence_legitimacy kernel. Three distinct stories decompose the historical process: drop_reading (ε≈0.22, Rope — external costs alone), contraction_reading (ε≈0.38, Tangled Rope — conceptual redefinition), composite_reading (ε≈0.55, Tangled Rope — both mechanisms). Each story has its own ε, beneficiary/victim structure, and perspectives. The network links them as alternative framings of the same historical phenomenon, not as cumulative accounts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, institutional, 0.12).
constraint_indexing:directionality_override(honor_violence_legitimacy__contraction_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
