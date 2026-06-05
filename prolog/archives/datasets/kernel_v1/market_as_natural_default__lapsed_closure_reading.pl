% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_closure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_closure_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: market_as_natural_default__lapsed_closure_reading
 *   human_readable: Market Naturalization as Sedimented Ideology (Lapsed Closure Reading)
 *   domain: political_economy/ideology_studies/institutional_theory
 *
 * SUMMARY:
 *   Market naturalization in the lapsed-closure reading is sedimented
 *   ideology: the historical construction of capitalist markets through state
 *   violence (enclosure, colonialism, slavery), state design (property law,
 *   currency regimes, subsidy structures), and intellectual closure
 *   (neoclassical economics gatekeeping) has been forgotten, leaving only the
 *   naturalized appearance of markets as inevitable law. The original
 *   beneficiaries—colonial powers, enclosing landlords, industrial
 *   capitalists—are dead; the institutions they built persist through inertia
 *   and epistemological privilege rather than through active maintenance.
 *   Victims have internalized the inevitability narrative (identity_locked
 *   exit) so thoroughly that alternatives are cognitively inaccessible. The
 *   constraint's power is sustained by theater: ritualized economic modeling,
 *   policy conferences, academic consensus, and media repetition that perform
 *   the science of necessity while actual generative mechanisms (state
 *   redesign, intellectual gatekeeping, violence against commons) remain
 *   invisible. Unlike active engineered closure, lapsed closure requires
 *   minimal ongoing enforcement cost—the ideology self-reproduces through
 *   educational institutions, cultural narratives, and the internalized
 *   frames of those born into the system. This reading instantiates one
 *   position in a contested kernel: the market-as-natural-default. Sibling
 *   readings (engineered_closure_reading, dual_operation_reading) dispute
 *   whether this sedimentation is genuinely lapsed or masks active
 *   institutional work.
 *
 * KEY AGENTS:
 *   - Dispossessed populations (powerless/identity_locked): Internalized naturalization frame; cannot conceptualize alternatives; bear extraction cost through constrained allocation options
 *   - Policy intellectuals (moderate/constrained): Trained in market-naturalist frameworks; career penalties for dissent; experience the constraint through epistemological gatekeeping
 *   - Capital accumulation beneficiaries (institutional/arbitrage): Benefit from market naturalization without requiring active enforcement; experience constraint as coordination mechanism
 *   - Left intellectual coalition (organized/mobile): Exited the naturalization frame; building counter-institutions; see the constraint as piton (performative maintenance)
 *   - Historical materialist analyst (analytical/analytical): Recognizes original construction through historical evidence; identifies sedimentation as extraction mechanism
 *   - Naturalized consensus view (analytical/analytical): Treats markets as natural law within its own epistemic frame; represents false-summit naturalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_closure_reading, 0.38).
domain_priors:suppression_score(market_as_natural_default__lapsed_closure_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__lapsed_closure_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_closure_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_closure_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_closure_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_closure_reading, piton).
narrative_ontology:human_readable(market_as_natural_default__lapsed_closure_reading, "Market Naturalization as Sedimented Ideology (Lapsed Closure Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_closure_reading, "political_economy/ideology_studies/institutional_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_closure_reading, '67389770-67bc-462b-a9aa-e972b51ee6a7').
narrative_ontology:cs_kernel_codification('67389770-67bc-462b-a9aa-e972b51ee6a7', distributed).
narrative_ontology:cs_authority_grounding('67389770-67bc-462b-a9aa-e972b51ee6a7', diffuse_epistemic).
narrative_ontology:cs_reading_relation('67389770-67bc-462b-a9aa-e972b51ee6a7', market_as_natural_default__engineered_closure_reading, coexists_with).
narrative_ontology:cs_reading_relation('67389770-67bc-462b-a9aa-e972b51ee6a7', market_as_natural_default__dual_operation_reading, coexists_with).
narrative_ontology:cs_axiom('67389770-67bc-462b-a9aa-e972b51ee6a7', foundational, market_naturalization_genuinely_sedimented).
narrative_ontology:cs_axiom_status(market_naturalization_genuinely_sedimented, holdable).
narrative_ontology:cs_axiom_grounding('67389770-67bc-462b-a9aa-e972b51ee6a7', market_naturalization_genuinely_sedimented, empirically_contingent).
narrative_ontology:cs_axiom('67389770-67bc-462b-a9aa-e972b51ee6a7', foundational, cognitive_internalization_is_primary_binding_mechanism).
narrative_ontology:cs_axiom_status(cognitive_internalization_is_primary_binding_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('67389770-67bc-462b-a9aa-e972b51ee6a7', cognitive_internalization_is_primary_binding_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('67389770-67bc-462b-a9aa-e972b51ee6a7', market_as_universal_coordination_mechanism).
narrative_ontology:cs_drift_state('67389770-67bc-462b-a9aa-e972b51ee6a7', contemporary_heterodox_revival, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('67389770-67bc-462b-a9aa-e972b51ee6a7', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_closure_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_closure_reading, capital_accumulation_beneficiaries).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_closure_reading, collective_alternative_imaginary).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISPOSSESSED (PITON) — Powerless agents trapped not by external force but by internalized naturalization: the market appears as inevitable law rather than contingent construction. The binding is cognitive — victims have internalized the frame that 'there is no alternative.' Structural mobility may exist (constrained exit options materially available) but identity fusion with the necessity narrative makes exit unthinkable. The constraint's primary mechanism is theater: the ritualized celebration of market inevitability through economics curricula, policy discourse, and media framing maintains the appearance while performing little coordination function. Maximum theater ratio because the function (if any) is purely ideological reproduction.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, piton,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: THE POLICY INTELLECTUAL (PITON) — Moderate power agents (economists, policy advisors) trained in frameworks that treat markets as natural baseline. Exit from this framing is materially constrained — career penalties for departing orthodoxy, funding conditioned on neoclassical assumptions, peer review gatekeeping by market-naturalists. Experiences the constraint as a real barrier (constrained exit) but one whose force has diminished relative to earlier periods. Theater is high: economic modeling rituals, formalized methodology, empirical validation ceremonies perform the science of necessity while actual generative mechanisms remain hidden. The constraint is degraded — alternatives (behavioral economics, institutional analysis, complexity theory) are now live options within the mainstream, suggesting the piton's performative function is waning.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL ACCUMULATION STRUCTURE (ROPE) — Institutional-level beneficiaries (finance, extractive industries, speculative capital) experience market naturalization as coordination mechanism: the appearance of inevitability reduces political contestation, enabling capital flows and rent extraction without requiring continuous legitimation work. This perspective sees the constraint as genuinely functional (rope) rather than performative — the sedimented ideology *does* coordinate capital concentration by foreclosing alternative allocation mechanisms. Low extraction cost because no overt coercion is required; the ideological closure does the work.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEFT INTELLECTUAL COALITION (PITON) — Organized agents (heterodox economists, activists, solidarity movements) who have exited the naturalization frame and can articulate alternatives. Mobile exit options reflect their structural capacity to build counter-institutions (cooperative networks, alternative economics programs, degrowth frameworks). See the constraint as piton: market naturalization persists through institutional inertia and epistemological privilege rather than through actual functional superiority or continuous enforcement. The theater is transparent to them — they can identify the ritualized performance (policy conference consensus, modeling assumption consensus) as maintenance work rather than discovery. Low ongoing extraction cost because no enforcement machinery prevents their organizing — the constraint's power rests on the cognitive capture of those still inside the frame.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HISTORICAL MATERIALIST ANALYST (SNARE) — From civilizational timescale, market naturalization appears as pure extraction: the original construction (by enclosure, colonialism, violent dispossession) is forgotten, leaving only the naturalized appearance. The analytical observer recognizes this as a snare precisely because the sedimented ideology is the extraction mechanism — victims cannot even conceptualize alternatives within the naturalized frame. Effective suppression (0.62) is high because cognitive internalization is the binding force. Base extractiveness (0.38) reflects that ongoing material coercion is minimal — the system's power rests entirely on epistemological monopoly. Theater ratio is maximal (0.81) because the entire institution is ideological reproduction.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: NATURALIZED CONSENSUS VIEW (MOUNTAIN) — The dominant institutional perspective that treats market organization as a natural law: given scarcity, rational actors, and exchange, markets emerge necessarily and optimally allocate resources. This perspective classifies market naturalization as mountain from within its own epistemic frame. However, structural data reveals this as a false summit: identifiable beneficiaries (capital accumulation beneficiaries) exist; the constraint requires epistemological closure (victims must internalize inevitability); and historical evidence shows contingent construction (enclosure, colonialism, state reregulation). The false-summit reading is not that markets don't function — they do — but that their apparent inevitability is ideological naturalization of historical construction, not immutable law.
constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_closure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_as_natural_default__lapsed_closure_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_as_natural_default__lapsed_closure_reading, TR),
    TR >= 0.70.

:- end_tests(market_as_natural_default__lapsed_closure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low, declining over interval. The lapsed-closure reading suggests that the constraint's extractive force has diminished over time as beneficiary consciousness faded and ideology became self-maintaining. At t=0 (1975), market naturalization was still consciously defended by beneficiaries (Friedman, Hayek, Chicago School actively promoting). By t=50 (2025), the ideology is so sedimented that conscious defense is unnecessary—the system appears inevitable even to many who reject capitalism in other registers. The decline in base_extractiveness reflects decreasing maintenance cost as ideology transitions from active doctrine to unconscious common sense. Suppression (0.62): Consistently high, declining slightly. The binding mechanism remains strong because alternatives are epistemically inaccessible to those inside the frame. Suppression declines over the interval as heterodox economics, cooperative movements, and degrowth frameworks build counter-institutions and make alternatives conceptually visible again. Theater ratio (0.81): Elevated and stable. Economic modeling, policy consensus, academic peer review, and media framing perform the science of market necessity with minimal actual coordination function. The theater increased sharply between t=0 and t=25 as neoclassical formalization intensified (rational expectations, general equilibrium theory, behavioral economics sophistication). Theater remains high at t=50 despite declining extractiveness because the performative apparatus persists even as its ideological necessity weakens—a classic piton signature (form without function, maintained through institutional inertia).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence between those inside and outside the naturalization frame. The dispossessed and policy intellectuals see the constraint as binding and inevitable (piton classifies as constraint from their perspective). The capital beneficiaries see coordination (rope—the market mechanism works efficiently). The organized exit coalition sees degraded theater (piton, with visible alternatives). The analytical observer recognizes extraction through historical reconstruction (snare if analyzing mechanism; mountain if trapped in the naturalized frame). The naturalized consensus view treats markets as natural law (false-summit mountain). No single classification is 'correct'—the perspectival gap reveals the constraint's structure: it appears as natural law to those who internalize the frame, as coordination to beneficiaries, as theater to those who've exited, and as extraction to historical analysts. This is precisely the signature of successful ideological naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural position relative to extraction flow. Dispossessed agents (victims with identity_locked exit) experience high d (0.85+) because they bear full extraction cost while their cognitive frame prevents exit conceptualization. Policy intellectuals (constrained exit) experience moderate d (0.60-0.70) because they face real career barriers but have structural mobility if they're willing to pay the cost. Capital beneficiaries (arbitrage exit) experience low d (0.15-0.25) because they benefit from the constraint and can exit costlessly (maintaining market dominance is their preferred exit anyway). Organized coalition (mobile exit) experiences low d (0.25-0.35) because they've already exited and maintain alternatives. The analytical observer's d is context-dependent: if analyzing from within the naturalization frame, d approaches beneficiary levels (0.20); if analyzing from materialist history, d approaches victim levels (0.85). The piton classification derives from high theater (0.81) rather than from high effective extraction—the formula χ = ε × f(d) × σ(S) produces moderate-to-low χ for most contexts despite high base ε because the beneficiaries' low d and the constraint's global scope (σ=1.2, amplifying χ) are offset by the fact that the primary binding mechanism is cognitive rather than coercive.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not apply here because extractiveness (0.38) is below the 0.70 threshold. The constraint resolves clearly as piton: high theater (0.81), moderate extractiveness (0.38), moderate suppression (0.62), and a claimed type (piton) that matches the measurement profile. The perspectival divergence is extreme—ranging from mountain (false summit) to snare (historical materialist) to rope (capital beneficiaries) to piton (organized coalition)—but this is expected for a successful ideological naturalization. The constraint does NOT exhibit the mandatrophy pattern (ambiguous between extraction and coordination at high levels) because the lapsed-closure reading gives a clear account: it is extraction dressed as inevitability, but the extractive function is fully delegated to ideology and epistemological closure. The theater ratio is high because the ideological reproduction work (academic consensus, policy ritualism, media repetition) IS the enforcement mechanism, not a cover for something else underneath.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sedimentation_vs_active_closure,
    'Is market naturalization genuinely a sedimented ideology (original construction forgotten, now self-maintaining through epistemological privilege) or is it actively enforced through ongoing institutional work?',
    'Historical institutional analysis: track state regulatory changes, subsidy flows, intellectual property enforcement, currency regime maintenance over 50-year periods. If enforcement machinery is visible and adaptable, constraint is engineered closure. If enforcement is invisible and beneficiaries claim passivity, constraint is lapsed closure.',
    'Sedimented (this reading): piton + low maintenance cost; exiting requires cognitive decolonization. Actively enforced (engineered reading): tangled_rope or snare; exiting requires political contestation. Classification outcome changes fundamentally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sedimentation_vs_active_closure, empirical, 'Sedimented ideology vs. active institutional enforcement').

omega_variable(
    identity_lock_depth_and_reversibility,
    'How deeply internalized is the market naturalization frame in populations exposed to it from early childhood through institutional socialization? Is the identity lock reversible through counter-framing or does it require generational replacement?',
    'Cognitive intervention studies (economics alternative curricula, activist resocialization, post-market community experiments); longitudinal tracking of frame shifts in populations exposed to heterdox economics early vs. late. Measure reversibility rates and time constants.',
    'If reversible in biographical time: identity_locked agents can exit through frame shift (piton reading confirmed). If requires generational replacement: binding is deeper than identity_locked captures (suggests snare classification is more accurate at individual level, even if social system shows piton dynamics).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_depth_and_reversibility, empirical, 'Reversibility and depth of market naturalization identity lock').

omega_variable(
    alternative_allocation_epistemic_accessibility,
    'From within the market naturalization frame, are alternative allocation mechanisms (cooperative ownership, participatory planning, gift economy, commons management) conceptually accessible or are they structurally unthinkable?',
    'Discourse analysis of mainstream economics curricula, policy documents, and media; measure frequency and valence of alternative mechanism discussion. Survey economists on whether alternatives are empirically testable or theoretically impossible. Identify the specific epistemic gate that renders alternatives invisible.',
    'If alternatives are empirically unthinkable: epistemological closure is total (snare reading suggested). If alternatives are conceptually available but dismissed as inefficient: closure is partial (piton reading confirmed — alternatives are visible but delegitimized). Distribution of responses across disciplines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_epistemic_accessibility, conceptual, 'Epistemic accessibility of alternative allocation mechanisms').

omega_variable(
    beneficiary_consciousness_and_intentionality,
    'Do capital accumulation beneficiaries actively maintain market naturalization through conscious ideological work, or do they benefit passively while the system maintains itself through sedimented ideology?',
    'Documentary evidence (corporate influence on economics departments, think-tank funding patterns, curriculum development); interviews with institutional beneficiaries about awareness of naturalization mechanism; historical tracking of ideological campaigns (neoliberal movement, Chicago School sponsorship) vs. passive benefit.',
    'If actively maintained: engineered closure reading applies (beneficiaries are agents in the mechanism). If passive benefit with institutional self-maintenance: lapsed closure reading applies (this story). If mixed: dual_operation reading applies (both mechanisms simultaneously).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_consciousness_and_intentionality, empirical, 'Active vs. passive beneficiary maintenance of market naturalization').

omega_variable(
    kernel_contest_empirical_signature,
    'What historical or contemporary evidence would distinguish the three kernel readings (lapsed_closure, engineered_closure, dual_operation) as the dominant mechanism?',
    'Comparative institutional analysis across regulatory regimes with varying beneficiary consciousness. Track ratio of state enforcement activity to passive sedimentation. Measure beneficiary intentionality through archival evidence and institutional behavior. Model the dynamics of ideological reproduction under different beneficiary consciousness conditions.',
    'Different readings imply different political intervention points: lapsed_closure → cognitive decolonization suffices; engineered_closure → institutional political contestation required; dual_operation → both cognitive and institutional work needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_empirical_signature, empirical, 'Empirical signatures distinguishing the three kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_closure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_lapsed_theater_t0, market_as_natural_default__lapsed_closure_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(market_lapsed_theater_t25, market_as_natural_default__lapsed_closure_reading, theater_ratio, 25, 0.81).
narrative_ontology:measurement(market_lapsed_theater_t50, market_as_natural_default__lapsed_closure_reading, theater_ratio, 50, 0.81).

% Extraction over time
narrative_ontology:measurement(market_lapsed_extract_t0, market_as_natural_default__lapsed_closure_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(market_lapsed_extract_t25, market_as_natural_default__lapsed_closure_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(market_lapsed_extract_t50, market_as_natural_default__lapsed_closure_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(market_lapsed_suppress_t0, market_as_natural_default__lapsed_closure_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(market_lapsed_suppress_t25, market_as_natural_default__lapsed_closure_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(market_lapsed_suppress_t50, market_as_natural_default__lapsed_closure_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_closure_reading, information_standard).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_closure_reading, market_as_natural_default__engineered_closure_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_closure_reading, market_as_natural_default__dual_operation_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_closure_reading, neoclassical_gatekeeping__economics_orthodoxy).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_closure_reading, enclosure_movement__historical_foundation).

% DUAL FORMULATION NOTE:
% Market naturalization is one kernel with three structurally distinct readings. This story (lapsed_closure_reading) treats the constraint as sedimented ideology with minimal ongoing enforcement cost. Sibling story engineered_closure_reading treats it as active institutional work. Sibling story dual_operation_reading integrates both mechanisms. All three are interpretations of the same empirical phenomenon (markets appearing inevitable and unquestionable). The three readings have different ε values, different beneficiary consciousness assumptions, and different political intervention points. They are linked via network.affects_constraints to show the kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
