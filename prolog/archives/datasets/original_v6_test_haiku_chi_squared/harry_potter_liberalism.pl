% ============================================================================
% CONSTRAINT STORY: harry_potter_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harry_potter_liberalism, []).

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
 *   constraint_id: harry_potter_liberalism
 *   human_readable: The Potterverse Liberalism Constraint
 *   domain: socio_political
 *
 * SUMMARY:
 *   The Potterverse Liberalism Constraint describes how the narrative
 *   framework of 1990s liberal humanism—tolerance, non-violence,
 *   institutional trust, dialogue with adversaries, rule of law—functioned as
 *   an effective coordination mechanism for the professional-managerial
 *   millennial cohort entering adulthood during the 'end of history' but has
 *   become experienced as an extractive trap by economically precarious
 *   cohorts and downstream generations facing material decline. The Harry
 *   Potter series (1997–2007) is the cultural exemplar: its core moral
 *   teaching is that love and non-violence defeat violence, that
 *   institutional due process and fair debate resolve conflicts, and that the
 *   greatest good is achieved through sacrifice of individual interest to
 *   collective procedure. This narrative provided genuine coordination
 *   benefits for beneficiaries—it enabled networked professional identity,
 *   cultural signaling, and institutional access. For those locked out of
 *   material advancement despite institutional compliance, however, the same
 *   framework functions as suppression: it demands patience, emotional labor,
 *   and continued faith in procedures while material conditions deteriorate.
 *   The constraint exhibits all six classification types depending on
 *   structural position, with a clear temporal trajectory from
 *   coordination-dominant (early period, ε≈0.28) to extraction-dominant
 *   (later period, ε≈0.52). The theater ratio's rise (0.38 → 0.61) reflects
 *   increasing awareness that the liberal ritual (civil debate, institutional
 *   reform, procedural due process) is performative—actions take decades,
 *   material stakes grow urgent, and the constraint becomes a mechanism for
 *   delay rather than resolution.
 *
 * KEY AGENTS:
 *   - Millennial Professional Class: Primary beneficiary (institutional/arbitrage) — captured degrees, professional networks, cultural capital through liberal institutional access during optimal conditions (2000–2008)
 *   - Economically Precarious Cohort (Post-2008): Primary victim (powerless/trapped) — cannot exit; faces student debt, housing unaffordability, wage decline while constrained by inherited liberal narrative demanding patience and faith in institutions
 *   - Marginalized Identity Groups: Secondary victim (powerless/constrained) — demands for tolerance and 'dialogue' defer material protections; liberal proceduralism slower than identity-based solidarity alternatives
 *   - Institutional Gatekeepers (Universities, Publishing, Professional Licensing): Beneficiary-adjacent (institutional/arbitrage) — maintain legitimacy and gatekeeping power through liberal proceduralism and meritocratic narrative
 *   - Left-Aligned Institutional Reformers: Organized actor (powerful/mobile) — see both coordination (institutional access for critique) and extraction (delayed redistribution); attempt to transmute liberalism into social democracy
 *   - Right-Aligned Institutional Conservatives: Organized actor (powerful/mobile) — use liberal proceduralism to slow leftward drift while extracting legitimacy through electoral democracy; alternative institutional networks (media, law) reduce dependence on traditional liberal institutions
 *   - Global South and Postcolonial Audiences: Victim-adjacent (institutional/constrained) — experience liberal universalism as mask for continued resource extraction and soft power dominance; constraint persists through cultural inertia rather than material coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harry_potter_liberalism, 0.52).
domain_priors:suppression_score(harry_potter_liberalism, 0.48).
domain_priors:theater_ratio(harry_potter_liberalism, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harry_potter_liberalism, extractiveness, 0.52).
narrative_ontology:constraint_metric(harry_potter_liberalism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(harry_potter_liberalism, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harry_potter_liberalism, tangled_rope).
narrative_ontology:human_readable(harry_potter_liberalism, "The Potterverse Liberalism Constraint").
narrative_ontology:topic_domain(harry_potter_liberalism, "socio_political").

domain_priors:requires_active_enforcement(harry_potter_liberalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harry_potter_liberalism, millennial_professional_class).
narrative_ontology:constraint_beneficiary(harry_potter_liberalism, institutional_gatekeepers).
narrative_ontology:constraint_victim(harry_potter_liberalism, economically_precarious_cohort).
narrative_ontology:constraint_victim(harry_potter_liberalism, marginalized_identity_groups).
narrative_ontology:constraint_victim(harry_potter_liberalism, downstream_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY PRECARIOUS COHORT (SNARE) — Cannot exit the constraint; structurally trapped. Inherits the narrative that tolerance, patience, and institutional faith are cardinal virtues while facing student debt, housing unaffordability, and declining wage expectations. The liberal framework (dialogue with Voldemort equivalents, procedural due process, civil debate) functions as extraction: it delays material redistribution while demanding emotional labor of reconciliation. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(harry_potter_liberalism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MILLENNIAL PROFESSIONAL CLASS & INSTITUTIONAL GATEKEEPERS (ROPE) — Primary beneficiaries (institutional power, arbitrage exit). Experience the liberal constraint as genuine coordination: tolerance and civil procedure enable the professional-managerial networks that granted them access. The framework works—they got the degrees, the careers, the cultural capital. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Negative effective extraction = net beneficiary. The constraint coordinates their class reproduction.
constraint_indexing:constraint_classification(harry_potter_liberalism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LEFT-ALIGNED INSTITUTIONAL REFORM (TANGLED ROPE) — Powerful actors (organized left within institutions, democratic socialism advocates, critical race theory scholars) see both coordination and extraction. The liberal framework enables intellectual labor and institutional critique (coordination benefit) but constrains material redistribution and rapid power transfer (extraction cost). They have partial mobility (can publish elsewhere, build alternative institutions) but depend on institutional access to scale. d≈0.48, f(d)≈0.58, σ=1.0 → χ≈0.30.
constraint_indexing:constraint_classification(harry_potter_liberalism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: RIGHT-ALIGNED INSTITUTIONAL CAPTURE (TANGLED ROPE) — Powerful actors (conservative legal movements, nationalist populism) also see mixed coordination and extraction. They use liberal proceduralism to slow leftward institutional drift (coordination benefit for conservative institutional maintenance) while extracting legitimacy through democratic processes. Have high mobility (alternative media, extrainstitutional networks) but benefit from formal institutional prestige. d≈0.45, f(d)≈0.53, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(harry_potter_liberalism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: POSTCOLONIAL & GLOBAL SOUTH (PITON) — The 1990s liberal framework (individual rights, institutional trust, dialogue) is experienced as a degraded residue of Cold War universalism. The constraint persists through cultural inertia and soft power (English-language media, university curricula, literary prestige) but no longer delivers coordination benefits in contexts of resource extraction, climate debt, or ongoing structural dominance. theater_ratio=0.61 reflects the performative character of 'global tolerance' narratives that mask material inequality. d≈0.70, f(d)≈1.08, σ=1.2 → χ≈0.38.
constraint_indexing:constraint_classification(harry_potter_liberalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — A civilizational-scale view treats liberal democracy, non-violence, and institutional trust as immutable features of sustainable social order—inherent to human flourishing. However, this constrains empirical analysis: the structural data (ε=0.52, suppression=0.48, theater=0.61) contradicts the mountain classification. The engine detects a false summit: what appears as natural law is actually a contingent institutional arrangement whose utility varies sharply by material position. accessibility_collapse=0.75, resistance=0.42 (below mountain gates), emerges_naturally=false.
constraint_indexing:constraint_classification(harry_potter_liberalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harry_potter_liberalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(harry_potter_liberalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(harry_potter_liberalism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(harry_potter_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(harry_potter_liberalism, TR),
    TR >= 0.70.

:- end_tests(harry_potter_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts material value from precarious cohorts (delays redistribution, justifies wage suppression, demands emotional labor of forgiveness and tolerance) while benefiting the professional class and institutional gatekeepers who successfully navigated liberal meritocratic institutions during optimal economic conditions (1990–2008). The extraction is not maximal because some genuine coordination benefits persist (civil discourse networks, intellectual access, institutional change is possible, albeit slow). Suppression (0.48): Moderate. The liberal framework suppresses alternative coordination substrates (direct action, class-based solidarity, non-violent civil disobedience that violates procedure, mutual aid that bypasses institutions). However, suppression is not total—alternative frameworks are visible (Black Lives Matter, climate activism, mutual aid networks) and increasingly deployed, indicating that the constraint's suppressive power is weakening. Theater ratio (0.61): Moderate-high and rising. The liberal performative character has become increasingly visible: institutional reform processes (diversity committees, DEI training, criminal justice reform initiatives) are perceived as theatrical—appearing to address material problems without material redistribution. The rise from 0.38 (1997) to 0.61 (2024) reflects Goodhart drift: civil discourse became an end in itself rather than a means to material change.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The millennial professional class experiences coordination (Rope)—the liberal framework delivered material benefits. The economically precarious cohort experiences extraction and entrapment (Snare)—the same framework demands patience while conditions deteriorate. Institutional reformers see a mixed hybrid (Tangled Rope) where liberal procedures enable critique but constrain redistribution speed. Conservative institutional actors also see Tangled Rope (procedures enable their defense of existing distributions). The postcolonial observer sees degraded inertia (Piton)—the constraint persists through soft power and cultural dominance, not functional coordination. The civilizational analytical observer risks a false mountain (naturalizing liberalism as inherent to human flourishing) but the structural metrics (ε=0.52, suppression=0.48, theater=0.61) contradict that classification. The perspectival gap is a diagnostic feature: it reveals that 'liberalism' is not a neutral, universal framework but a contingent institutional arrangement whose utility depends sharply on material position.
 *
 * DIRECTIONALITY LOGIC:
 *   Millennial professional class: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative effective extraction. The constraint coordinates their material advancement. Economically precarious cohort: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; structurally trapped with no exit. The framework demands compliance (patience, tolerance, procedural faith) while material conditions deteriorate. Marginalized identity groups: Victim + constrained → d≈0.78, f(d)≈1.13. High extraction. Demands for tolerance and dialogue defer material protection; constraints on direct action. Left-aligned reformers: Powerful + mobile → d≈0.48, f(d)≈0.58. Mixed. Institutional access enables critique but procedures constrain pace of redistribution. Mobility is real (can publish, organize) but scaling requires institutional legitimacy. Right-aligned conservatives: Powerful + mobile → d≈0.45, f(d)≈0.53. Mixed but different direction. Procedures slow their opponents; alternative networks reduce institutional dependence. Global South: Institutional + constrained → d≈0.70, f(d)≈1.08. Soft power extraction. Narrative prestige constrains alternative framings; institutional dependence (English-language scholarship, Western university prestige) reduces exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy between 'liberalism as pure coordination' and 'liberalism as pure extraction' by showing that both are true depending on material position and temporal period. The 1990s-2008 period is genuinely Rope-dominant for beneficiaries—the liberal framework coordinated professional identity, institutional access, and cultural capital. The 2008-2024 period shifts toward Snare-dominant for precarious cohorts—the same framework now functions as delayed redistribution and suppressed alternatives. No single type is 'correct.' The constraint IS a mixed hybrid (Tangled Rope) from most perspectives, with perspectival gaps revealing the material asymmetries it enforces. The mandatrophy would be mislabeled if resolved as 'liberalism is inherently extractive' (false for beneficiaries) or 'liberalism is inherently coordinative' (false for precarious cohorts). Instead, the correct reading is: liberalism functions as Rope for those positioned to benefit from meritocratic procedures and as Snare for those locked out. The theater rise (0.38 → 0.61) indicates the second reading is becoming perceptually dominant even among beneficiaries, creating potential for constraint transition toward Piton (degraded ritual) or Scaffold (temporary framework being replaced by post-liberal alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generational_material_threshold,
    'Is the collapse of liberal narrative support driven by genuine material precarity (structural) or by shifting aesthetic preferences among digital-native cohorts (cultural)?',
    'Comparison of liberal framework acceptance across class strata within generational cohorts; correlation between student debt, housing unaffordability, wage trajectories and reported confidence in institutional tolerance; cross-cultural replication in countries with different welfare regimes',
    'If structural: the constraint is fundamentally a Snare for precarious cohorts; no amount of narrative refinement resolves it without material redistribution. If cultural: alternative liberal framings (stakeholder capitalism, progressive taxation) could restore coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_material_threshold, empirical, 'Whether liberal framework collapse is material or cultural').

omega_variable(
    institutional_reform_feasibility,
    'Can the liberal procedural framework accommodate rapid material redistribution (student debt forgiveness, wealth caps, housing guarantees) without collapsing into authoritarian central planning or devolving into governance paralysis?',
    'Historical case studies of institutional reform under material pressure (post-WW2 social democracy, post-1989 central European transitions); simulation models of procedural democracy under rapid redistribution; comparative analysis of outcomes in jurisdictions that attempted both liberalism and rapid redistribution',
    'If feasible: Scaffold narrative (liberal procedures are tools for transition, not immutable constraints). If infeasible: Snare narrative (liberalism structurally prevents redistribution, trapping precarious cohorts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reform_feasibility, empirical, 'Whether liberal procedures can enable rapid material redistribution').

omega_variable(
    alternative_coordination_substrate,
    'Do post-liberal frameworks (strategic essentialism, intersectionalist coalition-building, mutual aid networks) provide comparable coordination benefits to the liberal tolerance narrative, or do they sacrifice scalability for coherence?',
    'Ethnographic analysis of non-liberal coordination mechanisms; measurement of coalition stability, resource-sharing effectiveness, and capacity to bridge scale (local → national → global); comparison of failure modes (liberal: co-optation and slowness; post-liberal: factional fragmentation)',
    'If post-liberal substrates scale: constraint can transition to Scaffold (temporary liberal coordination being replaced). If they don''t: constraint remains Tangled Rope or Snare depending on whether alternatives emerge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_substrate, conceptual, 'Whether post-liberal frameworks provide scalable coordination').

omega_variable(
    mirror_of_erised_temporal_dynamics,
    'Does generational turnover of dominant liberal narratives follow a predictable lifecycle (emergence, utility, degradation, collapse), or is the timing contingent on material shocks and institutional legitimacy crises?',
    'Historical analysis of previous dominant narratives (20th-century nationalism, Cold War anti-communism, post-colonial modernization myths); pattern detection of narrative lifecycle timescales; analysis of correlation between narrative persistence and material regime performance',
    'If predictable: the constraint exhibits irreversible temporal decay (Piton from all perspectives eventually). If contingent: the constraint''s type can shift with institutional reform or material redistribution (Tangled Rope with variable χ over time).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mirror_of_erised_temporal_dynamics, empirical, 'Whether liberal narrative collapse follows predictable lifecycle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harry_potter_liberalism, 1997, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hpl_tr_t0, harry_potter_liberalism, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hpl_tr_t5, harry_potter_liberalism, theater_ratio, 5, 0.5).
narrative_ontology:measurement(hpl_tr_t10, harry_potter_liberalism, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(hpl_be_t0, harry_potter_liberalism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hpl_be_t5, harry_potter_liberalism, base_extractiveness, 5, 0.39).
narrative_ontology:measurement(hpl_be_t10, harry_potter_liberalism, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harry_potter_liberalism, information_standard).
narrative_ontology:affects_constraint(harry_potter_liberalism, meritocratic_credential_inflation).
narrative_ontology:affects_constraint(harry_potter_liberalism, procedural_justice_delays).
narrative_ontology:affects_constraint(harry_potter_liberalism, cultural_soft_power_dominance).
narrative_ontology:affects_constraint(harry_potter_liberalism, millennial_precarity_paradox).

% DUAL FORMULATION NOTE:
% The HP Liberalism constraint decomposes from a higher-level 'end of history narratives' family. Upstream constraint: Cold War universalism (ε≈0.30, Mountain from Western institutional perspective; Piton from postcolonial perspective). Downstream constraints: specific institutional implementations (meritocratic credentialing, procedural delays, soft power). The constraint's ε increases over its interval (0.28 → 0.52) as material conditions diverge from narrative promises, representing degradation of its original coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(harry_potter_liberalism, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
