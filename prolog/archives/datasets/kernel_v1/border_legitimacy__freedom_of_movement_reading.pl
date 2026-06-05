% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Enforcement as Restriction on Freedom of Movement (Freedom-of-Movement Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   Under the freedom-of-movement reading, border enforcement is an
 *   extractive mechanism that restricts a fundamental human capacity —
 *   mobility across territory — without coherent justification beyond the
 *   preservation of privilege for incumbent citizens and state fiscal
 *   control. The reading treats borders as presumptively illegitimate unless
 *   justified by compelling necessity (life-threatening persecution,
 *   disaster). This constraint instantiates one coherent reading of the
 *   contested kernel 'border legitimacy' alongside two sibling readings: the
 *   sovereignty_reading (borders are legitimate expressions of territorial
 *   self-determination) and the humanitarian_obligation_reading (borders are
 *   justified for citizens but require humanitarian exceptions). Each reading
 *   produces a different constraint classification, victim set, and ε value.
 *   The freedom-of-movement reading produces the highest ε (0.68, Snare)
 *   because it casts the widest victim net: not only migrants but also
 *   displaced incumbent workers and the global poor are characterized as
 *   victims of border enforcement, which suppresses their capacity for
 *   self-movement and traps them in low-wage or resource-scarce zones.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victim (powerless/trapped) — structurally immobilized by legal prohibition and physical enforcement; no recognized alternatives
 *   - Displaced Workers / Incumbent Citizens: Secondary victim (moderate/constrained) — characterized as victims of wage suppression and nationalist ideology under this reading, though they may self-identify as beneficiaries
 *   - State Fiscal Authorities: Institutional beneficiary (institutional/arbitrage) — experience borders as coordination mechanism enabling welfare allocation and tax collection
 *   - Global Capital / Investor Class: Powerful mixed agent (powerful/mobile) — benefit from labor-cost arbitrage and selective mobility, but experience some constraint from nation-state regulation
 *   - Open Borders Coalition: Organized advocates (organized/mobile) — perceive borders as extractive but transformable through norm-change and regional frameworks
 *   - Analytical Observer (Sovereignty Perspective): Civilizational naturalizer (analytical/analytical) — risks treating sovereignty-reading as universal law rather than one competing reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.68).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.75).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Enforcement as Restriction on Freedom of Movement (Freedom-of-Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '0fe606de-d31c-4037-a583-1e84b05adf12').
narrative_ontology:cs_kernel_codification('0fe606de-d31c-4037-a583-1e84b05adf12', distributed).
narrative_ontology:cs_authority_grounding('0fe606de-d31c-4037-a583-1e84b05adf12', distributed).
narrative_ontology:cs_reading_relation('0fe606de-d31c-4037-a583-1e84b05adf12', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fe606de-d31c-4037-a583-1e84b05adf12', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('0fe606de-d31c-4037-a583-1e84b05adf12', foundational, freedom_of_movement_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_human_right, holdable).
narrative_ontology:cs_axiom_grounding('0fe606de-d31c-4037-a583-1e84b05adf12', freedom_of_movement_human_right, deontological).
narrative_ontology:cs_axiom('0fe606de-d31c-4037-a583-1e84b05adf12', foundational, borders_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(borders_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('0fe606de-d31c-4037-a583-1e84b05adf12', borders_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('0fe606de-d31c-4037-a583-1e84b05adf12', unrestricted_movement_baseline).
narrative_ontology:cs_drift_state('0fe606de-d31c-4037-a583-1e84b05adf12', contemporary_hardening_regimes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0fe606de-d31c-4037-a583-1e84b05adf12', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, incumbent_citizens).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_fiscal_authorities).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_excluded_noncitizens).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, global_poor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Structurally immobilized by border enforcement; no exit from the restriction itself. Faces maximum suppression: legal prohibition, physical barriers, deportation threat. Trapped geographically and legally. No alternative is recognized by the enforcing authority. Pure extraction from this agent's structural position — zero coordination benefit, maximum coercive overhead.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED WORKER / INCUMBENT CITIZEN (SNARE) — Under this reading, the worker experiences border restriction as extractive: it preserves wage suppression by limiting labor supply competition, maintains nationalist framing that obscures labor exploitation, and prevents them from exercising freedom of movement themselves. High suppression (constrained exit — cannot easily emigrate without state permission, visa requirements, skill gatekeeping). Extracted from through wage depression and nationalist ideology that identifies their interests with border enforcement rather than with migrant workers facing identical suppression.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE FISCAL AUTHORITY (ROPE) — Experiences border enforcement as coordination mechanism: it enables welfare-state resource allocation, labor-market regulation, and tax revenue collection by defining the beneficiary and tax-paying group. From this perspective, borders solve a collective action problem (how to fund redistributive institutions). The state has arbitrage exit options (can renegotiate borders, labor agreements, regional blocs). Beneficiary experiencing the constraint as enabling coordination, not extraction.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL CAPITAL / INVESTOR CLASS (TANGLED ROPE) — Experiences borders as mixed: coordination function enables territorial property rights and labor-cost arbitrage (access cheap labor in low-wage zones, restrict labor mobility in high-wage zones). Extractive function enables selective mobility for capital and high-skilled workers while restricting low-skilled workers. Powerful agents (corporations, wealthy individuals) can move across borders; borders restrict competition from below. Mobile exit options (can relocate operations, wealth, nationality). Both beneficiary and partial victim: benefits from labor-cost differential but constrained by nation-state regulation and worker organizing. Moderate extraction experienced because this agent has agency and exit options.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN BORDERS COALITION / ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized agents (migrant-rights groups, labor unions, some human-rights NGOs) perceive borders as an extractive constraint with a sunset: regional freedom-of-movement frameworks (EU model, though contested), labor-union cross-border organizing, and international human-rights norms are gradually delegitimizing restrictive border regimes. Coordination function: borders enable temporary regulation during transition. Extractive function: borders restrict movement now. Organized agents have mobile exit options (advocacy, coalition-building, institutional capture). See the constraint as transformable rather than immutable.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY PERSPECTIVE (MOUNTAIN) — From a civilizational/universal perspective, territorial sovereignty and border enforcement appear as immutable features of the international system: states are recognized units; they claim exclusive authority over territory; this is simply how the world is organized. However, this perspective naturalizes the sovereignty-reading, not the freedom-of-movement reading. The engine's false summit detector will flag this: the 'immutable' status of borders is a reading-dependent artifact, not a universal constant. This perspective instantiates the oracle gap — the analytical observer's native position (state-centric realism) cannot see that freedom of movement is an equally coherent reading of the kernel.
constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_legitimacy__freedom_of_movement_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Under the freedom-of-movement reading, borders extract substantial value from those excluded while redistributing it to incumbent citizens and state fiscal authorities. The extraction includes: (1) wage suppression for incumbent workers (borders limit labor supply competition, maintaining artificially high wages); (2) welfare-benefit gatekeeping (borders restrict access to redistributive programs, allowing higher per-capita redistribution to citizens); (3) global poverty traps (borders prevent billions from accessing high-wage zones, perpetuating low-income equilibrium). The extraction increases over the measurement interval (0.52→0.68) as border enforcement infrastructure intensifies and alternative migration pathways are progressively closed (family reunification restrictions, skill gatekeeping, visa hardening). Suppression (0.75): Very high. Border enforcement employs legal prohibition, physical barriers, deportation threats, and state violence. Exit options are structurally unavailable — excluded migrants cannot voluntarily choose to move without state permission. Incumbent citizens face constrained exit (visa requirements, skill-based selection, expensive relocation). The suppression requirement increases slightly (0.65→0.75) as enforcement machinery expands. Theater ratio (0.58): Moderate. Border rhetoric emphasizes security and sovereignty, which are partly performative (national security framing that obscures economic extraction, sovereignty claims that naturalize contingent institutional arrangements) and partly functional (borders do regulate population movement, however unjustly). The theater increases (0.48→0.58) as human-rights framing becomes more visible — states must justify borders on humanitarian grounds rather than naked exclusion, adding rhetorical layer without reducing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between the excluded migrant (Snare — pure extraction, zero exit) and the state fiscal authority (Rope — coordination mechanism for welfare allocation). The same infrastructure that traps migrants enables welfare coordination. This is a genuine structural ambiguity: borders do solve a collective-action problem (how to fund redistributive welfare) while also extracting from those excluded. A secondary gap lies between incumbent workers' objective structural position and their perceived interests. Under this reading, incumbent workers are victims of wage suppression (displaced by border scarcity) but benefit from welfare gatekeeping. The reading's analytical move is to reframe incumbent workers as victims of the same extraction mechanism that benefits from their exclusionary sentiment. The open borders coalition perceives this reading as achievable — they see a sunset through norm change and regional frameworks. The sovereignty perspective naturalizes borders as immutable (Mountain), but this is the false summit the engine detects: the appearance of immutability is contingent on accepting the sovereignty axiom.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from structural position relative to the extraction flow. Excluded migrants (d≈0.95): Trapped agents bearing maximum extraction → high f(d). Displaced incumbent workers (d≈0.70, not classic victim): Constrained agents partly benefiting from wage-suppression effect but also victimized by nationalist ideology that prevents alliance with migrant workers → moderate f(d). State fiscal authorities (d≈0.10): Beneficiary with arbitrage exit → negative f(d). Open borders coalition (d≈0.55): Organized agents with mobile options, perceiving transformation possible → moderate f(d). Global capital (d≈0.40): Powerful agents with arbitrage options, mixed position → low f(d). The analytical observer (d≈0.72): Canonical d for analytical position, perceives the constraint structurally but from outside beneficiary/victim positions. The perspectival gaps reflect real differences in structural position, not mere disagreement about values.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying the reading-dependent nature of border legitimacy classification. The question is not 'are borders extractive?' (they are under the freedom-of-movement reading, they are coordinative under the sovereignty reading, they are mixed under the humanitarian reading). The question is 'which reading of the kernel legitimacy question are we adopting?' Once the reading is chosen, the classification follows. The high ε (0.68) reflects this reading's specific framing: it counts incumbent workers as victims (wage suppression) and the global poor as victims (mobility exclusion), making borders appear maximally extractive. A different reading with different victim sets would produce a different ε. The mandatrophy dissolves when we recognize that each reading is internally consistent but the contest is political (which axioms should govern border policy), not empirical (how much borders extract).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    right_vs_capability_distinction,
    'Is ''freedom of movement as a human right'' a deontological entitlement (you have the right *in principle*) or an instrumental capability (you have the *power* to move)?',
    'Jurisprudential analysis of how freedom-of-movement claims are adjudicated in human-rights courts; correlation between rights declarations and actual mobility outcomes by class and nationality',
    'If deontological: borders are unjust regardless of whether they prevent actual movement (rights violation occurs at the level of principle). If instrumental: borders are unjust only if they prevent actual movement capacity (empirical question about who can and cannot move). Affects classification across all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(right_vs_capability_distinction, conceptual, 'Whether freedom of movement is a deontological right or instrumental capability').

omega_variable(
    reading_foreclosure_mutual_exclusion,
    'Does this reading''s core axiom (freedom of movement is a human right) logically foreclose the sovereignty_reading''s core axiom (states have legitimate border authority)?',
    'Formal logical analysis of the two axioms'' relationship within a single normative framework; examination of whether liberal-democratic theory can coherently hold both or requires choosing one',
    'If foreclosed: the two readings are in genuine conflict (one must be false). If coexistent: both readings remain live positions and the kernel contest is a genuine political dispute, not a logical error. Determines whether reading_relations edge is ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_mutual_exclusion, conceptual, 'Whether freedom-of-movement and sovereignty axioms logically exclude each other').

omega_variable(
    welfare_state_dependency_mechanism,
    'Does border restriction function primarily to preserve wage suppression for incumbent workers, or primarily to gate redistributive welfare benefits, or are both mechanisms equally operative?',
    'Labor economics analysis of wage suppression under restriction vs. open borders; welfare-state fiscal modeling of benefit sustainability under different migration scenarios; qualitative analysis of policy framing in incumbent-worker coalitions',
    'If wage suppression dominant: displaced workers are extractive victims of borders (high d → high chi). If benefit gatekeeping dominant: welfare beneficiaries are ambiguous (partly victim of restriction''s impact on fiscal base, partly beneficiary of exclusion). Affects whether perspective 2 classifies as snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_state_dependency_mechanism, empirical, 'Whether border restriction''s primary mechanism targets wages or welfare benefits').

omega_variable(
    global_poor_aggregated_harm,
    'How should the aggregated harm to billions globally excluded from high-wage zones be weighted against incumbent citizens'' labor-market and welfare interests?',
    'Utilitarian calculus of total welfare under open vs. restricted movement; empirical migration-impact studies; population-weighted analysis of who benefits and loses',
    'High weighting on global poor → borders appear highly extractive (ε→0.8+). High weighting on incumbent citizens → borders appear as legitimate coordination (ε→0.3). The weighting is axiomatically determined, not empirically resolvable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_poor_aggregated_harm, preference, 'How to aggregate harms across the global poor vs. incumbent citizens').

omega_variable(
    this_is_a_reading_not_universal_truth,
    'Is this constraint a description of an objective feature of border legitimacy, or a description of one coherent reading of a contested kernel?',
    'Comparative analysis of sibling readings'' internal coherence and structural consistency; examination of whether each reading survives scrutiny or self-destructs under its own axioms',
    'If objective feature: this reading is *the* correct classification of border legitimacy. If one reading among coherent alternatives: this reading is valid but not universal; sibling readings are equally structurally consistent. Affects how the compiled constraint is used in policy reasoning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_is_a_reading_not_universal_truth, conceptual, 'Whether this constraint describes objective border legitimacy or one reading of a contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blfm_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(blfm_tr_t10, border_legitimacy__freedom_of_movement_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement(blfm_tr_t20, border_legitimacy__freedom_of_movement_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(blfm_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(blfm_be_t10, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(blfm_be_t20, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(blfm_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(blfm_su_t10, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(blfm_su_t20, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% The contested kernel 'border legitimacy' decomposes into three structurally distinct constraints, each representing a coherent reading. This story (freedom-of-movement reading) produces ε=0.68 (Snare). The sovereignty_reading produces ε≈0.15-0.25 (Rope or Mountain depending on whether sovereignty is treated as justified or natural law). The humanitarian_obligation_reading produces ε≈0.40-0.50 (Tangled Rope — genuine humanitarian coordination function with embedded economic extraction). Each reading has different victim sets, different beneficiaries, and different classifications from identical institutional positions. The network links all three, enabling analysis of how the readings compete and influence each other. The freedom-of-movement reading influences the humanitarian reading (if freedom of movement is a right, the humanitarian carve-out is inadequate); the humanitarian reading influences the sovereignty reading (humanitarian obligations constrain what sovereignty permits). None forecloses the others within their respective frameworks, but the readings are genuinely incompatible — policy cannot simultaneously adopt all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
