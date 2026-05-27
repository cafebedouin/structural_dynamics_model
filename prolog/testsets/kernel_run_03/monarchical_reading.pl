% ============================================================================
% CONSTRAINT STORY: monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monarchical_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monarchical_reading
 *   human_readable: Monarchical Authority—Hereditary Legitimacy and Divine Ordination
 *   domain: political_philosophy/constitutional_theory/legitimacy
 *
 * SUMMARY:
 *   The monarchical reading of sovereign legitimacy grounds authority in
 *   hereditary succession and divine ordination. A monarch inherits the
 *   throne through bloodline and claims legitimacy through theological
 *   sanction — the idea that the ruler is appointed by or representing divine
 *   will and is therefore not accountable to the governed. This constraint is
 *   a reading of the contested kernel 'sovereign_legitimacy,' distinct from
 *   republican and constitutional-hybrid readings. The monarchical reading
 *   frames succession as immutable and natural, rendering the
 *   non-removability of the monarch as an inherent feature of legitimate
 *   governance. From different structural positions, this constraint appears
 *   as snare (to excluded commoners with no exit), rope (to the dynastic
 *   beneficiary experiencing coordination), tangled rope (to nobility
 *   experiencing both stability and obligation), and snare again (to the
 *   analytical observer noting the absence of non-violent removal
 *   mechanisms).
 *
 * KEY AGENTS:
 *   - Dynastic Lineage: Primary beneficiary (institutional/arbitrage) — captures sole authority, succession guarantee, resource concentration
 *   - Established Clergy: Secondary beneficiary (institutional/arbitrage) — maintains monopoly on legitimation narrative, interprets divine will selectively
 *   - Hereditary Nobility: Partial beneficiary/partial constrained (powerful/constrained) — gains predictable hierarchy and patronage but locked into obligation and exclusion from alternative power sources
 *   - Excluded Commoners: Primary victim (powerless/trapped) — barred from political participation by birth, no mechanism for appeal or collective voice
 *   - Deposed Rival Claimants: Secondary victim (moderate/constrained) — permanently excluded by succession rules despite once holding dynasty status; high exit cost
 *   - Analytical Observer: Civilizational view (analytical/analytical) — notes the absence of non-violent removal mechanisms; identifies non-accountability as the core extractive structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monarchical_reading, 0.68).
domain_priors:suppression_score(monarchical_reading, 0.72).
domain_priors:theater_ratio(monarchical_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monarchical_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(monarchical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(monarchical_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monarchical_reading, snare).
narrative_ontology:human_readable(monarchical_reading, "Monarchical Authority—Hereditary Legitimacy and Divine Ordination").
narrative_ontology:topic_domain(monarchical_reading, "political_philosophy/constitutional_theory/legitimacy").

domain_priors:requires_active_enforcement(monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monarchical_reading, 'ea70e6b2-48d6-4f68-a526-5ffeb299bb71').
narrative_ontology:cs_created_at('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', '').
narrative_ontology:cs_kernel_codification('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', fixed_text).
narrative_ontology:cs_authority_grounding('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', lineage).
narrative_ontology:cs_interpretation_layer_present('ea70e6b2-48d6-4f68-a526-5ffeb299bb71').
narrative_ontology:cs_kernel_id(monarchical_reading, sovereign_legitimacy).
narrative_ontology:cs_reading_relation('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', foundational, hereditary_succession_legitimacy).
narrative_ontology:cs_axiom_status(hereditary_succession_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', hereditary_succession_legitimacy, conventional).
narrative_ontology:cs_axiom('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', foundational, divine_ordination_accountability_exception).
narrative_ontology:cs_axiom_status(divine_ordination_accountability_exception, holdable).
narrative_ontology:cs_axiom_grounding('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', divine_ordination_accountability_exception, theological).
narrative_ontology:cs_reference_frame('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', divinely_ordained_hereditary_succession).
narrative_ontology:cs_drift_state('ea70e6b2-48d6-4f68-a526-5ffeb299bb71', modern_electoral_normativity_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monarchical_reading, dynastic_lineage).
narrative_ontology:constraint_beneficiary(monarchical_reading, established_clergy).
narrative_ontology:constraint_beneficiary(monarchical_reading, hereditary_nobility).
narrative_ontology:constraint_victim(monarchical_reading, excluded_commoners).
narrative_ontology:constraint_victim(monarchical_reading, deposed_rival_claimants).
narrative_ontology:constraint_victim(monarchical_reading, electoral_agency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED COMMONER (SNARE) — Structurally barred from political participation by birth. No mechanism for appeal, succession, or voice in governance. Suppression is total and inherited across generations. Extraction runs toward the dynastic beneficiary; trapped agents bear the cost of non-accountability and forced deference. Minimal coordination function from this agent's perspective — the constraint exists to extract obedience, not to solve a collective problem.
constraint_indexing:constraint_classification(monarchical_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEPOSED RIVAL CLAIMANT (SNARE) — Once within the beneficiary structure but permanently excluded by succession rules. High cost to exit (loss of dynasty status, exile, death). Extraction is severe because this agent could theoretically claim the throne but is prevented by force and law. Experiences the constraint as pure coercion, not coordination.
constraint_indexing:constraint_classification(monarchical_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DYNASTIC LINEAGE (ROPE) — Primary beneficiary. Perceives the constraint as pure coordination: the orderly transmission of authority, the stability of succession, the ritual recognition that legitimates rule. The monarch experiences the inherited system as solving the coordination problem of governance succession. Arbitrage exit option reflects the ability to negotiate terms, delegate functions, or work with established nobility. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(monarchical_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED CLERGY (ROPE) — Secondary beneficiary. Perceives the constraint as coordination of political and spiritual authority. The clergy's role is to sanctify the monarch, rendering the constraint a coordinated partnership. Arbitrage exit reflects the clergy's capacity to negotiate terms of religious sanction, interpret divine will selectively, and participate in succession rituals. Benefits from monopoly on legitimacy narrative.
constraint_indexing:constraint_classification(monarchical_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HEREDITARY NOBILITY (TANGLED ROPE) — Partial beneficiary with significant constraint. Benefits from stable succession and predictable hierarchy but is also locked into roles, obligations to the crown, and exclusion from alternative power sources. Exit options are constrained — breaking with the monarchy risks status and patronage. Experiences both coordination (governance structure stability) and extraction (obligation to obey, resource extraction for royal projects). Genuine hybrid structure.
constraint_indexing:constraint_classification(monarchical_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LEGITIMACY ANALYSIS (SNARE) — From a civilizational perspective examining the constraint's durability, the core extractive mechanism is the non-removability of the monarch. Unlike elected authority that can be voted out, hereditary monarchy entrenches power through bloodline. No collective mechanism exists for removing an incompetent, cruel, or unfit monarch without violent revolution. The divine ordination framing legitimizes this non-accountability as natural law, making it appear unchangeable. The analytical observer sees the constraint as a sophisticated snare that uses theological language to prevent exit mechanisms.
constraint_indexing:constraint_classification(monarchical_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monarchical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monarchical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monarchical_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The monarch captures absolute authority, resource flows, and succession guarantees through hereditary mechanism. The measurement trajectory shows slight accumulation (0.55 → 0.68) as the system matures and bureaucratic apparatus develops to enforce succession, suggesting rent-seeking layering onto the core coordination function. Suppression (0.72): High. Multiple structural barriers prevent exit: legal prohibition on political participation for commoners, theological framing that renders opposition as blasphemy or disorder, resource concentration that denies alternative power bases, and the ultimate threat of force. These barriers are inherited across generations. Theater ratio (0.55): Moderate. The constraint relies on substantial performative activity: coronation rituals, religious sanction ceremonies, oaths of fealty, heraldic display. However, the core coordination problem (succession order) and the core extraction mechanism (non-removability) are functionally real, not purely theatrical. The theater supports but does not constitute the constraint. As the theater ratio rises slightly over the interval (0.48 → 0.55), it suggests that as direct coercive mechanisms weaken (less reliance on visible force), the ritual performance of legitimacy intensifies to maintain compliance—a Piton-like degradation signal absent from the claimed type because extractiveness remains high and suppression remains structural.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is the gap between the beneficiary's experience (coordination and stability) and the powerless agent's experience (pure coercion). The dynastic lineage perceives rope—the constraint solves the succession coordination problem. The excluded commoner perceives snare—the constraint extracts obedience with no reciprocal benefit or exit mechanism. The analytical observer perceives snare with the additional diagnosis that the non-accountability structure is the constraint's primary feature and is legitimized through theological naturalization. The critical gap is that the beneficiary's rope perspective appears to solve a real coordination problem (succession order), which might justify classifying the constraint as tangled_rope (mixed coordination and extraction). However, this justification only holds if the hereditary succession rule is the *only* feasible mechanism for solving the succession problem. If succession could be ordered through other mechanisms (election, lottery, merit-based selection, constitutional procedure), then the hereditary rule is chosen precisely for its extractive benefit to the dynastic lineage, converting it from tangled_rope to snare. The measurement trajectory and omega variables are designed to test this hypothesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from the base extractiveness (ε=0.68), the agent's structural position relative to the constraint (beneficiary or victim, exit capacity), and scope modifiers. The dynastic beneficiary with arbitrage exit options experiences negative effective extraction (the constraint subsidizes them); their d value is very low. The excluded commoner with no exit experiences maximum effective extraction; their d value approaches 1.0, driving f(d) to its maximum. The analytical observer at global scope experiences the constraint through the civilizational lens of non-accountability mechanisms; their perspective reveals that the constraint's primary function is prevention of exit, making extraction appear as the core purpose rather than a side effect. The hereditary nobility's constraint-constrained position reflects that they have some agency (can negotiate terms with the crown, participate in governance) but cannot exit the system without sacrificing status and patronage. This produces the moderate-to-high extraction they experience relative to the dynamics of power among institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel frame: the 'truth' of whether monarchical authority is legitimate is not a matter of constraint classification but a matter of which reading of the sovereign_legitimacy kernel one adopts. The monarchical reading makes hereditary succession and divine ordination the authority ground; the republican reading makes popular consent the authority ground. Classification (snare vs rope vs mountain) follows from the axioms, not the reverse. From the monarchical reading's own perspective, the constraint is snare (extraction via non-removability). This is not a failure of classification but a structural feature: the reading that legitimizes non-removability as the source of authority is the same reading that produces snare classification from the powerless agent's perspective. The mandatrophy is dissolved by recognizing that each reading produces its own constraint story with its own ε and its own perspectival profile. The engine's job is to classify each reading cleanly, not to adjudicate which reading is 'correct.' The false summit detection system (FSM) would reclassify the monarchical reading if it were presented as a mountain (natural law of governance) despite clear beneficiary/victim structure—and indeed, defenders of monarchical authority often present it as a natural law grounded in divine order. The FSM detection and omega variables create a diagnostic flag for that rhetorical move.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_versus_contingent_tradition,
    'Is the monarch''s legitimacy derived from genuine divine ordination or from contingent tradition codified as divine?',
    'Theological analysis of claims vs historical emergence; comparison of different monarchical systems'' founding myths; examination of how clergy interpret divine will selectively to justify particular succession rules',
    'If genuinely divine: the constraint approaches mountain status (immutable natural/supernatural law). If contingent tradition: the constraint is purely constructed extraction, confirming snare classification and revealing false-summit potential if framed as natural law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_sanction_versus_contingent_tradition, conceptual, 'Whether divine sanction grounds legitimacy or naturalizes contingent tradition').

omega_variable(
    succession_stability_versus_coercive_lock,
    'Does the hereditary succession rule solve a genuine coordination problem (stable governance succession) or primarily serve to lock in dynastic extraction?',
    'Comparative institutional analysis: measure governance stability under hereditary vs elected systems; examine whether stability persists when succession rules are made revisable; track frequency and severity of conflicts within hereditary systems over succession disputes',
    'If genuine coordination problem: tangled_rope classification more justified from beneficiary perspective; suggests true mixed function. If primarily coercive lock: confirms snare classification; coordination narrative is cover story for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_versus_coercive_lock, empirical, 'Whether hereditary succession stabilizes governance or locks in extraction').

omega_variable(
    exit_mechanism_availability,
    'Do commoners or excluded claimants possess any actual mechanism (short of revolution) to exit the constraint or alter the succession rule?',
    'Legal analysis of constitutional provisions for amendment; historical documentation of peaceful modification of succession rules; examination of social movements'' capacity to pressure monarchical reform without violent rupture',
    'If no exit mechanism: confirms trapped classification for powerless agents; extraction is coercive. If exit mechanisms exist (even costly): reclassifies to constrained, reducing f(d) and effective extraction; suggests possibility of negotiated change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_mechanism_availability, empirical, 'Availability of non-revolutionary mechanisms to alter succession rules').

omega_variable(
    clergy_independence_versus_captured_legitimation,
    'Is the established clergy an independent authority sanctioning the monarch, or a captured institution whose legitimation is pure theater?',
    'Historical documentation of clergy''s ability to withhold sanction; cases where clergy challenged or refused to legitimize particular monarchs; comparison of clergy independence across different monarchical systems',
    'If independent: clergy''s rope perspective is accurate; genuine coordination between separate authorities. If captured: clergy''s perspective is piton—theater of sanction masking enforced alignment; extractiveness of constraint is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clergy_independence_versus_captured_legitimation, empirical, 'Whether established clergy retains independence to sanction or withhold sanction').

omega_variable(
    reading_foreclosure_by_republican_core_axiom,
    'Does the republican reading''s core axiom (popular sovereignty as ultimate authority source) logically foreclose the monarchical reading''s core axiom (hereditary lineage + divine ordination as sole legitimacy source), or do both readings coexist as competing live positions?',
    'Normative analysis of the axioms'' logical relationship; examination of whether a single legal/political framework can hold both axioms simultaneously; historical documentation of how different parties have attempted to reconcile or explicitly reject this coexistence',
    'If forecloses: the two readings are in genuine logical conflict; this reading''s authority must be overridden for the republican reading to hold. If coexists: both readings remain live positions in different factions; the constraint exhibits genuine kernel under-determination. This resolution directly informs cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_by_republican_core_axiom, conceptual, 'Logical compatibility of monarchical vs republican axioms within a single framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monarchical_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mona_tr_t0, monarchical_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mona_tr_t10, monarchical_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(mona_tr_t20, monarchical_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(mona_tr_t30, monarchical_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(mona_be_t0, monarchical_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mona_be_t10, monarchical_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(mona_be_t20, monarchical_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(mona_be_t30, monarchical_reading, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monarchical_reading, identity_coordination).
narrative_ontology:affects_constraint(monarchical_reading, republican_reading).
narrative_ontology:affects_constraint(monarchical_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The monarchical_reading, republican_reading, and constitutional_hybrid_reading are three constraint stories instantiating three readings of the same kernel: sovereign_legitimacy. They are not the same constraint viewed from different angles—they instantiate genuinely different structural relationships between authority, succession, and accountability. Each has its own beneficiary/victim structure, its own ε value, and its own perspectival profile. The network links are mandatory because each reading's stability affects the others: the monarchical reading's credibility is undermined by the institutional success of republican readings; the constitutional hybrid reading occupies a middle ground that influences both. These are constraint family links.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
