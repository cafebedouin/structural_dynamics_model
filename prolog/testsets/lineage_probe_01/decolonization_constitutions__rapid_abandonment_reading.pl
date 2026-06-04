% ============================================================================
% CONSTRAINT STORY: decolonization_constitutions__rapid_abandonment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decolonization_constitutions__rapid_abandonment_reading, []).

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
 *   constraint_id: decolonization_constitutions__rapid_abandonment_reading
 *   human_readable: Rapid Abandonment of Decolonization Constitutions: Westminster Rejection as Snare
 *   domain: political/legal/postcolonial
 *
 * SUMMARY:
 *   The rapid abandonment reading instantiates one structural interpretation
 *   of decolonization constitutions: the Westminster templates exported to
 *   newly independent states of Africa and Asia in the late 1950s and 1960s
 *   were systematically dismantled by the first generation of inheritors.
 *   Within a single decade — Ghana (1964), Congo (1965), Nigeria (1966),
 *   Uganda (1971) — elected parliaments were suspended, constitutional checks
 *   on executive power were stripped, and unitary one-party states or
 *   military councils replaced the parliamentary form. This reading treats
 *   the abandonment not as inevitable postcolonial necessity but as
 *   deliberate extraction: consolidating executives suppressed the
 *   institutional checks that Lancaster House agreements had meant to
 *   guarantee, using the language of state-building and national unity to
 *   justify the suppression. The constraint exhibits structural properties of
 *   a Snare: high extractiveness (0.68), high suppression (0.72), and very
 *   high theater (0.85). The form persists ceremonially — parliaments still
 *   meet, courts still exist, constitutions are still written — but their
 *   checking function has been hollowed out. This reading competes with two
 *   siblings: the durable adaptation reading (India and Caribbean democracies
 *   kept Westminster forms by transforming them on local material) and the
 *   Lancaster House template reading (independence came pre-drafted with
 *   enforceable constitutional protections). The rapid abandonment reading
 *   takes the constitutional form itself as a temporary scaffold that
 *   consolidating executives were always positioned to dismantle.
 *
 * KEY AGENTS:
 *   - Consolidating Executives (Nkrumah, Mobutu, Gowon, Amin): Primary beneficiaries (institutional/arbitrage) — extract power by suppressing the parliamentary checks meant to constrain them; use state-building rhetoric to justify constitutional suspension
 *   - Constitutional Oppositions and Minorities: Primary victims (powerless/trapped) — trapped in national political arena with no exit; the checks and balances meant to protect them become dead letters within the decade
 *   - Military Councils: Secondary beneficiaries (powerful/arbitrage) — benefit from the power vacuum created by suppression of parliamentary checks; operate outside the constitutional frame even when officially subordinate to civilian authority
 *   - Regional Institutional Frameworks (bureaucracies, civil services): Constrained participants (powerful/constrained) — depend on inherited institutions for state capacity but also participate in their dismantling; experience mixed extraction and coordination benefits
 *   - Westminster Ceremonial Apparatus (parliaments, courts, written constitutions): Degraded institutions (institutional/arbitrage) — persist as theater; invoked to legitimize what they no longer constrain
 *   - Lancaster House Negotiators (departed colonial power, ex-imperial bureaucrats): Absent beneficiaries (institutional/arbitrage) — architects of the template who departed; the suppression occurs after they have left, so their ability to enforce the agreement is non-existent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decolonization_constitutions__rapid_abandonment_reading, 0.68).
domain_priors:suppression_score(decolonization_constitutions__rapid_abandonment_reading, 0.72).
domain_priors:theater_ratio(decolonization_constitutions__rapid_abandonment_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decolonization_constitutions__rapid_abandonment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(decolonization_constitutions__rapid_abandonment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(decolonization_constitutions__rapid_abandonment_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decolonization_constitutions__rapid_abandonment_reading, snare).
narrative_ontology:human_readable(decolonization_constitutions__rapid_abandonment_reading, "Rapid Abandonment of Decolonization Constitutions: Westminster Rejection as Snare").
narrative_ontology:topic_domain(decolonization_constitutions__rapid_abandonment_reading, "political/legal/postcolonial").

domain_priors:requires_active_enforcement(decolonization_constitutions__rapid_abandonment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decolonization_constitutions__rapid_abandonment_reading, 'cb96816d-44e9-4763-a0e2-6b8d13147aef').
narrative_ontology:cs_kernel_codification('cb96816d-44e9-4763-a0e2-6b8d13147aef', fixed_text).
narrative_ontology:cs_authority_grounding('cb96816d-44e9-4763-a0e2-6b8d13147aef', extraction).
narrative_ontology:cs_interpretation_layer_present('cb96816d-44e9-4763-a0e2-6b8d13147aef').
narrative_ontology:cs_reading_relation('cb96816d-44e9-4763-a0e2-6b8d13147aef', decolonization_constitutions__durable_adaptation_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb96816d-44e9-4763-a0e2-6b8d13147aef', decolonization_constitutions__lancaster_house_template_reading, influences).
narrative_ontology:cs_axiom('cb96816d-44e9-4763-a0e2-6b8d13147aef', foundational, westminster_rejection_by_consolidating_power).
narrative_ontology:cs_axiom_status(westminster_rejection_by_consolidating_power, holdable).
narrative_ontology:cs_axiom_grounding('cb96816d-44e9-4763-a0e2-6b8d13147aef', westminster_rejection_by_consolidating_power, empirically_contingent).
narrative_ontology:cs_axiom('cb96816d-44e9-4763-a0e2-6b8d13147aef', foundational, inherited_form_as_temporary_scaffolding).
narrative_ontology:cs_axiom_status(inherited_form_as_temporary_scaffolding, holdable).
narrative_ontology:cs_axiom_grounding('cb96816d-44e9-4763-a0e2-6b8d13147aef', inherited_form_as_temporary_scaffolding, instrumental).
narrative_ontology:cs_reference_frame('cb96816d-44e9-4763-a0e2-6b8d13147aef', westminster_constitutional_commitment).
narrative_ontology:cs_drift_state('cb96816d-44e9-4763-a0e2-6b8d13147aef', decade_post_independence, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('cb96816d-44e9-4763-a0e2-6b8d13147aef', '').
narrative_ontology:cs_kernel_id(decolonization_constitutions__rapid_abandonment_reading, decolonization_constitutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decolonization_constitutions__rapid_abandonment_reading, consolidating_executives).
narrative_ontology:constraint_beneficiary(decolonization_constitutions__rapid_abandonment_reading, military_councils).
narrative_ontology:constraint_victim(decolonization_constitutions__rapid_abandonment_reading, constitutional_oppositions).
narrative_ontology:constraint_victim(decolonization_constitutions__rapid_abandonment_reading, minority_protections).
narrative_ontology:constraint_victim(decolonization_constitutions__rapid_abandonment_reading, institutional_checks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL OPPOSITIONS (SNARE) — Trapped within a constitutional framework that is actively being suppressed and dismantled by inheritors. The checks and balances, entrenched rights chapters, and minority protections the Westminster template was meant to guarantee become dead letters within a decade. No exit from the national political arena; no alternative institutional frame; no capacity to enforce the document they did not draft. Maximum extraction as consolidating executives hollow out the form while maintaining the ceremonial shell.
constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL INSTITUTIONAL ACTORS (TANGLED ROPE) — Constrained by the realities of governing through the inherited template while also benefiting from its residual capacity to coordinate state action. These actors experience the constraint as mixed: they participate in the dismantling of checks while also relying on whatever institutional stability the form provides. Partial beneficiaries who also bear the costs of institutional instability as the template erodes.
constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSOLIDATING EXECUTIVES (ROPE) — Experience the inherited Westminster form as a coordination mechanism that can be instrumentalized for centralization. The template provides a legitimate frame for extracting executive power while appearing to preserve the form. First-generation inheritors use the constitution's language and structures to justify the transition to one-party rule or presidential power — the form coordinates the dismantling of the form. Net beneficiaries with full flexibility to redefine the constraint to serve their interests.
constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POST-INDEPENDENCE ELITE CONSENSUS (SCAFFOLD) — The founding cohort that negotiated independence and signed the Lancaster House agreements sees the constitutional transition as temporary scaffolding: the Westminster form was a tool for securing independence, but a sunset logic operates immediately. Within the first decade, the elite consensus shifts to one-party states, military councils, or presidential systems — the template has served its purpose. The scaffold appears to them as an exit path taken, not a structure suppressed. Theater_ratio is high because the constitutional form persists ceremonially while losing functional enforcement.
constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: WESTMINSTER CEREMONIAL APPARATUS (PITON) — The inherited structures (parliaments, courts, written constitutions) persist as degraded institutions: they exist, they are invoked, but their functional enforcement has atrophied. A parliament convenes but has no real checking power; a constitution exists but is routinely suspended or reinterpreted by decree; courts enforce some cases but not those that challenge executive consolidation. The form survives through inertia and ceremonial maintenance, not because it works. Theater_ratio reflects this: the apparatus persists largely to legitimize what it no longer constrains.
constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the abandonment of Westminster forms appears inevitable: all postcolonial states face the pressure to centralize executive power, to eliminate colonial-era institutional constraints, and to build unified national identity through one-party systems or strong presidencies. This perspective treats the rapid abandonment as a law of postcolonial development — you cannot import checks designed for stable imperial cores and expect them to survive in territories building states from fragments. However, the structural data reveals this is a false summit: the constraint's beneficiaries (consolidating executives) actively suppress the checks; the suppression is real (0.72) and the extraction is high (0.68). This is not inevitability — it is deliberate dismantling masked as natural process.
constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decolonization_constitutions__rapid_abandonment_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decolonization_constitutions__rapid_abandonment_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decolonization_constitutions__rapid_abandonment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decolonization_constitutions__rapid_abandonment_reading, TR),
    TR >= 0.70.

:- end_tests(decolonization_constitutions__rapid_abandonment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The consolidating executives capture the ability to govern by suppressing the checks meant to distribute power. Within the decade, the extraction becomes severe as one-party rule or military authority solidifies. The trajectory in measurements (0.35 → 0.68 over ten years) shows acceleration: early constitutionalism gives way rapidly to concentrated executive power. Suppression (0.72): High. The suppression is multifaceted: suspension of parliament, constitutional amendments to expand executive powers, elimination of competing parties, arrest or exile of opposition leaders, and subordination of the judiciary to executive direction. The requirement for suppression is high because the Westminster template did create institutional barriers; those barriers had to be actively broken, not merely sidestepped. Theater ratio (0.85): Very high. The form persists long after its function has been eliminated. Parliaments meet but cannot vote down the executive; courts exist but will not rule against the president; constitutions are written but are routinely suspended. The ceremonial apparatus is maintained because it provides legitimacy for consolidating power while the actual locus of authority has moved elsewhere (military councils, presidential decree, one-party hierarchy). The rising theater ratio (0.55 → 0.85) reflects the widening gap between constitutional form and executive reality.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The consolidating executives see the Westminster form as temporary scaffolding (Scaffold perspective) or pure coordination (Rope) — they experience the dismantling as solving a coordination problem (how to build a unified state). The constitutional oppositions see pure extraction and entrapment (Snare) — they experience the dismantling as suppression of the protections they did not have the power to defend. The military councils see opportunity (Tangled Rope) — they benefit from the power vacuum while also bearing the costs of governing without clear constitutional authority. The Westminster ceremonial apparatus itself is now Piton — it exists and persists but has lost functional force. The analytical observer at civilizational timescale risks seeing this as inevitable (Mountain: all postcolonial states must centralize) — but the false summit detector reveals that the suppression is not inevitable but deliberate, driven by identifiable beneficiaries who actively dismantled the checks.
 *
 * DIRECTIONALITY LOGIC:
 *   The consolidating executives experience low directionality (d ≈ 0.05-0.15): they are beneficiaries of the suppression, with high exit flexibility (arbitrage) — they can redefine the constraint at will, can operate outside it, and can reshape the state structure around their preferred system. The constitutional oppositions experience high directionality (d ≈ 0.90-0.95): they are victims with no exit (trapped) — their power is exclusively within the parliamentary form the constraint dismantles, they cannot opt out of the national arena, and they cannot escape the consolidating power that redefines the rules. The military councils experience moderate-high directionality (d ≈ 0.70-0.75): they benefit from the power vacuum created by suppression but are constrained by the fragility of their legitimacy and the need for civil bureaucratic cooperation. Regional institutional actors experience mixed directionality (d ≈ 0.50-0.60): they benefit from state consolidation in some dimensions (centralized authority provides clarity) but also bear costs of institutional instability and reduced check-and-balance coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that the Westminster template was never a stable equilibrium in the postcolonial context — its status as Rope (pure coordination) or Tangled Rope (mixed coordination/extraction) depended entirely on whether consolidating executives would choose to enforce the checks. Once inheritance passes to executives with incentives to centralize power, the template becomes Snare (pure extraction). The rapid abandonment reading affirms that the snare classification is correct: this was always a potential outcome once power consolidated, and it became the actual outcome within a decade across multiple cases. The mandatrophy is resolved by recognizing that the constraint's classification is not time-invariant — it shifted from Rope/Tangled Rope (at independence, when the checks were meant to operate) to Snare (within a decade, as those checks were systematically suppressed). This reading emphasizes the extractive endpoint; the durable adaptation reading emphasizes cases where the checks persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consolidation_necessity_vs_extraction,
    'Is the suppression of Westminster checks a structural necessity for postcolonial state-building, or deliberate extraction by consolidating executives using state-building as cover?',
    'Comparative historical analysis: cases where executives preserved constitutional checks (India, Caribbean democracies) vs cases where checks were rapidly abandoned (Ghana, Congo, Nigeria in this period). If preservation and abandonment occur under similar state-building pressures, the difference is deliberate choice, not structural necessity.',
    'If necessary: reclassify as Mountain (natural pressure). If extractive: remains Snare (deliberate suppression by beneficiaries). If mixed: remains Tangled Rope or Snare depending on the ratio of necessity to extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consolidation_necessity_vs_extraction, empirical, 'Whether constitutional abandonment is structural necessity or deliberate extraction').

omega_variable(
    recipient_resistance_capacity,
    'Did oppositions and minorities attempt organized defense of Westminster constitutional protections, or did they lack capacity from day one?',
    'Archive analysis: constitutional court cases, legislative record of amendments, opposition party documentation, civil society mobilization in defense of checks and balances during the first 10 years post-independence. Presence of organized resistance suggests capacity existed but was suppressed (Snare). Absence suggests victims were trapped from the start (Mountain-adjacent Snare).',
    'High resistance capacity that was suppressed: extraction is deliberate (Snare confirmed). Low or no resistance capacity: suppression was easier than active enforcement (still Snare but with lower-cost extraction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recipient_resistance_capacity, empirical, 'Whether constitutional oppositions mounted organized defense of checks').

omega_variable(
    reading_contest_foreclosure,
    'Does the rapid abandonment reading logically foreclose the durable adaptation reading (India, Caribbean) within a single framework, or do they coexist as different outcomes of the same postcolonial moment?',
    'Conceptual: Ask whether the rapid abandonment reading''s core claim (''Westminster forms were rejected universally as incompatible with postcolonial consolidation'') necessarily rules out the durable adaptation reading''s core claim (''Westminster forms survived through local rebuilding in India and the Caribbean''). If yes: foreclosure. If both readings can be held simultaneously by recognizing different outcomes in different contexts, the readings coexist.',
    'If foreclosure: only one reading can be correct within postcolonial legal theory. If coexistence: both readings are live positions held by scholars emphasizing different regional outcomes; the kernel permits both readings to survive in parallel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether rapid abandonment reading forecloses or coexists with durable adaptation reading').

omega_variable(
    lancaster_house_blueprint_salience,
    'To what degree did the Lancaster House template constrain the choices available to inheriting executives, versus providing a framework they could instrumentalize or discard at will?',
    'Document analysis: Lancaster House agreements vs. post-independence constitutional amendments and suspensions. If Lancaster House constraints were routinely violated without penalty, the template lacked enforcement power. If violated constraints triggered international intervention or restoration, the template retained binding force. The degree of constraint determines whether the template was a genuine check (Rope/Tangled Rope reality) or a performance stage for dismantling (Snare reality).',
    'Strong constraint: template was binding (durable_adaptation_reading gains force). Weak constraint: template was instrumental (rapid_abandonment_reading confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lancaster_house_blueprint_salience, empirical, 'Salience and enforcement power of Lancaster House blueprint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decolonization_constitutions__rapid_abandonment_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decolcon_rapid_tr_t0, decolonization_constitutions__rapid_abandonment_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(decolcon_rapid_tr_t2, decolonization_constitutions__rapid_abandonment_reading, theater_ratio, 2, 0.68).
narrative_ontology:measurement(decolcon_rapid_tr_t5, decolonization_constitutions__rapid_abandonment_reading, theater_ratio, 5, 0.78).
narrative_ontology:measurement(decolcon_rapid_tr_t10, decolonization_constitutions__rapid_abandonment_reading, theater_ratio, 10, 0.85).

% Extraction over time
narrative_ontology:measurement(decolcon_rapid_be_t0, decolonization_constitutions__rapid_abandonment_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(decolcon_rapid_be_t2, decolonization_constitutions__rapid_abandonment_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(decolcon_rapid_be_t5, decolonization_constitutions__rapid_abandonment_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(decolcon_rapid_be_t10, decolonization_constitutions__rapid_abandonment_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(decolcon_rapid_su_t0, decolonization_constitutions__rapid_abandonment_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(decolcon_rapid_su_t5, decolonization_constitutions__rapid_abandonment_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(decolcon_rapid_su_t10, decolonization_constitutions__rapid_abandonment_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decolonization_constitutions__rapid_abandonment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(decolonization_constitutions__rapid_abandonment_reading, decolonization_constitutions__durable_adaptation_reading).
narrative_ontology:affects_constraint(decolonization_constitutions__rapid_abandonment_reading, decolonization_constitutions__lancaster_house_template_reading).

% DUAL FORMULATION NOTE:
% The decolonization_constitutions kernel has three reading instantiations, each with distinct extractiveness values reflecting different structural outcomes of the same postcolonial moment. The rapid_abandonment_reading models the cases where the template was suppressed (ε=0.68, Snare). The durable_adaptation_reading models the cases where the template survived through transformation (lower ε, Rope/Tangled Rope). The lancaster_house_template_reading models the constitutional commitment itself as the constraint, independent of its preservation or abandonment (ε variable depending on enforcement perspective). All three are linked via network.affects_constraints because they address the same kernel and their outcomes are causally related: where adaptation took hold, abandonment did not; where Lancaster House protections were enforced, rapid abandonment was prevented.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
