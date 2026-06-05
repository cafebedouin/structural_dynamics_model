% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation: Study as Archiving (Post-Temple Binding Law Without Performance)
 *   domain: religious_studies/halakhic_commitment_systems
 *
 * SUMMARY:
 *   This constraint represents ONE READING of the contested temple sacrifice
 *   obligation kernel. The reading instantiated here is STUDY_AS_ARCHIVING:
 *   the position that study of sacrificial law preserves knowledge for future
 *   Temple restoration but does NOT constitute fulfillment of the binding
 *   divine obligation. This is structurally distinct from two sibling
 *   readings: (1) STUDY_AS_OCCUPATION, which claims that study of sacrifice
 *   law occupies the obligation legitimately in the Temple's absence, and (2)
 *   MESSIANIC_SUSPENSION, which claims the obligation is suspended (neither
 *   fulfilled nor violated) pending messianic restoration. The archiving
 *   reading asserts an asymmetry: the obligation remains binding and
 *   unfulfilled; study's function is preservational, not substitutional. This
 *   creates a tangled_rope structure: the rabbinic authority benefits from
 *   maintaining the obligation's binding status (beneficiary position) while
 *   the observant community and the unfulfilled obligation itself are victims
 *   of indefinite non-performance. The halakhic archiving system itself
 *   becomes increasingly performative (theater_ratio rising from 0.40 to 0.58
 *   over 1500 years) as the Temple's non-existence becomes historical
 *   certainty rather than hoped-for temporary condition.
 *
 * KEY AGENTS:
 *   - Observant Jewish Community: Primary victim (powerless/identity_locked) — constituted through the obligation but prevented from fulfilling it; binding status maintained indefinitely
 *   - The Unfulfilled Divine Command: Abstract victim (victim set itself) — the obligation in its binding form, never executed
 *   - Rabbinic Interpretive Authority: Primary beneficiary (institutional/arbitrage) — authority to interpret, transmit, and maintain the obligation's binding status; captures institutional power from archiving framework
 *   - Textual Transmission Institutions: Secondary beneficiary (institutional/arbitrage) — yeshivot, scholarly networks, legal codification — benefit from maintaining obligation's binding status
 *   - Sacrificial Scholars: Mixed position (moderate/constrained) — genuinely coordinate knowledge preservation but bear extraction cost of studying unexecutable law
 *   - Messianic Restoration Movement: Organized dissent (organized/constrained) — assert that archiving does not fulfill obligation; coordinate around restoration expectation
 *   - Analytical Observer: Civilizational witness (analytical/analytical) — risks naturalizing constructed framework as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.48).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.62).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.48).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation: Study as Archiving (Post-Temple Binding Law Without Performance)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious_studies/halakhic_commitment_systems").

domain_priors:requires_active_enforcement(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '0ba5553d-f40b-41e8-9342-852d12c0d10f').
narrative_ontology:cs_kernel_codification('0ba5553d-f40b-41e8-9342-852d12c0d10f', fixed_text).
narrative_ontology:cs_authority_grounding('0ba5553d-f40b-41e8-9342-852d12c0d10f', extraction).
narrative_ontology:cs_interpretation_layer_present('0ba5553d-f40b-41e8-9342-852d12c0d10f').
narrative_ontology:cs_reading_relation('0ba5553d-f40b-41e8-9342-852d12c0d10f', temple_sacrifice_obligation__study_as_occupation, forecloses).
narrative_ontology:cs_reading_relation('0ba5553d-f40b-41e8-9342-852d12c0d10f', temple_sacrifice_obligation__messianic_suspension, influences).
narrative_ontology:cs_axiom('0ba5553d-f40b-41e8-9342-852d12c0d10f', foundational, obligation_remains_binding_post_temple).
narrative_ontology:cs_axiom_status(obligation_remains_binding_post_temple, holdable).
narrative_ontology:cs_axiom_grounding('0ba5553d-f40b-41e8-9342-852d12c0d10f', obligation_remains_binding_post_temple, deontological).
narrative_ontology:cs_axiom('0ba5553d-f40b-41e8-9342-852d12c0d10f', foundational, study_preserves_not_fulfills).
narrative_ontology:cs_axiom_status(study_preserves_not_fulfills, holdable).
narrative_ontology:cs_axiom_grounding('0ba5553d-f40b-41e8-9342-852d12c0d10f', study_preserves_not_fulfills, deontological).
narrative_ontology:cs_reference_frame('0ba5553d-f40b-41e8-9342-852d12c0d10f', binding_obligation_unfulfilled_archiving_framework).
narrative_ontology:cs_drift_state('0ba5553d-f40b-41e8-9342-852d12c0d10f', contemporary_post_enlightenment_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0ba5553d-f40b-41e8-9342-852d12c0d10f', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, textual_transmission_institutions).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, unfulfilled_divine_command).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jews_observing_incomplete_practice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBSERVANT COMMUNITY (SNARE) — Constituted through the obligation yet structurally unable to perform. Identity is fused with the commandment (part of core Jewish practice commitment). The binding remains civilizationally; exit would mean abandoning core identity. Full victimhood: taught the obligation is binding, prevented from fulfilling it, and held to the binding status indefinitely. Extraction mechanism is the obligation's continued binding authority despite unavoidable non-compliance.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: RABBINIC AUTHORITY (ROPE) — Benefits from maintaining the obligation's binding status while permitting study substitution. Authority to interpret, preserve, and transmit the knowledge becomes the central institutional function. The interpretive regime coordinates study activity (genuine coordination function) while capturing the authority position (beneficiary of the archiving framework). Net beneficiary with significant coordination role — sees the constraint as legitimate framework for their institutional power.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SACRIFICIAL SCHOLARS (TANGLED ROPE) — Genuinely coordinate knowledge preservation and textual transmission (coordination function), yet bear extraction costs: required to study law they cannot execute, constrained by the obligation's continued binding status. Study partially occupies the obligation but does not fulfill it — schism between commitment and capacity. Moderate power with constrained exit — can exit scholarship but exit disrupts identity and community standing.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HALAKHIC ARCHIVING SYSTEM (PITON) — Generates extensive legal literature, detailed protocols, refined interpretations of sacrificial law—yet acknowledges that no actual sacrifice occurs. The performative content is high (detailed ritual description, precise legal analysis) while the functional content is zero (no Temple exists, no performance possible). Maintained through institutional inertia and the binding-obligation fiction, not because the archiving actually fulfills the obligation. Theater ratio ≥0.70 indicates degraded primary function with persistent institutional structure.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MESSIANIC ADVOCATES (TANGLED ROPE) — Organized agents (Temple restoration movements, third-Temple scholars) see the archiving as partial occupation with genuine unfulfilled obligation. They coordinate around the claim that study does NOT fulfill the obligation and that restoration is the required completion. Active enforcement of obligation's binding status coordinates their movement. Constrained exit — exit would mean accepting study-as-fulfillment frame (incompatible with messianic premise).
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL (MOUNTAIN VIEW) — From the civilizational perspective, the constraint appears immutable: a divine obligation that cannot be performed in the post-Temple world is an unchangeable structural feature of halakhic reality. No authority can change it; no interpretation can fulfill it; study cannot constitute performance. This perspective risks naturalizing what may be a constructed framework (false summit candidate). The binding authority maintains the obligation's status; the obligation's status maintains the authority's interpretive power.
constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temple_sacrifice_obligation__study_as_archiving, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, TR),
    TR >= 0.70.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The archiving reading entails the binding obligation's perpetual non-fulfillment. The core extraction is imposed on the observant community and the unfulfilled obligation itself — indefinite binding status without possible performance. However, the extractiveness is moderate rather than high (snare level ≥0.66) because: (1) the archiving function is genuine (study does preserve knowledge; this is not pure theater), (2) the obligation is not enforced through external coercion (no punishment for non-performance), (3) the reading is contested—other readings exist that reframe the obligation as occupied or suspended. Suppression (0.62): Moderate-high. Barriers to exit the framework include: identity fusion (being Jewish entails the obligation), interpretive authority (the rabbinate maintains binding status), textual infrastructure (halakhic literature presupposes the binding law), community practice (obligation is taught and referenced continuously). These are psychological, institutional, and social rather than legal—but collectively they constitute substantial suppression. Theater ratio (0.58): Moderate-high and rising. The post-Temple halakhic system generates extensive literature on sacrifice law (detailed descriptions, nuanced interpretations) while acknowledging that actual sacrifice does not occur. The performative content increased from 0.40 (early post-Temple, when restoration might have seemed temporally proximate) to 0.58 (after 1500 years, when the performative character becomes structurally obvious). This rising trajectory indicates theater-ratio drift: as the Temple's absence becomes historical certainty, the archiving activity's performative character becomes more salient.
 *
 * PERSPECTIVAL GAP:
 *   The archiving reading produces sharp perspectival gaps. The rabbinic authority and transmission institutions see the constraint as legitimate coordination (Rope)—they are solving the knowledge-preservation problem. The observant community sees binding non-fulfillment (Snare)—they are told the obligation is binding yet cannot perform it. Scholars see mixed coordination and extraction (Tangled Rope)—they genuinely preserve knowledge yet study unexecutable law. The halakhic archiving system sees itself as degraded (Piton)—performative legal analysis with zero functional performance. The messianic movement sees incomplete obligation (Tangled Rope)—study must be superseded by restoration. The analytical observer risks naturalizing the reading as immutable (Mountain)—the obligation's binding status appears unchangeable—but the false-summit detector identifies the beneficiary (rabbinic authority) and questions whether the 'unchangeability' is structural or constructed. Each perspective is coherent within its own position; the gaps reveal that the constraint is not a property of the world but of the interpretive frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation from beneficiary/victim + power + exit: Rabbinic authority (institutional/arbitrage): Full beneficiary. Maintains binding status that anchors their interpretive power. Exit options of arbitrage (can switch to other hermeneutical frameworks) reduce their experienced extraction, but they benefit from status quo → low d → low/negative chi. Observant community (powerless/identity_locked): Full victim. Identity fused with obligation; cannot exit without identity dissolution. Bound to binding status indefinitely → high d → high chi. Scholars (moderate/constrained): Mixed position. Coordinate genuine archiving (Rope-like) yet study law they cannot execute (extraction). Constrained exit (can leave scholarship but disrupts community standing) → moderate d → moderate chi. The perspectival gap is significant: beneficiary sees coordination (Rope); victim sees extraction (Snare); moderate scholar sees hybrid (Tangled Rope); the archiving system itself appears degraded (Piton).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy at the READING level within a kernel contest. The mandatrophy question is not 'which reading is correct?' but 'can the archiving reading be internally coherent while other readings exist?' The archiving reading asserts binding non-fulfillment, which creates tension: (1) why maintain binding status if fulfillment is impossible? (2) is indefinite binding status functionally equivalent to suspension or abolition? (3) does the obligation's binding status serve primarily to maintain rabbinic authority, rather than to pursue the command's substantive intent? The tangled_rope classification resolves the mandatrophy by acknowledging both genuine coordination (study does preserve sacrificial knowledge) and genuine extraction (the binding status is maintained in the beneficiary's interest). The reading coexists with the occupation reading (both held by different communities) and influences the suspension reading (the archiving claim that obligation remains binding creates pressure on the suspension reading's 'held in abeyance' framing). No single reading resolves all tensions; the presheaf of readings IS the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_fulfillment_boundary,
    'Does study of sacrifice law constitute fulfillment of the obligation to perform sacrifice, or does it represent archiving for future performance?',
    'Textual analysis of medieval rabbinic sources on intent (kavvanah) in study: does study require performative intent or preservational intent? Comparison across halakhic communities (Ashkenazi, Sephardi, Kabbalistic) on the fulfillment question.',
    'If study constitutes fulfillment: constraint reclassifies to Rope (study_as_occupation reading). If study is purely archiving: victim set expands to include the unfulfilled obligation itself (current classification sustained).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_fulfillment_boundary, conceptual, 'Whether study fulfills or merely archives the sacrifice obligation').

omega_variable(
    authority_maintenance_extraction,
    'Does the rabbinic authority structure benefit from maintaining the obligation''s binding status while permitting study substitution, and is this benefit substantial enough to constitute extraction?',
    'Historical analysis of authority consolidation following the Temple''s destruction: did interpretive authority expand or contract? Examination of alternatives (cessation of obligation, study-as-fulfillment framing) that would have altered the authority structure. Comparison with rabbinic positions on other suspended commandments.',
    'If extraction is structural: tangled_rope classification confirmed, beneficiary designation upheld. If benefit is incidental to coordination: reclassify to Rope, demote beneficiary to secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_maintenance_extraction, empirical, 'Whether authority structure extracts benefit from obligation-binding status').

omega_variable(
    messianic_timeline_indeterminacy,
    'Does the indefinite messianic timeline render the obligation''s binding status a genuine constraint or a form of perpetual deferral that functions identically to cessation?',
    'Examination of whether indefinite deferral structurally differs from suspension or abolition. Analysis of how the binding status functions in practice when fulfillment is indefinitely postponed. Empirical test: do communities with stronger messianic expectation (shorter timeline) behave differently under the obligation than those with weaker expectation (longer/indefinite timeline)?',
    'If indeterminacy dissolves the constraint''s binding force: extraction mechanism weakens, reclassify toward Piton or Rope. If indeterminacy preserves binding force: current tangled_rope classification sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timeline_indeterminacy, conceptual, 'Whether indefinite messianic deferral functionally suspends or merely postpones the obligation').

omega_variable(
    reading_logical_boundary,
    'Is the study-as-archiving reading logically distinct from the study-as-occupation reading, or do they represent points on a continuum?',
    'Formal analysis of the core claims: archiving reading asserts study does NOT fulfill obligation; occupation reading asserts study DOES fulfill obligation (or constitutes legitimate practice given Temple''s absence). These are logically contradictory at core premise level.',
    'If logically contradictory: reading relation is ''forecloses'' (this reading rules out occupation reading in any single framework). If continuum: relation is ''coexists_with'' (both held by different communities). This resolution determines the cs_structure.reading_relations entry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_boundary, conceptual, 'Logical distinctness of archiving and occupation readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temple_study_theater_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.4).
narrative_ontology:measurement(temple_study_theater_t500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 500, 0.52).
narrative_ontology:measurement(temple_study_theater_t1500, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1500, 0.58).

% Extraction over time
narrative_ontology:measurement(temple_study_extractiveness_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(temple_study_extractiveness_t500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 500, 0.42).
narrative_ontology:measurement(temple_study_extractiveness_t1500, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1500, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, halakhic_authority_post_temple_legitimacy).

% DUAL FORMULATION NOTE:
% The temple sacrifice obligation kernel decomposes into three structurally distinct constraint stories: (1) study_as_archiving (ε=0.48, Tangled Rope)—archiving reading asserting binding non-fulfillment; (2) study_as_occupation (ε=0.32, Rope)—occupation reading asserting study legitimately fulfills obligation; (3) messianic_suspension (ε=0.35, Tangled Rope)—suspension reading asserting obligation is held in abeyance pending restoration. Each story has distinct epsilon because each represents a different structural claim about the obligation's status. Archiving reading has highest epsilon because it maintains binding status without fulfillment mechanism—maximum tension. Network links establish that these are readings of the same kernel, not independent constraints. All three affect the broader constraint of post-Temple halakhic authority legitimacy, which depends on which reading of the obligation is operative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_obligation__study_as_archiving, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
