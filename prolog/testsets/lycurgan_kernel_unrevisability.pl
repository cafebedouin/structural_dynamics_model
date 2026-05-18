% ============================================================================
% CONSTRAINT STORY: lycurgan_kernel_unrevisability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_kernel_unrevisability, []).

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
 *   constraint_id: lycurgan_kernel_unrevisability
 *   human_readable: Lycurgan Code's Prohibition on Written Law
 *   domain: ancient_politics/spartan_constitution
 *
 * SUMMARY:
 *   The Lycurgan code's prohibition on written law represents the
 *   foundational anchored-fixity mechanism that made Spartan political
 *   organization possible for over three centuries. Lycurgus (whether
 *   historical figure or mythological ancestor) reportedly forbade committing
 *   the constitution to writing on explicit grounds that any textual kernel
 *   would invite commentary, debate, and revision. Instead, the code existed
 *   as an oral tradition maintained by the gerousia (council of elders) and
 *   the ephorate (five executive magistrates), who possessed exclusive
 *   interpretive authority. This constraint exhibits all six DR types across
 *   different structural positions: for Spartiate warriors, it coordinated
 *   collective identity and military discipline (tangled rope — mixed
 *   coordination and enforced conformity); for helots, it legitimized pure
 *   extraction without reciprocal benefit (snare — maximal extractiveness and
 *   suppression); for the gerousia and ephorate, it granted unquestionable
 *   authority (rope — coordination without experienced extraction); for later
 *   generations facing oliganthropia (population decline) and military
 *   pressure, it became an impediment to adaptation (piton — performative
 *   invocation of tradition without functional capacity); and for the
 *   analytical observer, it appears as a false-summit naturalization of a
 *   contingent political choice that served specific elite interests. The
 *   constraint's collapse under pressure (Cleomenes III and Agis IV's failed
 *   reform attempts, Sparta's military decline in the fourth-third centuries
 *   BCE, and eventual conquest by Macedonia) reveals that the illusion of
 *   immutability was the constraint's essential function — once challenged,
 *   it could not regenerate legitimacy because it had no textual foundation
 *   to retreat to. The unwritten status was kernel preservation strategy at
 *   maximum strength: the kernel was preserved by being made inaccessible to
 *   the kind of textual inspection and debate that would reveal drift, enable
 *   challenge, or allow legitimate revision.
 *
 * KEY AGENTS:
 *   - Gerousia and Ephorate: Institutional beneficiaries (institutional/arbitrage) — derive unquestionable interpretive authority from the kernel's oral-only status; experience the constraint as coordination mechanism (rope perspective)
 *   - Spartiate Citizen Body: Moderate beneficiary-victims (moderate/constrained) — benefit from participation in coordinated warrior collective; experience suppression through inability to challenge or revise interpretation; identity-locked to Spartan citizenship (tangled rope perspective)
 *   - Helot Population: Primary victims (powerless/trapped) — experience pure extraction legitimized through immutable tradition; have no exit option and no textual standard to appeal to; suppression is structural and identity-locked (snare perspective)
 *   - Spartan Institutional Adaptability: Structural victim (analytical agent) — the constraint prevents the institutional flexibility needed for environmental adaptation; becomes catastrophic when oliganthropia and military pressure demand innovation (mountain false-summit perspective from analytical observer)
 *   - Constitutional Revision Mechanism: Absent institutional victim — no legitimate pathway for amending the kernel even when environmental change makes adaptation necessary; the constraint prevents the possibility of reform (snare perspective at institutional level)
 *   - Reformist Movement (Cleomenes III, Agis IV): Constrained organized agents (organized/constrained) — attempt to reinterpret the kernel to enable adaptation; fail because the constraint's enforcement is too strong (scaffold perspective with failed sunset)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_kernel_unrevisability, 0.58).
domain_priors:suppression_score(lycurgan_kernel_unrevisability, 0.75).
domain_priors:theater_ratio(lycurgan_kernel_unrevisability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_kernel_unrevisability, extractiveness, 0.58).
narrative_ontology:constraint_metric(lycurgan_kernel_unrevisability, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(lycurgan_kernel_unrevisability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_kernel_unrevisability, tangled_rope).
narrative_ontology:human_readable(lycurgan_kernel_unrevisability, "Lycurgan Code's Prohibition on Written Law").
narrative_ontology:topic_domain(lycurgan_kernel_unrevisability, "ancient_politics/spartan_constitution").

domain_priors:requires_active_enforcement(lycurgan_kernel_unrevisability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_kernel_unrevisability, gerousia_ephorate).
narrative_ontology:constraint_beneficiary(lycurgan_kernel_unrevisability, spartiate_citizen_elite).
narrative_ontology:constraint_victim(lycurgan_kernel_unrevisability, helot_population).
narrative_ontology:constraint_victim(lycurgan_kernel_unrevisability, spartan_institutional_adaptability).
narrative_ontology:constraint_victim(lycurgan_kernel_unrevisability, constitutional_revision_mechanism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPARTIATE CITIZEN (TANGLED ROPE) — Experiences the unwritten code as both coordination mechanism and extraction device. The oral kernel coordinates warrior discipline, shared citizenship ideals, and military collective action — genuine coordination goods. Simultaneously, the inability to challenge or revise the interpreted code extracts conformity from the citizen body; dissent becomes heresy against immutable tradition. High suppression (0.75) because exit from Spartan citizenship was structurally possible but socially/identity-locked — abandoning the polis meant abandoning Spartiate identity itself. The citizen is both beneficiary (participation in the coordinated elite warrior collective) and victim (coerced conformity to an interpretation they cannot textually challenge).
constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: HELOT POPULATION (SNARE) — Pure extraction with maximal suppression (0.75). The unwritten code legitimized helot subjugation through appeal to immutable tradition — 'this is how things have always been, as Lycurgus ordained.' The helots bore complete extraction (tribute, labor, terror) with no internal coordination benefit and no legitimate exit path. The kernel's oral-only status prevented helots from appealing to any written standard or demanding textual justification. They experienced maximum extractiveness because the constraint existed solely to extract their labor and suppress their agency, with zero coordination function from their perspective. Suppression was structural (violent subordination) and identity-locked (helot status was hereditary, constituting their permanent social position).
constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a long-term analytical stance, the constraint appears to be a natural law of political organization: 'unwritten constitutions preserve stability; written revision invites decay.' This perspective naturalizes the Lycurgan prohibition as an inherent principle of institutional design. However, this is a false summit. The constraint is not a natural law but a contingent political choice with identifiable beneficiaries (the gerousia and ephorate, who derived absolute interpretive authority from the kernel's textual inaccessibility). The analytical observer sees the 'natural law' framing only when refusing to examine the structural beneficiaries and victims. The engine's false-summit detector will flag this perspective's attempted naturalization.
constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: LATER SPARTAN INSTITUTIONAL FRAMEWORK (PITON) — By the fourth century BCE, as oliganthropia (population decline) and economic pressure forced adaptation, the unwritten code became a degraded constraint. The kernel no longer coordinated Spartan survival effectively — it prevented the institutional flexibility required to adapt to changed circumstances (recruitment of perioeci, reorganization of land tenure, military innovation). The gerousia and ephorate continued to invoke the kernel's immutability through ritual invocation, but the kernel's actual function had decayed. Theater ratio (0.68) captures this degradation: the performance of invoking timeless tradition persisted long after the tradition's functional capacity to coordinate had atrophied. The constraint persisted through institutional inertia and the elite's investment in their inherited interpretive authority, not because it still worked.
constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: GEROUSIA AND EPHORATE (ROPE) — The councils of elders and the ephorate experience the unwritten code as pure coordination mechanism from their structural position. The oral-only kernel gave them exclusive interpretive authority — they were the legitimate voice of the immutable tradition, deriving legitimacy and power from their role as custodians of an untextable, unchallengeable kernel. They experience the constraint as coordination (unity through shared appeal to tradition, prevention of factionalism through invocation of immutable law, delegation of institutional authority through the kernel's own existence). No extraction from their perspective because they are net beneficiaries: the constraint extracts toward them in the form of unquestionable authority. Extractiveness and suppression are experienced as coordination and order, not as coercion. Theater ratio (0.68) from their view is low — the invocation of tradition is functional, not performative, from their standpoint.
constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ATTEMPTED REFORMIST MOVEMENT (SCAFFOLD) — Historical figures like Cleomenes III and Agis IV attempted to revise or reinterpret the kernel to enable institutional adaptation while preserving Spartan identity. Their perspective treats the unwritten code as a temporary constraint with a potential sunset: if the kernel could be reinterpreted (e.g., 'the true Lycurgan vision included land redistribution and perioeci integration'), a revised version might enable adaptation. This perspective sees the constraint as soluble rather than immutable. The reformists were constrained agents (they faced the full force of the gerousia's interpretive monopoly and the military establishment's resistance) but they saw a potential exit path: reinterpretation rather than overthrow. Ultimately, the reform movement failed because the constraint was stronger than any single agent could challenge — the kernel's immutability was enforced too deeply. But the perspective reveals that the constraint was not actually immutable; it was maintained through concentrated power and suppression. Had the reformists succeeded, the sunset would have been: reinterpretation of the kernel + democratic voting on revisions + written codification of the new law. They did not succeed; instead, Sparta collapsed and was eventually conquered.
constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, scaffold,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_kernel_unrevisability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lycurgan_kernel_unrevisability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_kernel_unrevisability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lycurgan_kernel_unrevisability, TR),
    TR >= 0.70.

:- end_tests(lycurgan_kernel_unrevisability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Moderate-high. The constraint extracts conformity from the Spartiate body and absolute subjugation from helots, but extracts coordination benefits to the elite. Measuring extractiveness from the population-wide perspective (including helots), the extraction is severe (averaging ~0.72 at the endpoint); measured from the Spartiate perspective alone, it appears more moderate (~0.40-0.50) because of the genuine coordination goods. The value of 0.58 reflects the population-weighted average across the interval, rising from 0.35 in the early period (when the constraint functioned efficiently as a coordination mechanism with limited suppression) to 0.72 by the end (when oliganthropia made the kernel's immutability extractive rather than protective). Suppression (0.75): High throughout the interval. The suppression mechanism has two components: (1) structural coercion enforced through military discipline and helot violence, and (2) identity-lock that makes citizens unable to imagine abandoning Spartan identity even when the system becomes dysfunctional. For helots, suppression is pure structural coercion with hereditary imprisonment in status. For citizens, suppression combines both mechanisms, making exit functionally possible but unthinkable. Theater ratio (0.68): Rising from 0.40 to 0.81 over the interval. In the early period, the kernel functioned effectively — invocation of tradition coordinated genuine military and civic goods, minimizing performative content. As population decline and military pressure accumulated, the kernel's actual coordination capacity declined while the frequency of appeals to tradition increased. By the fourth century, invoking the kernel became performative: elders cited immutable tradition while the actual substance of Spartan military organization was degraded, and power consolidated into fewer hands. The rising theater ratio captures the piton degradation — the ritual of invocation persisted long after the functional coordination had atrophied. Claimed type (tangled rope): Justified by the presence of both genuine coordination function (warrior discipline, collective identity, prevention of factionalism) and asymmetric extraction (suppression of revision, coerced conformity, helot subjugation, prevention of institutional adaptation). The constraint is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid where elite authority and coordination goods are structurally entangled.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The gerousia sees Rope (unquestionable coordination mechanism). The Spartiate citizen sees Tangled Rope (mixed coordination and enforced conformity, identity-locked exit). The helot sees Snare (pure extraction with no exit). The analytical observer sees a false-summit Mountain (naturalization of contingent authority). The later institutional framework sees Piton (degraded ritual). The reformist movement sees Scaffold (temporary constraint with potential sunset through reinterpretation). The constraint's structure is robust enough that all six types can be legitimately assigned from different positions — the perspectival gap is not a measurement error but an accurate reflection of how asymmetrically the constraint distributes coordination benefits and extraction costs across the social hierarchy. The false summit is particularly acute: the constraint is often presented in historical and political theory as a natural law of conservative institutional design ('oral traditions preserve stability') when it was actually a contingent choice that benefited the elite at the cost of suppressing adaptation, preventing reform, and maintaining helot subjugation. The 'naturalness' of the unwritten status is post-hoc rationalization, not structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's structural position relative to the extraction flow and their exit capacity. The gerousia (institutional, arbitrage exit) are full beneficiaries with maximum optionality — derived d ≈ 0.05 (full beneficiary side of spectrum). Spartiate citizens (moderate, constrained exit + mixed beneficiary-victim status) experience d ≈ 0.50 (symmetric, experiencing both coordination goods and extraction costs). Helots (powerless, trapped exit + pure victim status) experience d ≈ 0.95 (full target side of spectrum). The reformist movement (organized, constrained exit + victim-challenger status) experiences d ≈ 0.75 (primarily target, seeking reversal). The institutional adaptability (abstract collective agent, analytical perspective) experiences d ≈ 0.90 (victim of prevention, suffering from the constraint's rigidity). These d values feed into the sigmoid function f(d) to produce effective extractiveness (χ) for each perspective: beneficiaries experience low or negative χ (the constraint subsidizes them); victims experience high χ (the constraint extracts from them); symmetric agents experience moderate χ; analytical observers experience the structural χ without the confounds of the agent's position in the extraction flow. The delta between perspectives' experienced χ values is the perspectival gap: it measures how asymmetrically the constraint distributes extraction across positions.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY — CONSTRAINT FAMILY ANALYSIS. The Lycurgan kernel exhibits mandatrophy at extractiveness 0.58 > 0.46, requiring omega resolution and documentation of how classification prevents confusion between coordination and extraction. The mandatrophy is resolved through perspectival decomposition: the constraint is simultaneously Rope (from gerousia perspective), Tangled Rope (from Spartiate citizen perspective), Snare (from helot perspective), Piton (from institutional degradation perspective), Scaffold (from reform-attempt perspective), and false-summit Mountain (from analytical perspective). These are not contradictory classifications of a single constraint under different measurements — they are legitimate structural readings from genuinely different social positions. The constraint is objectively a mixed mechanism: it coordinates warrior discipline and elite unity (the Rope component, experienced by the gerousia and enabling Spartiate warriors' effectiveness), while simultaneously extracting conformity from citizens and absolute subjugation from helots (the Snare and Tangled Rope components). The mandatrophy is resolved by recognizing that asymmetric social hierarchies naturally generate perspectival divergence — the constraint's classification type depends materially on the observer's structural position in the extraction flow. There is no 'true' type that all positions should converge on; rather, the presheaf of classifications over different observer positions IS the structural truth. The false-summit Mountain is diagnostic: it reveals that analytical observers risk naturalizing contingent institutional arrangements as inherent laws if they abstract away from the concrete beneficiaries and victims. The reformist Scaffold is also diagnostic: it shows that the constraint's apparent immutability depended on enforcement and suppression, not on genuine structural necessity — once challenged (even unsuccessfully), the constraint's brittleness was revealed. The piton classification documents the constraint's degradation as its functional coordination capacity atrophied but its enforcement machinery persisted through institutional inertia. Resolution: the constraint is objectively Tangled Rope at the population level (mixing genuine coordination with substantial extraction and suppression) but exhibits perspectival divergence across the six types when viewed from specific structural positions. This is the correct analytic result, not a failure of the classification system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_preservation_efficacy,
    'Did the oral-only status actually preserve the kernel more faithfully than writing would have, or did it enable drift through reinterpretation?',
    'Comparative textual analysis of Lycurgan law as reported by different sources (Plutarch, Xenophon, Aristotle); reconstruction of which interpretations changed over time; identification of inconsistencies in the reported kernel across sources.',
    'If oral preservation was effective: the constraint functioned as claimed (stability through immutability). If oral preservation enabled drift: the constraint relied on false narrative (immutability was illusory) to maintain authority — making it a snare, not a mountain. This determines whether the false-summit classification is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oral_preservation_efficacy, empirical, 'Whether oral-only status preserved the kernel faithfully or enabled interpretive drift').

omega_variable(
    gerousia_interpretive_monopoly_mechanism,
    'How did the gerousia enforce exclusive interpretive authority over the unwritten kernel? What prevented rival interpretations from circulating?',
    'Historical analysis of documented challenges to the gerousia''s interpretation; examination of institutional mechanisms (religious sanctions, military enforcement, social ostracism) used to suppress rival readings; comparison with other oral-tradition societies'' methods of managing interpretive contestation.',
    'If enforcement was purely institutional (coercive authority backed by the military): the constraint''s stability depended on force, making it a snare/tangled rope, not a natural law. If enforcement was partly epistemic (the kernel was genuinely difficult to access, making rival interpretations literally impossible): the constraint had a structural basis for immutability. If enforcement was partly identity-based (citizens were culturally conditioned to accept the gerousia''s interpretation without question): identity_locked exit options apply to citizens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gerousia_interpretive_monopoly_mechanism, empirical, 'Mechanisms of gerousia''s interpretive monopoly enforcement').

omega_variable(
    categorical_necessity_of_unwritten_status,
    'Was the unwritten status essential to the kernel''s function, or was it contingent — could a written code with restricted-access scrolls have served the same coordination function?',
    'Counterfactual institutional analysis: comparison with other Greek poleis that used written law with restricted revision procedures (Athens, Corinth); examination of whether the coordination benefits (warrior discipline, elite unity) could have been achieved with a written code preserved under strict interpretive control; analysis of whether the suppression benefits (prevention of challenge to authority) depend specifically on textual inaccessibility or merely on enforcement monopoly.',
    'If unwritten status was essential: the constraint embodies a genuine insight about institutional design (Rope-level coordination benefit). If status was contingent: the constraint relied on false necessity (the unwritten status was chosen for extractive benefits, not coordination benefits) — making it more snare-like than the tangled-rope classification suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_necessity_of_unwritten_status, conceptual, 'Whether unwritten status was categorically necessary or contingent choice').

omega_variable(
    oliganthropia_cascade_timing,
    'At what point did population decline make the kernel''s immutability catastrophic? Could adaptation have occurred if the kernel had been revisable?',
    'Demographic reconstruction of Spartan population 400-200 BCE; timeline of military failures and institutional stresses; counterfactual analysis of whether specific military defeats (Leuctra, Mantinea, conflicts with Macedonia) would have had different outcomes if Sparta had maintained institutional flexibility; comparison with how other poleis adapted to population decline.',
    'If adaptation could have prevented collapse: the constraint''s immutability was a critical failure point, making it a snare (pure extraction of inflexibility). If adaptation could not have prevented collapse: the constraint was incidental to Sparta''s decline (a piton, not a snare) — Sparta was doomed by structural factors beyond institutional reform. This determines whether the constraint was actively extractive (preventing survival) or merely degraded (persisting after functionality was lost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oliganthropia_cascade_timing, empirical, 'Whether institutional revisability could have enabled Spartan adaptation to oliganthropia').

omega_variable(
    identity_lock_vs_coercive_suppression,
    'For Spartiate citizens, was the suppression (0.75) experienced as identity-locked (they could not imagine being non-Spartiate) or as coercive (they could imagine exit but faced intolerable costs)?',
    'Historical analysis of documented cases where Spartiate citizens abandoned the polis or considered doing so; examination of defector accounts (e.g., perioeci who transitioned to citizen status, or Spartiate exile narratives); comparison of Spartan exit barriers with other Greek city-states to identify whether the barrier was structural coercion or identity fusion.',
    'If identity-locked: exit_options should shift from ''constrained'' to ''identity_locked'' for the Spartiate perspective, which could alter the tangled-rope classification boundary. If coercive: ''constrained'' is accurate, and the tangled-rope classification stands. This affects diagnosis of whether the constraint''s stability depended on internalized acceptance or on external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercive_suppression, empirical, 'Suppression mechanism: identity-locked vs coercive barriers to exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_kernel_unrevisability, 0, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_tr_t0, lycurgan_kernel_unrevisability, theater_ratio, 0, 0.4).
narrative_ontology:measurement(lycurgan_tr_t200, lycurgan_kernel_unrevisability, theater_ratio, 200, 0.55).
narrative_ontology:measurement(lycurgan_tr_t400, lycurgan_kernel_unrevisability, theater_ratio, 400, 0.68).
narrative_ontology:measurement(lycurgan_tr_t450, lycurgan_kernel_unrevisability, theater_ratio, 450, 0.81).

% Extraction over time
narrative_ontology:measurement(lycurgan_be_t0, lycurgan_kernel_unrevisability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lycurgan_be_t200, lycurgan_kernel_unrevisability, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(lycurgan_be_t400, lycurgan_kernel_unrevisability, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(lycurgan_be_t450, lycurgan_kernel_unrevisability, base_extractiveness, 450, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_kernel_unrevisability, enforcement_mechanism).
narrative_ontology:affects_constraint(lycurgan_kernel_unrevisability, spartan_military_organization).
narrative_ontology:affects_constraint(lycurgan_kernel_unrevisability, helot_subordination_system).
narrative_ontology:affects_constraint(lycurgan_kernel_unrevisability, ephorate_check_system).

% DUAL FORMULATION NOTE:
% The Lycurgan kernel prohibition decomposes into three structurally distinct constraints within the Spartan constraint family. (1) spartan_military_organization (ε≈0.25, Rope): genuine coordination of warrior discipline and collective training — verification shows this coordination function is real and partially explains the kernel's durability. (2) helot_subordination_system (ε≈0.85, Snare): pure extraction and terror apparatus — verification shows helot subjugation was the extracted side of the 'immutable tradition' legitimation. (3) lycurgan_kernel_unrevisability (ε≈0.58, Tangled Rope): the meta-constraint that prevents revision of the kernel and prevents institutional adaptation — verification shows this was the mechanism that bound the other two together and prevented reform once environmental change made adaptation necessary. The kernel prohibition is upstream of the other two: it prevents the helot system from being reformed away (even when reform seemed necessary), and it prevents the military organization from being adapted to changed circumstances. The ε-invariance principle justifies three separate stories rather than one: measuring the kernel's extractiveness by its effect on helot subjugation yields ε≈0.85 (Snare); measuring it by its effect on military coordination yields ε≈0.25 (Rope); measuring it by its effect on institutional adaptability yields ε≈0.58 (Tangled Rope). Since observable selection changes ε, the three constraints are structurally distinct and should be modeled as a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_kernel_unrevisability, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
