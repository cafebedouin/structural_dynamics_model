% ============================================================================
% CONSTRAINT STORY: swiss_referendum_system__minority_rights_tension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_swiss_referendum_system__minority_rights_tension, []).

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
 *   constraint_id: swiss_referendum_system__minority_rights_tension
 *   human_readable: Swiss Referendum System: Minority Rights Tension (Minaret Ban Reading)
 *   domain: political/comparative_constitutionalism
 *
 * SUMMARY:
 *   The Swiss referendum system's collision with minority rights emerges most
 *   acutely in the minaret ban (2009), where direct democracy — positioned as
 *   the purest expression of popular sovereignty — voted to restrict
 *   constitutional protections for a religious minority. This constraint
 *   story instantiates one reading of the contested Swiss referendum kernel:
 *   the reading that prioritizes majority power to revise constitutional
 *   commitments via initiative. In this reading, direct democracy exposes
 *   rights protections themselves to majoritarian amendment, creating a snare
 *   mechanism where constitutional minorities have no institutional shelter
 *   from ballot-based extraction. The beneficiary is the majoritarian
 *   coalition that uses direct democracy to entrench its preferences at the
 *   constitutional level; the victims are the targeted minority and the
 *   abstract constitutional principle that rights should protect against
 *   majoritarian amendment. This reading coexists with two sibling readings:
 *   one that emphasizes how the referendum threat forces consensus (magic
 *   formula effect), and one that emphasizes how the initiative mechanism
 *   empowers popular agenda-setting. This constraint story focuses
 *   exclusively on the collision between majority power and minority
 *   protection.
 *
 * KEY AGENTS:
 *   - Majoritarian initiating coalition: Institutional/arbitrage — captures constitutional amendment power through initiative mechanism; benefits from direct-democracy access
 *   - Targeted religious minority (Muslims in Switzerland): Powerless/trapped — faces constitutional amendment by popular vote with no counter-majoritarian shelter; bears full extraction cost
 *   - Constitutional rights framework: Abstract collective victim — the principle that rights protect against majoritarian amendment is itself exposed to ballot revision
 *   - Parliament/representative institutions: Institutional/constrained — experience referendum as coordination threat that forces consensus; have alternative authority channels
 *   - Constitutional Court/supranational review bodies: Institutional/arbitrage — maintain vestigial review function but lack power to overturn constitutional amendments; substantially performative
 *   - Analytical observer: Analytical/analytical — risks naturalizing majority sovereignty as immutable law rather than design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(swiss_referendum_system__minority_rights_tension, 0.58).
domain_priors:suppression_score(swiss_referendum_system__minority_rights_tension, 0.72).
domain_priors:theater_ratio(swiss_referendum_system__minority_rights_tension, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(swiss_referendum_system__minority_rights_tension, extractiveness, 0.58).
narrative_ontology:constraint_metric(swiss_referendum_system__minority_rights_tension, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(swiss_referendum_system__minority_rights_tension, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(swiss_referendum_system__minority_rights_tension, snare).
narrative_ontology:human_readable(swiss_referendum_system__minority_rights_tension, "Swiss Referendum System: Minority Rights Tension (Minaret Ban Reading)").
narrative_ontology:topic_domain(swiss_referendum_system__minority_rights_tension, "political/comparative_constitutionalism").

domain_priors:requires_active_enforcement(swiss_referendum_system__minority_rights_tension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(swiss_referendum_system__minority_rights_tension, '35b59a13-3776-49da-88ed-2446aedea781').
narrative_ontology:cs_kernel_codification('35b59a13-3776-49da-88ed-2446aedea781', formalized).
narrative_ontology:cs_authority_grounding('35b59a13-3776-49da-88ed-2446aedea781', lineage).
narrative_ontology:cs_interpretation_layer_present('35b59a13-3776-49da-88ed-2446aedea781').
narrative_ontology:cs_reading_relation('35b59a13-3776-49da-88ed-2446aedea781', swiss_referendum_system__consensus_forcing_effect, coexists_with).
narrative_ontology:cs_reading_relation('35b59a13-3776-49da-88ed-2446aedea781', swiss_referendum_system__popular_initiative_engine, coexists_with).
narrative_ontology:cs_axiom('35b59a13-3776-49da-88ed-2446aedea781', foundational, majority_sovereignty_over_constitutional_amendment).
narrative_ontology:cs_axiom_status(majority_sovereignty_over_constitutional_amendment, holdable).
narrative_ontology:cs_axiom_grounding('35b59a13-3776-49da-88ed-2446aedea781', majority_sovereignty_over_constitutional_amendment, deontological).
narrative_ontology:cs_axiom('35b59a13-3776-49da-88ed-2446aedea781', secondary, rights_as_revisable_constitutional_commitments).
narrative_ontology:cs_axiom_status(rights_as_revisable_constitutional_commitments, holdable).
narrative_ontology:cs_axiom_grounding('35b59a13-3776-49da-88ed-2446aedea781', rights_as_revisable_constitutional_commitments, deontological).
narrative_ontology:cs_reference_frame('35b59a13-3776-49da-88ed-2446aedea781', popular_sovereignty_unshackled).
narrative_ontology:cs_drift_state('35b59a13-3776-49da-88ed-2446aedea781', post_minaret_ban_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('35b59a13-3776-49da-88ed-2446aedea781', '').
narrative_ontology:cs_kernel_id(swiss_referendum_system__minority_rights_tension, swiss_referendum_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(swiss_referendum_system__minority_rights_tension, majoritarian_initiating_coalition).
narrative_ontology:constraint_victim(swiss_referendum_system__minority_rights_tension, targeted_religious_minority).
narrative_ontology:constraint_victim(swiss_referendum_system__minority_rights_tension, constitutional_rights_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED RELIGIOUS MINORITY (SNARE) — The Muslim minority in Switzerland faces constitutional amendment by popular vote with no institutional shelter. Direct democracy exposes rights to majoritarian revision; exit is geographic (relocation) or submission. No counter-majoritarian institution protects constitutional minorities. The extraction mechanism is the ballot itself: majority can directly vote away minority rights protection.
constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL RIGHTS FRAMEWORK (SNARE) — As an abstract collective good, the constraint that rights are protected from majoritarian amendment is itself a victim. The minaret ban amendment (2009) demonstrates that direct democracy can extract from the rights framework by popular vote. Once a constitutional principle is exposed to ballot revision, its protection mechanism has been fundamentally compromised for future minorities. The structural victim here is not a population but a constitutional principle.
constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJORITARIAN INITIATING COALITION (TANGLED ROPE) — Benefits from direct democracy as a coordination mechanism for mobilizing political will (100,000 signatures organized around a shared objective). Experiences genuine coordination: the initiative mechanism enables expression of latent public preferences that representative institutions might suppress. But also benefits from extraction: the same mechanism can suppress minority dissent within the constitutional order. The coalition faces minimal suppression and gains political power; extraction runs toward them.
constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PARLIAMENT / REPRESENTATIVE INSTITUTIONS (ROPE) — Institutional actors with structural alternatives. They experience direct democracy as a coordination mechanism that enforces consensus: the threat of referendum forces inclusion of minority preferences to avoid ballot defeat (magic formula logic). This perspective sees the initiative as coordination insurance rather than extraction. When they act as institutional beneficiaries, they experience rope classification; when minoritized by majoritarian initiative, they shift toward snare.
constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL COURT / SUPRANATIONAL REVIEW (PITON) — Swiss courts and European human rights bodies have limited authority to overturn constitutional amendments. Their review function is substantially performative: they can declare the minaret ban in tension with international law but cannot block its domestic implementation. The institutional framework maintains a vestigial rights-review theater while direct democracy has structural supremacy. This is inertial — the review institutions persist but their protection mechanism has been degraded.
constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, this reading risks naturalizing direct democracy as an immutable expression of popular sovereignty: 'the people must have the ultimate power to decide their constitutional order.' This perspective treats the collision between majority power and minority rights as an inherent, unchangeable feature of democratic self-determination. However, structural data contradicts this — the minaret ban represents a *choice* to expose rights to ballot revision, not a law of nature. The false summit detector will flag this classification.
constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swiss_referendum_system__minority_rights_tension_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(swiss_referendum_system__minority_rights_tension, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(swiss_referendum_system__minority_rights_tension, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(swiss_referendum_system__minority_rights_tension, TR),
    TR >= 0.70.

:- end_tests(swiss_referendum_system__minority_rights_tension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The minaret ban demonstrates that direct democracy enables constitutional extraction from minorities — the majority can directly revise rights protections through initiative. However, this is not the highest extractiveness possible because: (1) the mechanism is transparent and formal, not covert; (2) the victim has structural alternatives (relocation, identity suppression, litigation); (3) beneficiary power is contingent on maintaining referendum supermajority, which constrains unlimited extraction. The value reflects real extraction with some structural friction. Suppression (0.72): High. Multiple suppression layers converge: structural (no counter-majoritarian constitutional institution), procedural (100,000 signatures is a low threshold for constitutional amendment), cultural (Swiss consensus norms can be mobilized against minority dissent), legal (minority rights claims face higher evidentiary burden at the ballot than in court). The measurement trajectory shows rising suppression — as direct democracy matures as a mechanism, minority dissent becomes increasingly pre-emptively suppressed in anticipation of unfavorable referendums. Theater ratio (0.35): Relatively low. The minaret ban shows minimal theater — the vote was direct, the outcome implemented mechanically, the will of the majority was translated into constitutional amendment without intermediary interpretation. This is not performative; it is extraction with transparency.
 *
 * PERSPECTIVAL GAP:
 *   The Swiss referendum system illustrates the perspectival distance between majority and minority positions on the same institutional mechanism. To the majoritarian coalition, direct democracy is rope: a coordination mechanism that enables latent preferences to become policy. To the targeted minority, it is snare: a ballot-based extraction mechanism with no institutional appeal. To parliament, it is tangled rope with elements of rope: coordination insurance against being captured by excluded factions, but also a threat to representative autonomy. To the constitutional court, it is piton: the formal right to review exists but lacks power to overturn constitutional amendments. To the analytical observer at civilizational timescale, it risks appearing as mountain: popular sovereignty as an immutable law of democratic systems. The false summit detector will identify this as naturalization. The perspectival gap reveals that the same mechanism is genuinely experienced as coordination by beneficiaries and extraction by victims — not a difference in observation but a difference in structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The minaret ban demonstrates that structural position determines experienced extractiveness. The majority coalition experiences low effective extraction (they control the mechanism and benefit from it); the targeted minority experiences maximum extraction (no exit options, no institutional shelter, no alternative authority). The institutional parliament experiences the referendum as a constraint on their autonomy but also as a coordination mechanism — they benefit from the consensus-forcing effect in non-balloted moments. Directionality (d) varies sharply across perspectives: beneficiary coalition d ≈ 0.20 (low), targeted minority d ≈ 0.92 (very high), parliament d ≈ 0.55 (symmetric). The beneficiary's experience of rope (coordination benefit) versus the victim's experience of snare (extraction) flows from these different d values applied to the same structural constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that direct democracy can function as both coordination mechanism (rope) and extraction mechanism (snare) depending on whether the observer is included in or excluded from the majority. The minaret ban reading privileges the extraction reading: it shows that when direct democracy is used to revise constitutional rights protections, the mechanism becomes a snare for those minoritized at the ballot. However, the sibling consensus-forcing reading (not this story) emphasizes how the referendum threat forces inclusion and consensus, converting potential snare into rope. The mandatrophy is not 'which is correct' but 'which constitutional commitment takes priority: majority sovereignty or minority protection?' This story instantiates one normative reading (majority sovereignty) that generates snare classification. The sibling reading would instantiate another normative reading (constitutional consensus) that generates rope classification. Both are live in Swiss constitutional discourse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counter_majoritarian_shelter_design,
    'Is the absence of counter-majoritarian constitutional shelter a deliberate Swiss design choice or an unintended structural vulnerability?',
    'Historical analysis of Swiss constitutional debates; comparison with other direct-democracy systems (California, Austria, Denmark) and their minority protections; examination of pre-2009 discourse on whether rights were understood to be ballotable',
    'If deliberate design: this reading reflects an authentic choice to prioritize majority sovereignty over minority protection — classification as snare is structural. If unintended: the snare reflects a design flaw correctable by institutional amendment — classification shifts toward tangled_rope with sunset potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_shelter_design, empirical, 'Whether absence of counter-majoritarian shelter is deliberate or accidental').

omega_variable(
    majority_support_stability,
    'Does the majoritarian coalition benefit stably from direct democracy, or does the same mechanism expose them to counter-majoritarian initiative by future majorities?',
    'Time-series analysis of initiative success rates by political direction; simulation of worst-case scenarios where minority voters become swing voters or shift electoral geography; comparison of beneficiary stability across multiple referendum cycles',
    'If stable majority benefit: snare classification is robust — the mechanism systematically favors current coalition. If unstable: the benefit is temporary and the mechanism creates recursive vulnerability — reclassifies toward tangled_rope with cyclical extraction patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_support_stability, empirical, 'Stability of majoritarian coalition benefit across time').

omega_variable(
    direct_democracy_kernel_contestation,
    'Which reading of the direct-democracy kernel — majority sovereignty, consensus forcing, or agenda power — is constitutively privileged in Swiss legitimacy claims?',
    'Textual analysis of Swiss constitutional documents, referendums campaigns, and judicial reasoning; interviews with constitutional scholars and referendum campaigners; examination of which kernel reading appears in justifications for referendum outcomes vs. pre-ballot rhetoric',
    'If majority-sovereignty reading is privileged: snare classification is authenticated by the system''s own legitimacy narrative. If consensus-forcing reading is privileged: the minority-rights-tension reading is a misapplication of the kernel — reclassify toward rope. If agenda-power reading is privileged: the snare reflects who controls the agenda, not inherent system structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_democracy_kernel_contestation, conceptual, 'Which kernel reading is constitutively privileged in Swiss legitimacy claims').

omega_variable(
    suppression_mechanism_structural_vs_cultural,
    'Is suppression of minority dissent a structural feature (the ballot''s direct force) or a cultural feature (Swiss norms of consensus and inclusion)?',
    'Comparative analysis: do minority groups in California face equivalent ballot suppression despite weaker consensus norms? Do minorities in Austria face different suppression trajectories than in Switzerland despite similar direct-democracy structures? Ethnographic study of how minorities experience suppression pre-ballot vs. post-ballot.',
    'If structural: snare classification is robust across governance contexts. If cultural: the minaret ban represents Swiss consensus-culture failure, not direct-democracy necessity — potentially reclassifies toward tangled_rope with stronger cultural-amendment pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cultural, empirical, 'Whether suppression is structural or culturally contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(swiss_referendum_system__minority_rights_tension, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrt_theater_t0, swiss_referendum_system__minority_rights_tension, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mrt_theater_t5, swiss_referendum_system__minority_rights_tension, theater_ratio, 5, 0.32).
narrative_ontology:measurement(mrt_theater_t10, swiss_referendum_system__minority_rights_tension, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(mrt_extract_t0, swiss_referendum_system__minority_rights_tension, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mrt_extract_t5, swiss_referendum_system__minority_rights_tension, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mrt_extract_t10, swiss_referendum_system__minority_rights_tension, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mrt_suppress_t0, swiss_referendum_system__minority_rights_tension, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(mrt_suppress_t5, swiss_referendum_system__minority_rights_tension, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(mrt_suppress_t10, swiss_referendum_system__minority_rights_tension, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(swiss_referendum_system__minority_rights_tension, enforcement_mechanism).
narrative_ontology:affects_constraint(swiss_referendum_system__minority_rights_tension, swiss_referendum_system__consensus_forcing_effect).
narrative_ontology:affects_constraint(swiss_referendum_system__minority_rights_tension, swiss_referendum_system__popular_initiative_engine).

% DUAL FORMULATION NOTE:
% The Swiss referendum system kernel decomposes into three structurally distinct constraints reflecting different readings of the same institutional mechanism. This story (minority_rights_tension, ε=0.58, snare) emphasizes extraction from minorities; the sibling constraint (consensus_forcing_effect, ε~0.30, rope) emphasizes coordination forcing; the third sibling (popular_initiative_engine, ε~0.40, tangled_rope) emphasizes agenda power asymmetry. All three are real structural features of the system; which one dominates depends on the historical moment and the reference majority. The network links reflect that constitutional changes in one reading propagate structural pressure to the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(swiss_referendum_system__minority_rights_tension, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
