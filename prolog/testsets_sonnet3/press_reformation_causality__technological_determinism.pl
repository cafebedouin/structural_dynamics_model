% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: The Printing Press as Autonomous Enabling Technology (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological-determinism reading of the
 *   press-Reformation kernel: the claim that the printing press functioned as
 *   an autonomous enabling technology whose inherent properties made
 *   vernacular scripture distribution and Reformation success effectively
 *   inevitable, with human actors (reformers, printers, rulers) as downstream
 *   responders to a technological unfolding rather than co-authors of a
 *   contingent outcome. This is presented AS A MOUNTAIN CLAIM — the reading
 *   treats the press's causal power as a fixed, near-physical constraint —
 *   while the authored metrics reflect what I believe is descriptively true:
 *   the determinist account is a constructed interpretive frame that obscures
 *   real beneficiaries (print-centered historiography, reformers whose agency
 *   it launders, printers whose commercial strategy it naturalizes) and
 *   produces rising theater_ratio and extractiveness over the interval as the
 *   frame hardens into textbook orthodoxy despite mounting counter-evidence
 *   from regional political variation. The claim/metric divergence IS the
 *   finding: a mountain claim computing as extractive is exactly the
 *   false-summit signature this corpus exists to detect.
 *
 * KEY AGENTS:
 *   - printing_press_operators: beneficiary (moderate/mobile) — commercial strategy laundered into technological inevitability
 *   - protestant_reformers: beneficiary (organized/mobile) — political agency and rhetorical strategy erased by determinist framing
 *   - modernization_historiography_school: beneficiary (institutional/analytical) — interpretive tradition vindicated by the reading's persistence
 *   - catholic_church_authorities: payer (institutional/constrained) — real, sometimes effective countermeasures rendered historically irrelevant
 *   - manuscript_scribes_and_illuminators: excluded (powerless/trapped) — labor displacement invisible in a natural-succession narrative
 *   - illiterate_and_non_vernacular_populations: excluded (powerless/trapped) — majority population for whom the press's 'universal' effect did not apply
 *   - regional_political_rulers: excluded (powerful/constrained) — decisive political agency subsumed into technological trend
 *   - comparative_historians_of_technology: observer (analytical) — the seat that tests the inevitability claim against counterfactual cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.58).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.42).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "The Printing Press as Autonomous Enabling Technology (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, 'e988650d-ebae-4c31-a57a-ef9e39352ce5').
narrative_ontology:cs_kernel_codification('e988650d-ebae-4c31-a57a-ef9e39352ce5', distributed).
narrative_ontology:cs_authority_grounding('e988650d-ebae-4c31-a57a-ef9e39352ce5', distributed).
narrative_ontology:cs_reading_relation('e988650d-ebae-4c31-a57a-ef9e39352ce5', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('e988650d-ebae-4c31-a57a-ef9e39352ce5', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('e988650d-ebae-4c31-a57a-ef9e39352ce5', foundational, technology_possesses_autonomous_causal_agency).
narrative_ontology:cs_axiom_status(technology_possesses_autonomous_causal_agency, holdable).
narrative_ontology:cs_axiom_grounding('e988650d-ebae-4c31-a57a-ef9e39352ce5', technology_possesses_autonomous_causal_agency, empirically_contingent).
narrative_ontology:cs_axiom('e988650d-ebae-4c31-a57a-ef9e39352ce5', secondary, human_strategic_choice_is_epiphenomenal_to_technological_capacity).
narrative_ontology:cs_axiom_status(human_strategic_choice_is_epiphenomenal_to_technological_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e988650d-ebae-4c31-a57a-ef9e39352ce5', human_strategic_choice_is_epiphenomenal_to_technological_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('e988650d-ebae-4c31-a57a-ef9e39352ce5', print_revolution_paradigm_shift).
narrative_ontology:cs_drift_state('e988650d-ebae-4c31-a57a-ef9e39352ce5', post_regional_variation_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e988650d-ebae-4c31-a57a-ef9e39352ce5', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, printing_press_operators).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, modernization_historiography_school).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, catholic_church_authorities).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_autonomy_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, print_capitalism_inevitability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owned and operated the presses that mass-produced vernacular scripture and reform pamphlets. Under this reading, their commercial choices, print-run decisions, and city-to-city business strategies disappear into the technology's 'inherent' capacity to spread text; the press itself is credited as the causal engine, which retroactively naturalizes their profits as a byproduct of physics-like diffusion rather than deliberate commercial strategy.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, printing_press_operators, beneficiary,
    moderate, biographical, mobile, continental).

% Used pamphlets, translated Bibles, and woodcuts to build a movement. This reading frames their success as the press 'enabling' an outcome that was going to happen regardless of their rhetorical choices, alliance-building, or timing decisions — collapsing decades of contingent political maneuvering into a technological unfolding they merely rode.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% A scholarly tradition (Eisenstein-lineage print-revolution historiography) whose explanatory framework depends on treating print as a self-acting historical force. This reading vindicates that tradition's core interpretive commitments and its place in survey curricula; it has an intellectual stake in the determinist account remaining the default explanation taught to students.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, modernization_historiography_school, beneficiary,
    institutional, civilizational, analytical, global).

% Attempted censorship, indices of prohibited books, and licensing controls to slow vernacular scripture distribution. Under this reading their countermeasures are rendered structurally futile in advance — resistance to an 'inevitable' technology is framed as historically irrelevant, which erases the real periods and regions where suppression measurably slowed or redirected print circulation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, catholic_church_authorities, payer,
    institutional, generational, constrained, continental).

% Lost livelihoods as print displaced manuscript production. Their labor-market displacement is invisible in a determinist frame that treats the transition as a natural technological succession rather than a contested economic reallocation with identifiable losers who had no voice in adopting the new technology.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, manuscript_scribes_and_illuminators, excluded,
    powerless, biographical, trapped, regional).

% The majority of the European population who could not read and for whom vernacular print did not autonomously transform religious access; oral transmission, local clergy, and communal practice mediated their experience of the Reformation far more than presses did. Their near-total absence from print's reach is unaddressed by a story in which the press is credited as the universal solvent.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, illiterate_and_non_vernacular_populations, excluded,
    powerless, biographical, trapped, regional).

% Princes and city councils whose decisions to protect, expel, or license printers and reformers were often decisive for whether print-driven reform succeeded locally (e.g., Saxony vs. regions where rulers suppressed reform successfully). Their agency is structurally absent from a technological-determinist account, which treats political variation as noise around an inevitable trend line.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, regional_political_rulers, excluded,
    powerful, generational, constrained, regional).

% Study counterfactual cases (regions with presses but no successful reform; regions with weak printing infrastructure that reformed anyway) to test whether the press's causal contribution was necessary, sufficient, or merely facilitating. Their comparative work is the primary check on this reading's inevitability claim.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, comparative_historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the printing press coordinates nothing socially — it is treated as a physical/technical capacity (rapid, cheap, accurate text replication) that removes a bottleneck. The 'coordination' is between the technology's inherent properties and an outcome it makes possible, not between human parties negotiating anything.
% TRANSFER_FUNCTION: Explanatory credit and causal weight are transferred FROM the deliberate choices of reformers, printers, patrons, and rulers TO the technology itself. This is a transfer of historical agency and interpretive authority, not a material transfer — it moves credit for the Reformation's success away from contingent human decision-making and into an autonomous technological process.
% ABSENT_VOICES: Regional rulers whose political choices were often decisive, manuscript workers displaced by the print economy, and the large illiterate/non-vernacular population for whom the press was largely irrelevant are all structurally written out of an account organized around the press's autonomous causal power. Historians who emphasize contingency (failed reform movements in well-printed regions, successful reform in poorly-printed ones) would object strongly if consulted.
% DISAPPEARANCE_RATIONALE: If this READING (not the press itself) disappeared from historiography, the standard undergraduate narrative of 'print caused the Reformation' would need replacement with more contingent, multi-causal accounts already present in specialist literature (Pettegree, Scribner) — a real rearrangement of how the period is taught and popularly understood, though the underlying historical events would of course be unchanged. Whether this counts as the world rearranging or merely the interpretation correcting itself is exactly the dispute between this reading and its co-constitution/strategic-deployment siblings.
% FOUNDING_PROBLEM: The reading was built to explain why the Reformation succeeded where earlier reform movements (Hussites, Wycliffites) failed despite similar theological content — the answer offered was: this time, the technology for mass, rapid, cheap textual replication existed and did the causal work earlier movements lacked.
% FOUNDING_PROBLEM_CORROBORATION: Elizabeth Eisenstein's print-revolution thesis and its historiographical descendants attest the founding problem as still live and correctly solved by the determinist frame. Outside that tradition, historians of the Reformation's regional political variation (e.g., scholarship emphasizing princely protection, urban council politics, and the documented failure of print-rich regions to reform) attest that the founding problem was real but the determinist answer overstates its solution — the press was necessary-but-radically-insufficient, and treating it as sufficient serves the explanatory economy of print-centered historiography more than it serves the evidence.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater_ratio (0.61) are authored moderately high because the determinist reading, once established as historiographical default, performs significant EXPLANATORY WORK beyond its evidentiary support: it standardizes into textbooks, absolves specific human actors and institutions of contingent responsibility, and forecloses inquiry into the regional political variation that better explains uneven reform success. Accessibility_collapse is high (0.7) because once a student or reader accepts 'the press made it happen,' alternative causal accounts (strategic deployment, co-constitution) become harder to notice as live options — the naturalizing frame is sticky. Resistance is moderate-low (0.35) because comparative historians and regional-political-variation scholars do actively contest the frame, but it remains institutionally dominant in general education. The rising temporal trajectory reflects the reading's historiographical entrenchment from Eisenstein's mid-20th-century formulation forward into a default popular and pedagogical narrative, not a claim about the 15th-17th century events themselves.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical/observer seat (comparative historians), this reading computes as a constructed interpretive convenience — a tangled_rope-like hybrid that genuinely coordinates a simplified pedagogical narrative (real explanatory value: it does capture something true about scale and speed) while extracting interpretive authority from excluded political and economic actors. From the beneficiary seats (the historiographical tradition itself, reformers whose success it naturalizes), it presents as simply true, a mountain — describing physical facts about print technology's capacity, not a contestable interpretive choice. The gap between these seats is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (printing_press_operators, protestant_reformers, modernization_historiography_school) receive low derived directionality because the reading launders their contingent choices into technological inevitability, insulating them from counterfactual scrutiny — they are subsidized by the frame's explanatory generosity. catholic_church_authorities sit as payers because their real, sometimes locally effective resistance is retroactively rendered futile by the determinist frame regardless of its actual historical efficacy. Excluded seats (manuscript workers, illiterate populations, regional rulers) carry high implicit cost but are structurally outside the narrative's accounting entirely — their absence is itself a directionality signal: agents erased from a causal story cannot register as either beneficiary or victim within it, which is precisely the erasure this reading's excluded-voices question is meant to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining Reformation success where prior reform movements failed — was live and worth solving in the 20th century when Eisenstein's thesis was formulated against a historiography that had underweighted media technology entirely. Whether that problem remains 'live' in its determinist form is contested: the corrective function (taking print seriously as a causal factor) succeeded and arguably no longer needs the strong inevitability claim to do its work — a weaker 'necessary but insufficient' claim would preserve the corrective without the erasure. The determinist reading's persistence past the point its corrective function was achieved, sustained by textbook inertia and the interpretive tradition's institutional stake, is a candidate case of the reading itself becoming vestigial relative to more contingent multi-causal accounts specialists already favor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_as_mountain_or_constructed_frame,
    'Is treating the printing press''s Reformation-enabling capacity as an autonomous, near-physical constraint (mountain) a genuine natural-law-like claim about technological diffusion, or a constructed historiographical frame that happens to benefit specific scholarly and institutional actors?',
    'Comparative analysis of regions with comparable print infrastructure but divergent reform outcomes (e.g., well-printed regions that did not reform vs. reform-successful regions with limited local printing capacity). If print capacity correlates weakly with reform success once political and social variables are controlled, the mountain framing is undermined.',
    'If the press''s causal role is shown to be highly contingent on political protection, market structure, and reformer strategy, this reading reclassifies from mountain toward tangled_rope or snare (a coordinating narrative with obscured beneficiaries) rather than a genuine structural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_as_mountain_or_constructed_frame, empirical, 'Whether the determinist frame describes a physical fact or launders a beneficiary structure as natural law — required omega for the FSM (false-summit-mountain) candidacy of this story.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (technological_determinism, strategic_deployment, co_constitution) locate their disagreement — is it about the DIRECTION of causality (tech-to-outcome vs. actor-to-outcome), the UNIT of agency (technology vs. individual strategists vs. feedback systems), or the EXPLANATORY SUFFICIENCY of any single factor?',
    'Structural comparison of the three constraint files'' beneficiary/victim declarations and claimed_type assignments — determinism claims mountain, strategic_deployment likely claims rope or tangled_rope (deliberate coordination), co_constitution likely claims tangled_rope or scaffold (transitional feedback system).',
    'If the disagreement is purely about explanatory sufficiency (all three agree multiple factors mattered, disagree on weighting), the readings are closer to coexists_with across the board. If the disagreement is about the fundamental unit of causal agency (autonomous technology vs. strategic human actors), technological_determinism and strategic_deployment may be closer to a foreclosing relationship at the level of ''what kind of thing caused the Reformation.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Committer-frame ambiguity: the specific structural element the three sibling readings differ on.').

omega_variable(
    excluded_populations_scope_bias,
    'Does the determinist reading''s apparent global/continental scope claim actually rest on evidence disproportionately drawn from urban, literate, vernacular-speaking populations, silently excluding the rural and illiterate majority for whom the press''s effect was negligible or mediated entirely through oral/clerical channels?',
    'Literacy-rate-weighted analysis of the population actually reached by vernacular print in the relevant period (estimates generally place European literacy well under 30% through much of the 16th century) against claims of ''Reformation success'' at the population level.',
    'If the reading''s inevitability claim rests on evidence from a literate minority projected onto whole populations, its effective spatial_scope claim (continental/global) is overstated relative to its actual evidentiary base, inflating computed extraction via the scope amplification the engine applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_populations_scope_bias, empirical, 'Scope-inflation risk: whether continental-scope claims are supported by literate-population-only evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__technological_determinism, theater_ratio, 1450, 0.35).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causality__technological_determinism, theater_ratio, 1490, 0.42).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__technological_determinism, theater_ratio, 1517, 0.48).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.55).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__technological_determinism, theater_ratio, 1600, 0.58).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__technological_determinism, theater_ratio, 1650, 0.61).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__technological_determinism, base_extractiveness, 1450, 0.3).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causality__technological_determinism, base_extractiveness, 1490, 0.38).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__technological_determinism, base_extractiveness, 1517, 0.46).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.52).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__technological_determinism, base_extractiveness, 1600, 0.56).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__technological_determinism, base_extractiveness, 1650, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, identity_coordination).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__technological_determinism, 0.1).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the press_reformation_causality kernel. technological_determinism (this file) authors the press as an autonomous mountain-like constraint with human actors as downstream responders and beneficiary structure obscured. strategic_deployment authors reformers and printers as deliberate strategic agents (likely rope or tangled_rope, with clear agenda-setters). co_constitution authors a feedback-loop structure between print economy and religious controversy (likely tangled_rope or scaffold, with mutual causation). Each reading has its own ε, beneficiary/victim structure, and claimed_type per the ε-invariance principle — they are not the same constraint measured three ways but three structurally distinct constraints sharing a contested kernel. The disagreement locus is documented in the kernel_reading_disagreement_locus omega above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
