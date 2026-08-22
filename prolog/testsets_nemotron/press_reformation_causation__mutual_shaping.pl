% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Printing Press as Mutually Shaping Scaffold for Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The printing press and the Protestant Reformation co-evolved in a
 *   bidirectional feedback loop from ~1450–1650. The press (invented for
 *   Latin humanist and devotional markets) provided the infrastructure that
 *   allowed vernacular dissent to scale; the Reformation's explosive demand
 *   for pamphlets, vernacular Bibles, and polemical literature drove
 *   technical and organizational innovation in printing (smaller formats,
 *   faster turnaround, vernacular typefaces, distribution networks). This
 *   mutual shaping means neither 'the press caused the Reformation' nor
 *   'reformers merely used a neutral tool' captures the structure. The
 *   constraint is a scaffold: a temporary enabling structure (the press as
 *   open communicative infrastructure) that the Reformation reinforced
 *   through use, and which lost its scaffold character once confessional
 *   boundaries hardened and print became a regulated institution in both
 *   Protestant and Catholic territories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.18).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.32).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.18).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press as Mutually Shaping Scaffold for Reformation").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).
narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'ef9edf0d-9695-4104-b3cf-c9e9c500c82a').
narrative_ontology:cs_kernel_codification('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', distributed).
narrative_ontology:cs_authority_grounding('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', practice).
narrative_ontology:cs_interpretation_layer_present('ef9edf0d-9695-4104-b3cf-c9e9c500c82a').
narrative_ontology:cs_reading_relation('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', foundational, technology_and_agency_co_constitute).
narrative_ontology:cs_axiom_status(technology_and_agency_co_constitute, holdable).
narrative_ontology:cs_axiom_grounding('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', technology_and_agency_co_constitute, empirically_contingent).
narrative_ontology:cs_axiom('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', secondary, scaffold_transition_completed_by_1555).
narrative_ontology:cs_axiom_status(scaffold_transition_completed_by_1555, holdable).
narrative_ontology:cs_axiom_grounding('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', scaffold_transition_completed_by_1555, conventional).
narrative_ontology:cs_reference_frame('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', pre_print_manuscript_bottleneck).
narrative_ontology:cs_drift_state('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', post_westphalia_confessional_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ef9edf0d-9695-4104-b3cf-c9e9c500c82a', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, early_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, urban_lay_audiences).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, monastic_scribes).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, latin_scholastic_networks).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, imperial_censorship_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reformers like Luther and Zwingli found the press an unprecedented amplifier for vernacular theology — pamphlets, sermons, and translations spread beyond any single pulpit. They actively shaped print culture by writing for the medium, negotiating with printers, and establishing distribution networks. Their exit was constrained: manuscript circulation was slow and traceable; the press was the only scalable vehicle. They did not control the technology but learned to steer it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, early_reformers, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, early_reformers, agenda_setter).

% Printers in Basel, Strasbourg, Wittenberg, and Geneva found Reformation texts their most reliable bestsellers — high demand, fast turnover, low censorship risk in sympathetic territories. They invested in typefaces for vernacular languages, developed faster workflows for pamphlet production, and built distribution corridors along trade routes. Their exit was mobile: they could relocate to freer cities, and many did. They shaped the press as much as used it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_printers, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, vernacular_printers, agenda_setter).

% Artisans, merchants, and literate laypeople gained direct access to theological argument in their own language — something the Latin mass and clerical monopoly had denied them. This access reshaped religious identity and political consciousness. Their exit was identity-locked: once vernacular scripture and sermon-pamphlets became part of self-understanding, returning to passive Latin obedience was not a live option. They were shaped by the press and became its constituency.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, urban_lay_audiences, beneficiary,
    moderate, generational, identity_locked, local).

% Scriptoria that had controlled book production for centuries lost their economic basis and institutional rationale within decades. Some transitioned to proofreading or type design; most simply disappeared. Their exit was trapped: the skill set was non-transferable at scale, and the monastic vocation itself was dissolved in Reformation territories. They bore the cost of the transition without sharing in its gains.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, monastic_scribes, payer,
    powerless, biographical, trapped, local).

% The pan-European Latin republic of letters — university faculties, cathedral schools, papal curia — lost its monopoly on authorized theological discourse. Vernacular print bypassed their gatekeeping. They responded with Index librorum prohibitorum, censorship decrees, and counter-publications, but the infrastructure had shifted. Their exit was constrained: they retained institutional power in Catholic territories but could not reclaim the communicative center.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, latin_scholastic_networks, payer,
    organized, generational, constrained, continental).

% The Habsburg imperial apparatus and parallel ecclesiastical censorship machinery (Frankfurt book fair inspections, imperial mandates, papal bulls) found their pre-print control model — pre-publication licensing of manuscripts — structurally inadequate for the speed and volume of pamphlet print. They escalated to post-publication suppression, printer licensing, and trade restrictions, but the constraint had already mutated. Their exit was constrained: they adapted the machinery but never regained the pre-print equilibrium.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, imperial_censorship_regimes, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, imperial_censorship_regimes, agenda_setter).

% Sees the bidirectional feedback loop: the press lowered the cost of dissent, which created demand for more and faster printing, which drove technical improvements (smaller formats, faster presses, vernacular type), which further lowered the cost of dissent. Neither pure cause nor pure effect — a scaffold that both enabled and was reinforced by the movement it carried.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved the coordination problem of scaling dissent: before print, a reformer's reach was bounded by voice and manuscript; the press created a standardized, replicable, distributable medium that allowed theological argument to coordinate across cities and principalities without physical presence.
% TRANSFER_FUNCTION: The arrangement moved authoritative theological interpretation from Latin clerical monopoly to vernacular lay access; moved economic rents from scriptoria to commercial printers; moved political legitimacy from imperial/universal structures to territorial/confessional ones.
% ABSENT_VOICES: Peasant communities in non-urban areas — the press reached them indirectly through oral re-publication (sermons, readings aloud), but they had no direct access to production or distribution. Women were largely excluded from the printer-reformer networks, though some (Argula von Grumbach, Katharine von Bora) entered the print record. Anabaptist and radical reformers were often suppressed by magisterial reformers using the same print infrastructure.
% DISAPPEARANCE_RATIONALE: If the press-reformation scaffold vanished overnight, the Reformation as a mass movement would not have occurred — dissent would have remained local, suppressible, and Latin-bound. The press did not cause the Reformation in a deterministic sense, but it was the enabling structure without which the specific historical Reformation (vernacular, mass, territorially organized) is inconceivable. Conversely, without the Reformation's demand, the press would have developed differently — more Latin humanist editions, fewer vernacular pamphlets, slower technical iteration.
% FOUNDING_PROBLEM: The founding problem was the communicative bottleneck of manuscript culture: theological dissent could not scale because reproduction was slow, expensive, traceable, and Latin-gated. The press was not invented for the Reformation (Gutenberg printed indulgences and Latin bibles), but the Reformation discovered the press as the solution to its scaling problem — and then reshaped the press to serve that solution.
% FOUNDING_PROBLEM_CORROBORATION: Eisenstein (1979) and Febvre & Martin (1958) document the pre-print communicative bottleneck from the side of book history; Oberman (1981) and Brecht (1993) confirm from the Reformation side that Luther himself treated the press as the solution to a communicative problem. No serious historian of either field maintains the bottleneck was illusory — the founding problem is corroborated outside the benefiting parties.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the primary dynamic is coordination gain, not rent extraction — though censorship regimes and displaced scribes bear real costs. Suppression (0.32) reflects that the constraint required active enforcement (censorship, printer licensing, Index) to maintain the old order, but the new order also developed its own suppression (orthodoxy enforcement). Theater ratio (0.25) captures the performative dimension: the 'freedom of the press' rhetoric coexisted with confessional press control. Accessibility collapse (0.45) is moderate — manuscript culture did not vanish instantly; Latin remained the language of scholarship for centuries. Resistance (0.38) reflects real but ultimately unsuccessful pushback from the old communicative order. The measurement series shows the characteristic scaffold arc: extraction and suppression peak during the transition (1520s–1540s) then decline as the new order stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer-printer seat, the press looks like a rope — a coordination mechanism they built and used. From the scribe-censor seat, it looks like a snare — an exogenous shock that destroyed their position. From the analyst seat, it is a scaffold — a temporary structure that enabled a transition and then faded into the background infrastructure of early modern Europe. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Early reformers and vernacular printers are structural beneficiaries (d ~ 0.2–0.3) — they gained reach, revenue, and agency from the scaffold. Urban lay audiences are beneficiaries with identity-locked exit (d ~ 0.25) — they gained access but cannot return. Monastic scribes are trapped victims (d ~ 0.9) — total displacement. Latin scholastic networks and censorship regimes are constrained victims (d ~ 0.7) — they lost monopoly control but retained institutional power in Catholic zones. The historical analyst sees the full bidirectional structure (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold's mandate (enabling communicative scaling for dissent) was fulfilled by ~1555 (Peace of Augsburg) — confessional boundaries were set, print was institutionalized, the transition complete. The constraint persisted as regulation (censorship, licensing) but the scaffold function was dead. The founding problem (manuscript bottleneck) is dead; the arrangement that solved it (open vernacular print) was replaced by confessional print regimes. This is a clean scaffold-to-institution transition, not mandatrophy — the mandate was resolved, not outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_as_scaffold_vs_mountain,
    'Is the printing press''s communicative affordance a contingent historical scaffold (emerging from specific technical/economic conditions) or a structural mountain (an inevitable phase shift in information technology)?',
    'Counterfactual analysis: if Gutenberg had failed, would another press have emerged within 50 years? Comparative: Chinese movable type (11th c.) did not trigger comparable scaling — why? The answer determines whether the scaffold is historically contingent or structurally necessary.',
    'If mountain, the Reformation''s communicative scaling was inevitable; the press is a natural law of information physics. If scaffold, the specific mutual shaping is a historical achievement, not a given — and could have failed or taken radically different form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_as_scaffold_vs_mountain, conceptual, 'Natural-law vs. contingent-historical status of the press''s communicative affordance.').

omega_variable(
    reformation_shaped_press_or_press_shaped_reformation,
    'In the bidirectional loop, which direction carries more structural weight — did the Reformation shape the press''s trajectory more than the press shaped the Reformation''s possibility?',
    'Measure technical innovation rates in printing before/after 1517 in Reformation vs. non-Reformation territories; measure pamphlet/vernacular share of output. If Reformation territories show accelerated innovation and vernacularization, the shaping is bidirectional with Reformation->press as a strong vector.',
    'If press->Reformation dominates, the scaffold is press-led (closer to determinism). If Reformation->press dominates, the scaffold is movement-led (closer to strategic deployment). If balanced, mutual_shaping is structurally distinct from both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformation_shaped_press_or_press_shaped_reformation, empirical, 'Asymmetry in the bidirectional causation between press and Reformation.').

omega_variable(
    kernel_framing_ambiguity,
    'Does the kernel ''press_reformation_causation'' name a single constraint with multiple readings, or a family of distinct constraints (press affordance, Reformation communicative need, censorship response) that the label conflates?',
    'Apply ε-invariance test: would measuring ''press caused Reformation'' via pamphlet volume, via censorship failure rate, and via vernacular Bible penetration yield the same ε? If not, decompose into separate constraint stories.',
    'If the kernel is a conflation, the sibling readings are not readings of one constraint but distinct constraints that should be authored separately and linked via network.affects_constraints. This story assumes a single kernel with readings; the omega flags the decomposition question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel itself is a coherent unit or a category error conflating multiple constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.1).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__mutual_shaping, theater_ratio, 1480, 0.15).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__mutual_shaping, theater_ratio, 1517, 0.2).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__mutual_shaping, theater_ratio, 1530, 0.3).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causation__mutual_shaping, theater_ratio, 1555, 0.25).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__mutual_shaping, theater_ratio, 1600, 0.22).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__mutual_shaping, theater_ratio, 1650, 0.25).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__mutual_shaping, base_extractiveness, 1480, 0.12).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__mutual_shaping, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__mutual_shaping, base_extractiveness, 1530, 0.22).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causation__mutual_shaping, base_extractiveness, 1555, 0.18).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.16).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__mutual_shaping, base_extractiveness, 1650, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__mutual_shaping, suppression_requirement, 1450, 0.15).
narrative_ontology:measurement(pres_su_t1480, press_reformation_causation__mutual_shaping, suppression_requirement, 1480, 0.2).
narrative_ontology:measurement(pres_su_t1517, press_reformation_causation__mutual_shaping, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__mutual_shaping, suppression_requirement, 1530, 0.45).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causation__mutual_shaping, suppression_requirement, 1555, 0.4).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__mutual_shaping, suppression_requirement, 1600, 0.35).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causation__mutual_shaping, suppression_requirement, 1650, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__mutual_shaping, 0.03).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, vernacular_print_standardization).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, confessional_censorship_regimes).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, early_modern_public_sphere_formation).

% DUAL FORMULATION NOTE:
% This constraint family (press_reformation_causation) decomposes the single historiographical label into three structurally distinct constraints: mutual_shaping (scaffold, bidirectional), technological_determinism (mountain-claimed, press-as-inevitable-cause), strategic_deployment (rope-claimed, press-as-neutral-tool). The ε values differ: mutual_shaping ε=0.18 (coordination gain with transition costs), determinism ε≈0.05 (press as natural law), strategic_deployment ε≈0.25 (tool use with strategic capture). They are linked because determinism and strategic_deployment are often cited as evidence for/against mutual_shaping in historiography.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__mutual_shaping, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
