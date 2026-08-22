% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Deployment of the Printing Press by Reformers and Printers
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'strategic deployment' reading of
 *   the press-Reformation causation kernel. It treats the printing press as a
 *   neutral coordination technology — a rope — that reformers and printers
 *   deliberately exploited to bypass Catholic communicative control and
 *   extract doctrinal and commercial gains. The press did not 'cause' the
 *   Reformation in a deterministic sense; rather, it provided the
 *   infrastructure that made continental dissemination possible. Reformers
 *   (Luther, Melanchthon, Zwingli) and commercial printers (Froben, Lotter,
 *   Rhau) are the primary beneficiaries. Catholic authorities and imperial
 *   censors bear the costs of lost control and reactive enforcement. Lay
 *   readers benefit asymmetrically — access expands but remains constrained
 *   by literacy and cost. The kernel contest has three readings: this one
 *   (strategic_deployment), mutual_shaping (co-evolution), and
 *   technological_determinism (press as inevitable cause).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.18).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.12).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.18).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Deployment of the Printing Press by Reformers and Printers").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '6cd3feec-b02d-47e2-a65d-8d70b022428d').
narrative_ontology:cs_kernel_codification('6cd3feec-b02d-47e2-a65d-8d70b022428d', distributed).
narrative_ontology:cs_authority_grounding('6cd3feec-b02d-47e2-a65d-8d70b022428d', distributed).
narrative_ontology:cs_reading_relation('6cd3feec-b02d-47e2-a65d-8d70b022428d', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_reading_relation('6cd3feec-b02d-47e2-a65d-8d70b022428d', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_axiom('6cd3feec-b02d-47e2-a65d-8d70b022428d', foundational, technology_neutral_capacity).
narrative_ontology:cs_axiom_status(technology_neutral_capacity, holdable).
narrative_ontology:cs_axiom_grounding('6cd3feec-b02d-47e2-a65d-8d70b022428d', technology_neutral_capacity, conventional).
narrative_ontology:cs_axiom('6cd3feec-b02d-47e2-a65d-8d70b022428d', foundational, agency_upstream_driver).
narrative_ontology:cs_axiom_status(agency_upstream_driver, holdable).
narrative_ontology:cs_axiom_grounding('6cd3feec-b02d-47e2-a65d-8d70b022428d', agency_upstream_driver, conventional).
narrative_ontology:cs_reference_frame('6cd3feec-b02d-47e2-a65d-8d70b022428d', pre_print_communicative_monopoly).
narrative_ontology:cs_drift_state('6cd3feec-b02d-47e2-a65d-8d70b022428d', post_peace_of_augsburg, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6cd3feec-b02d-47e2-a65d-8d70b022428d', '2026-08-15T10:30:00Z').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, commercial_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, vernacular_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, lay_readers).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_authorities).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, press_as_neutral_tool).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, agency_as_upstream_driver).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used the press to bypass ecclesiastical censorship, distribute vernacular translations, and coordinate theological dissent across German territories. The press amplified their message exponentially; without it their reach would have remained local. They gained doctrinal dissemination and institutional leverage but did not control the technology itself.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).

% Produced and distributed Reformation pamphlets, Bibles, and polemics at scale. Profited from massive demand for vernacular texts. Their business model depended on the Reformation's momentum; they invested in typefaces, distribution networks, and rapid turnaround. If the Reformation collapsed, their specialized inventory and market position would degrade.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, commercial_printers, beneficiary,
    moderate, biographical, constrained, regional).

% Specialized in German-language religious and instructional texts. The press created a new market for vernacular literacy materials. They benefited from the coordination function — standardized editions replacing manuscript variation — and extracted profit from the resulting demand surge.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, vernacular_publishers, beneficiary,
    moderate, biographical, mobile, regional).

% Lost monopoly over religious communication and doctrinal interpretation. The press enabled uncontrollable dissemination of heterodox texts. They responded with the Index, censorship machinery, and Counter-Reformation printing — reactive enforcement that acknowledged the press as a threat. Their structural position shifted from gatekeepers to pursuers.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_authorities, payer,
    institutional, generational, constrained, continental).

% Attempted to regulate printing through imperial mandates (e.g., 1521 Edict of Worms, 1529 Diet of Speyer). Enforcement was patchy across the fragmented Holy Roman Empire; printers relocated to tolerant territories. The constraint they administered (imperial censorship) was structurally porous — the press as a coordination tool for reformers operated in the gaps.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, imperial_censors, agenda_setter,
    institutional, biographical, constrained, national).

% Gained access to vernacular scripture, catechisms, and polemical literature. Literacy spread unevenly; the press made texts cheaper but not universally affordable. Their agency was enabled by the coordination function — standardized, widely available texts — but they did not direct the deployment.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, lay_readers, beneficiary,
    powerless, biographical, constrained, local).

% Analyze the press-Reformation relationship through the strategic deployment lens: technology as neutral capacity, agency as upstream driver. This reading treats the press as a rope — a coordination tool that reformers and printers deliberately exploited for doctrinal and commercial gain.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historians_of_technology, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved a genuine collective-action problem for Reformation actors: how to disseminate a coherent theological message across linguistic and political fragmentation without relying on ecclesiastical channels. It standardized texts, enabled mass distribution, and created a communication infrastructure that no single actor could build alone.
% TRANSFER_FUNCTION: Moves doctrinal authority and interpretive control from Catholic ecclesiastical hierarchy to reformers and vernacular publishers. Moves commercial profit from manuscript production to print shops. Moves literacy access from clerical mediation to direct lay engagement (unevenly).
% ABSENT_VOICES: Peasant and urban poor populations who could not read or afford printed materials — their experience of the Reformation was mediated through oral preaching and communal reading, not direct press access. Anabaptist and radical reformers who used the press but were suppressed by both Catholic and magisterial Protestant authorities.
% DISAPPEARANCE_RATIONALE: If the printing press had not existed or had been suppressed in 1517, the Reformation would likely have remained a localized academic dispute. The coordination function — mass dissemination of vernacular theology — would collapse. Catholic authorities would retain communicative monopoly. Commercial printers would revert to manuscript and Latin markets. The world rearranges because the press was the infrastructure that made the Reformation a continental movement.
% FOUNDING_PROBLEM: The Catholic Church's communicative monopoly over religious doctrine, enforced through manuscript culture, Latin liturgy, and episcopal censorship, prevented theological dissent from spreading beyond local circles. Reformers needed a way to bypass these gates.
% FOUNDING_PROBLEM_CORROBORATION: Eisenstein (1979) and Febvre & Martin (1958) document the communicative monopoly's collapse. Modern historians (Pettegree, 2014; Edwards, 2004) corroborate that the press solved a real dissemination problem for reformers — but also note the founding problem (ecclesiastical communicative monopoly) was already eroding through pre-print networks. The status 'dead' reflects that the specific monopoly the press broke no longer exists in any form.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.18) because the press's operation as a coordination tool generated genuine shared benefits — standardized texts, wider literacy, cheaper books — alongside the targeted gains of reformers and printers. Suppression (0.12) is low because the constraint (the press as deployed) did not require active enforcement to persist; its adoption was voluntary and self-reinforcing. Theater ratio (0.08) is minimal — the coordination function was real and not performative. Accessibility collapse (0.25) is moderate: alternatives (manuscript circulation, oral preaching) persisted but were marginalized. Resistance (0.35) reflects Catholic censorship efforts, which were real but structurally ineffective at scale. The claimed type 'rope' fits: genuine coordination problem solved, net beneficiaries, no suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer/printer seats, the press is a rope — a tool they chose and deployed for gain. From the Catholic authority seat, the same press operates as a snare — an uncontrollable force extracting their communicative monopoly. From the lay reader seat, it is a scaffold — temporary access expansion that depended on literacy they did not control. The engine computes these per-seat classifications from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are structural beneficiaries (d ~ 0.1-0.2): they gained doctrinal reach and profit without bearing the costs of the old system's collapse. Catholic authorities are payers (d ~ 0.7-0.8): they lost communicative monopoly and invested heavily in reactive enforcement (Index, Inquisition, Counter-Reformation printing). Imperial censors are agenda_setters with constrained exit (d ~ 0.5): they administered a failing constraint. Lay readers are beneficiaries with constrained exit (d ~ 0.3): genuine access gains but limited agency. Historians are analytical observers (d = 0.5). The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ecclesiastical communicative monopoly) is dead — the monopoly no longer exists in any form. Yet the press as a coordination infrastructure persists and has been repurposed countless times. This is not mandatrophy in the extractive sense (the constraint did not persist by extracting after its function vanished); rather, the coordination function proved generalizable beyond its founding problem. The rope classification captures this: a genuine coordination tool that outlived its original context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_neutrality_vs_affordance,
    'Was the printing press truly a neutral tool, or did its material affordances (standardization, fixity, speed, scale) structurally favor certain messages and actors regardless of user intent?',
    'Compare diffusion patterns of Reformation texts vs. Catholic texts in the same presses and regions. If Catholic authorities could have used the press equally effectively but chose not to, neutrality is supported. If material constraints (e.g., pamphlet format favoring polemic over systematic theology) shaped the message regardless of intent, affordance structure is supported.',
    'If the press has inherent affordances that favor fragmentation/polemic over unity/system, the ''neutral tool'' claim weakens and the mutual_shaping or technological_determinism readings gain structural ground. The strategic_deployment reading''s ε would need to account for affordance-driven extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_neutrality_vs_affordance, conceptual, 'Whether the press''s material properties constrain what can be communicated, independent of user intent.').

omega_variable(
    printer_agency_vs_market_forcing,
    'Did printers strategically choose Reformation content, or were they forced by market demand (Reformation texts sold; Catholic texts did not)?',
    'Examine printer correspondence, financial records, and output mix in mixed-confession cities (e.g., Strasbourg, Augsburg, Basel). Did printers print both sides? Did they switch confessional output based on local politics or purely demand?',
    'If printers were market-forced, their beneficiary status is more passive — they captured demand they did not create. If they actively curated Reformation output (e.g., investing in Luther''s German Bible before demand was proven), the strategic_deployment reading''s agency claim is stronger. Affects whether ''commercial_printers'' are beneficiaries or constrained payers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(printer_agency_vs_market_forcing, empirical, 'Whether printer participation was strategic agency or market compulsion.').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the strategic_deployment reading logically foreclose the technological_determinism reading within a single explanatory framework, or do they coexist as different levels of analysis?',
    'Formalize the causal claims: ''Press caused Reformation'' (determinism) vs. ''Reformers used press to cause Reformation'' (deployment). Test whether a single model can assign causal weights to both technology and agency without contradiction (e.g., necessary condition vs. sufficient condition).',
    'If they foreclose, the kernel has a genuine logical split — only one reading can be structurally true. If they coexist, the kernel represents a perspectival difference (upstream vs. downstream causation) not a structural one. Determines reading_relations assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Structural relationship between strategic_deployment and technological_determinism readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_strategic_tr_t1517, press_reformation_causation__strategic_deployment, theater_ratio, 1517, 0.02).
narrative_ontology:measurement(press_reformation_strategic_tr_t1521, press_reformation_causation__strategic_deployment, theater_ratio, 1521, 0.04).
narrative_ontology:measurement(press_reformation_strategic_tr_t1525, press_reformation_causation__strategic_deployment, theater_ratio, 1525, 0.06).
narrative_ontology:measurement(press_reformation_strategic_tr_t1530, press_reformation_causation__strategic_deployment, theater_ratio, 1530, 0.07).
narrative_ontology:measurement(press_reformation_strategic_tr_t1540, press_reformation_causation__strategic_deployment, theater_ratio, 1540, 0.08).
narrative_ontology:measurement(press_reformation_strategic_tr_t1555, press_reformation_causation__strategic_deployment, theater_ratio, 1555, 0.08).

% Extraction over time
narrative_ontology:measurement(press_reformation_strategic_be_t1517, press_reformation_causation__strategic_deployment, base_extractiveness, 1517, 0.05).
narrative_ontology:measurement(press_reformation_strategic_be_t1521, press_reformation_causation__strategic_deployment, base_extractiveness, 1521, 0.08).
narrative_ontology:measurement(press_reformation_strategic_be_t1525, press_reformation_causation__strategic_deployment, base_extractiveness, 1525, 0.12).
narrative_ontology:measurement(press_reformation_strategic_be_t1530, press_reformation_causation__strategic_deployment, base_extractiveness, 1530, 0.15).
narrative_ontology:measurement(press_reformation_strategic_be_t1540, press_reformation_causation__strategic_deployment, base_extractiveness, 1540, 0.17).
narrative_ontology:measurement(press_reformation_strategic_be_t1555, press_reformation_causation__strategic_deployment, base_extractiveness, 1555, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_strategic_su_t1517, press_reformation_causation__strategic_deployment, suppression_requirement, 1517, 0.05).
narrative_ontology:measurement(press_reformation_strategic_su_t1521, press_reformation_causation__strategic_deployment, suppression_requirement, 1521, 0.08).
narrative_ontology:measurement(press_reformation_strategic_su_t1525, press_reformation_causation__strategic_deployment, suppression_requirement, 1525, 0.1).
narrative_ontology:measurement(press_reformation_strategic_su_t1530, press_reformation_causation__strategic_deployment, suppression_requirement, 1530, 0.12).
narrative_ontology:measurement(press_reformation_strategic_su_t1540, press_reformation_causation__strategic_deployment, suppression_requirement, 1540, 0.12).
narrative_ontology:measurement(press_reformation_strategic_su_t1555, press_reformation_causation__strategic_deployment, suppression_requirement, 1555, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__strategic_deployment, 0.02).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, vernacular_literacy_standardization).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, protestant_communicative_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint (strategic_deployment) is one of three readings of the press_reformation_causation kernel. The mutual_shaping reading treats technology and agency as co-evolutionary; the technological_determinism reading treats the press as a Mountain-like causal driver. All three share the same historical referent (1517-1555 press-Reformation dynamics) but author different ε, different beneficiary/victim structures, and different claimed types. They are linked via network.affects_constraints to enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
