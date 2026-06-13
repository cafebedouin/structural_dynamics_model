% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Provincial Democratic Sovereignty & Unilateral Secession Right (Popular Sovereignty Reading)
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the popular sovereignty reading of the
 *   secession legitimacy boundary kernel. It asserts that the ultimate source
 *   of political legitimacy resides in the democratic will of a provincial
 *   majority; a referendum producing such a majority is self-legitimating and
 *   grants the province unilateral right to exit the federal union. Federal
 *   authority, under this reading, is subordinate to popular sovereignty —
 *   the federal government cannot legitimately resist a clear democratic
 *   mandate for secession and must honor the result as binding. The
 *   constraint is heavily contested. Sibling readings deny this unilateral
 *   exit right, ground legitimacy in constitutional text
 *   (constitutional_impossibility), in grievance thresholds
 *   (grievance_threshold), or in indigenous treaty rights that predate
 *   provincial authority (treaty_primacy). This story models only the popular
 *   sovereignty reading as a clean, internally consistent claim: it specifies
 *   who benefits (provincial majorities able to exit), who bears costs
 *   (federal authority, minorities unable to block exit, indigenous nations
 *   whose treaty rights are subordinated to provincial majorities), and the
 *   structural enforcement dynamic (suppression of competing legitimacy
 *   readings, enforcement machinery that resists exit but lacks authority to
 *   prevent it indefinitely).
 *
 * KEY AGENTS:
 *   - provincial_majority_bloc: Agents within a province (or multiple provinces) commanding electoral majority; benefit from the unilateral exit right that gives them veto power over continued federal union and ability to exit if federal actions are perceived as extractive or contrary to provincial interest.
 *   - federal_authority_structure: The federal government and its institutions; bear the cost of the constraint via subordination of federal sovereignty to provincial referendum decisions and forced acceptance of secession if majority votes for it.
 *   - interprovincial_minorities: Minority groups within seceding provinces (regional minorities, ethnic/linguistic minorities, political opposition) who would be subject to provincial majority decision without nested self-determination rights; bear extraction costs if secession happens against their will.
 *   - indigenous_treaty_holders: Nations holding treaty rights that predate provincial and federal jurisdiction; their treaty-based authority is downranked under this reading relative to provincial majority will, creating structural conflict with the treaty_primacy reading.
 *   - federal_constitutional_text: Not an agent (agent=false in stakeholders, if named) but a vindicated proposition — the popular sovereignty reading subordinates constitutional text to referendum outcomes, treating the text as binding only insofar as popular will chooses to honor it.
 *   - international_order_and_recognition_states: External actors whose recognition of a seceded province determines practical viability; exert suppressive force on the constraint by conditioning recognition on factors (minority protection, debt assumption, treaty compliance) that may not align with provincial majority preferences.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.76).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Provincial Democratic Sovereignty & Unilateral Secession Right (Popular Sovereignty Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political/constitutional").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '03c2c8b6-0eb1-40fa-9012-97f4c12cf987').
narrative_ontology:cs_kernel_codification('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', distributed).
narrative_ontology:cs_authority_grounding('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', distributed).
narrative_ontology:cs_reading_relation('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', secession_legitimacy_boundary__treaty_primacy_reading, influences).
narrative_ontology:cs_axiom('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', foundational, popular_will_primacy).
narrative_ontology:cs_axiom_status(popular_will_primacy, holdable).
narrative_ontology:cs_axiom_grounding('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', popular_will_primacy, deontological).
narrative_ontology:cs_axiom('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', secondary, referendum_self_legitimation).
narrative_ontology:cs_axiom_status(referendum_self_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', referendum_self_legitimation, deontological).
narrative_ontology:cs_reference_frame('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', provincial_democratic_sovereignty_supreme).
narrative_ontology:cs_drift_state('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', contemporary_federal_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('03c2c8b6-0eb1-40fa-9012-97f4c12cf987', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_majority_bloc).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, federal_authority_structure).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, interprovincial_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_treaty_holders).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint grants a concentrated beneficiary (provincial majority) unilateral power over a large set of actors (federal authority, minorities, indigenous treaty holders) without structured negotiation or consent from the affected parties. The constraint persists because the provincial majority — commanding electoral dominance — can impose its will through referendum. Suppression is higher still (0.76) because maintaining the constraint requires actively enforcing the subordination of competing legitimacy claims: the federal government must be prevented from treating its constitutional authority as co-equal or independent; minorities must be prevented from exercising nested self-determination; indigenous treaty holders must be prevented from exercising treaty-based veto. This enforcement is not accomplished through resource incentives or voluntary coordination — it is accomplished through the silencing of alternative legitimacy narratives. Theater is moderate (0.42): the constraint operates through visible, repeatedly practiced mechanisms (referendum campaigns, election cycles, constitutional debate), but a growing portion of the machinery is devoted to narrative suppression (dismissing 'constitutional technicalities,' reframing indigenous concerns as 'side issues,' treating minorities as 'irrelevant to the majority mandate') rather than to positive coordination. The measurement trajectory shows rising extractiveness as the provincial majority's electoral dominance consolidates and alternative readings are progressively marginalized; theater rises as legitimation narratives become more elaborate and performative. Suppression rises as enforcement of the reading requires harder suppression of competing claims.
 *
 * PERSPECTIVAL GAP:
 *   The federal authority seat and the provincial majority seat compute radically differently. From the federal perspective, this reading is snare-like: it imposes unilateral exit rights on a federal structure that was designed as a negotiated compact, removes federal veto power over its own dissolution, and subordinates constitutional text to referendum outcomes it cannot influence. From the provincial majority perspective, it is rope-like: it solves the coordination problem of how a province exits federal arrangements it perceives as extractive, and it gives that majority voice and agency in determining its own fate. From the minorities-within-province and indigenous-treaty-holder perspectives, it is snare: it removes their veto power and subordinates their treaty rights to provincial majority will. The engine computes per-seat classification from the power, exit options, and directionality data; the authored metrics model the aggregate constraint as a tangled rope (coordinating provincial exit while extracting from federal authority, minorities, and indigenous treaty holders). Where computed types diverge from the claimed tangled rope, that divergence is the measurement — a federal seat computing snare reflects the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure flows from beneficiary/victim declarations and exit options. Provincial majorities are beneficiaries (gain the unilateral exit right) with powerful, organized power atoms and arbitrage-grade exit (can exit the federal union itself). This produces low d for them. Federal authority is a victim (loses co-equal sovereignty, forced to accept exit decisions) with institutional power but trapped exit (cannot dissolve its own structure unilaterally if the reading prevails). This produces high d for the federal authority structure, though this is asymmetrical — the federal government retains significant institutional enforcement capacity even under this reading, making it a 'trapped but still powerful' seat. Interprovincial minorities are victims (locked into provincial majority decision without their own referendum right) with moderate or powerless power atoms and identity_locked or trapped exit (cannot leave the province without abandoning citizenship or assets in their homeland). This produces very high d. Indigenous treaty holders are victims (their treaty authority is subordinated) with organized or institutional power atoms but trapped exit (cannot exercise treaty veto if the reading prevails). The reading systematically arranges d values to concentrate power in one seat (provincial majority) at the cost of all others. No directionality overrides are needed; the structural derivation is coherent.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading has no mandatrophy issue — the founding problem (how to enable a province to exit federal arrangements perceived as extractive or unjust) remains live and contested. The reading is not a vestigial relic; it is an active, defended claim in ongoing political contest. However, the reading does exhibit a subtle coordination/extraction boundary problem: the reading claims to solve the coordination problem of provincial exit, but it does so by extracting from minorities and indigenous treaty holders who have no voice in the referendum. This makes it a textbook tangled rope — genuine coordination function (enabling exit) paired with asymmetric extraction (imposing costs on non-participants). The theater component (42%) reflects the fact that significant legitimation narrative is required to sustain the reading against competing claims; as the reading's dominance consolidates in a real-world setting, theater would likely rise (more elaborate defenses of why indigenous treaty rights and minority protections are 'secondary to popular will').
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_secession_legitimacy,
    'Which of the four competing readings of the secession legitimacy kernel is structurally correct: popular sovereignty, constitutional impossibility, grievance threshold, or treaty primacy?',
    'Resolution would require either: (1) a definitive constitutional amendment or supreme court ruling foreclosing competing readings; (2) a real secession attempt and international recognition pattern revealing which reading''s logic the global order and courts actually enforce; (3) negotiated resolution among federal, provincial, and indigenous authorities producing a unified legitimacy framework.',
    'If this reading (popular sovereignty) is rejected in favor of constitutional_impossibility, the constraint type downgrades to snare with high suppression and restricted exit. If treaty_primacy prevails, the constraint becomes a negotiation mechanism with indigenous veto. If grievance_threshold prevails, legitimacy becomes contingent on federal conduct rather than unilateral democratic will.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_secession_legitimacy, conceptual, 'Contest among four incommensurable legitimacy framings of the same kernel (secession boundary).').

omega_variable(
    popular_will_measurement_ambiguity,
    'What constitutes a valid expression of ''democratic majority will'' for secession: a simple referendum majority, a supermajority threshold, repeated elections, or consultation with affected minorities and indigenous nations?',
    'Analysis of precedent from prior secession referenda (Québec 1995, Scotland 2014, Catalonia 2017) and court rulings on what counts as self-determination. Alternatively, a successful secession with international recognition would demonstrate which measurement standard was accepted as sufficient.',
    'Raising the threshold (supermajority, repeated validation) reduces the constraint''s extractiveness by locking in higher exit costs and coordination burdens. Lowering it (simple majority once) amplifies extractiveness by giving provincial majorities unilateral power. This is the practical bite of the reading: if 50%+1 suffices, the constraint is highly extractive for minorities and federal authority; if 66% or multiple elections are required, it becomes more coordination-oriented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_will_measurement_ambiguity, empirical, 'Operationalization of ''democratic majority will'' determines the constraint''s practical extractiveness.').

omega_variable(
    federal_authority_nature_under_popular_sovereignty,
    'Under this reading, is federal authority retained by delegation from provincial majorities (revocable at will), or does it possess an independent legitimacy source that constrains provincial exit?',
    'Textual analysis of constitutional and statutory language grounding federal power. Empirical observation: do provincial actors treat federal authority as delegated (and thus dispensable) or as co-equal and binding? Do courts recognize federal supremacy claims as independent or only when popular will in provinces has not clearly revoked the delegation?',
    'If federal authority is purely delegated and revocable, suppression must be very high (0.85+) because the federal government cannot legitimately resist a provincial exit decision without violating the reading''s core axiom. If federal authority has independent legitimacy even under this reading, suppression could be lower and the constraint becomes more symmetric — a genuine negotiation between co-equal sovereigns. The authored suppression (0.76) splits the difference, modeling federal enforcement machinery that resists the exit but lacks illegitimate authority to do so, creating active conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_authority_nature_under_popular_sovereignty, conceptual, 'Whether federal authority under this reading is delegated-and-revocable or co-equal-and-binding.').

omega_variable(
    interprovincial_minority_protection_gap,
    'This reading grants unilateral secession to provincial majorities; what protection does it offer minorities within the seceding province (regional minorities, ethnic minorities, indigenous nations holding treaty rights)?',
    'Analysis of actual secession proposals and their minority-protection provisions. Empirically: do provincial majorities in secession contexts propose to include minority exit rights, minority veto powers, or border-redrawing to protect minority-majority regions? Or does provincial majority sovereignty treat internal minorities as subordinate to the majority will?',
    'If the reading includes robust minority protections and nested self-determination (minorities within the province retain their own exit rights), suppression and extractiveness decline because the mechanism becomes coordinative rather than majoritarian. If the reading treats provincial majorities as absolute over their territory, suppression rises sharply (0.85+) and extractiveness becomes high for internal minorities — the constraint functions as snare for them even if it functions as tangled rope at the federal level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interprovincial_minority_protection_gap, empirical, 'Whether provincial majority sovereignty includes or excludes protections for minorities within the seceding province.').

omega_variable(
    international_recognition_conditionality,
    'Does this reading''s legitimacy rest on internal democratic process alone, or is it contingent on international recognition (which may impose conditions like minority protection, debt assumption, or treaty compliance)?',
    'Observation from real secession cases: does the reading predict that internal referendum suffices, or do proponents acknowledge that international acceptance depends on external factors (UN, neighboring states, trade partners, indigenous treaty compliance)?',
    'If purely internal process suffices, the constraint is a unilateral exit rule and suppression is lower (federal enforcement machinery meets no external constraint). If international recognition is necessary and conditional, the constraint becomes a multi-stage negotiation and suppression is higher (federal can appeal to international law and indigenous treaty rights to block recognition even after referendum). The authored suppression (0.76) models partial internationalization — federal enforcement is partially constrained by external legitimacy pressure, but internal referendum still carries weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_conditionality, empirical, 'Whether the reading''s legitimacy is internal-democratic-process-only or subject to international-recognition conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sece_tr_t5, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(sece_tr_t15, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(sece_tr_t25, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sece_be_t5, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(sece_be_t15, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(sece_be_t25, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(sece_su_t5, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(sece_su_t15, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(sece_su_t25, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 25, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a four-story family decomposing the contested kernel 'secession_legitimacy_boundary'. Each story is one incommensurable reading of what counts as legitimate grounds for provincial exit from a federal union. The four readings have different ε values (this one is high-extractiveness because it concentrates exit power in provincial majorities; the treaty_primacy reading has different victims and different extraction structure; the constitutional_impossibility reading denies unilateral exit entirely, producing different suppression mechanics). The family members are linked by network.affects_constraints in both directions: each reading's legitimacy claim influences the others' viability (a prevailing popular sovereignty reading undermines the constitutional impossibility reading; the presence of treaty rights constrains the popular sovereignty reading). The ε-invariance principle requires separate stories because measuring the constraint via 'does provincial referendum outcome override federal authority?' yields different ε under different readings. The four stories should be analyzed as a family to understand how legitimacy frameworks differ and compete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
