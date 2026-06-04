% ============================================================================
% CONSTRAINT STORY: criminal_procedure_amendments__fifth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_criminal_procedure_amendments__fifth_amendment, []).

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
 *   constraint_id: criminal_procedure_amendments__fifth_amendment
 *   human_readable: Fifth Amendment: Charter Against Compelled Self-Conviction and Uncompensated Taking
 *   domain: political/legal/criminal_procedure
 *
 * SUMMARY:
 *   The Fifth Amendment bundles five nominally distinct protections (grand
 *   jury indictment, double jeopardy, self-incrimination, due process,
 *   takings) into a unified charter against state overreach. This reading
 *   instantiates one interpretation of the contested
 *   criminal_procedure_amendments kernel. The structural delta distinguishes
 *   it from sibling readings: the Fifth Amendment's core concern is
 *   SUPPRESSION of compelled self-conviction and uncompensated taking, with
 *   beneficiary set (accused persons, property holders) and victim set
 *   (prosecutorial convenience, state eminent-domain power) derived from that
 *   core. The constraint exhibits tangled rope structure: it coordinates the
 *   criminal process (requires clear procedures, prevents harassment through
 *   double jeopardy, creates attorney-client privilege) while simultaneously
 *   extracting (prosecutorial advantage persists despite nominal protections,
 *   resource asymmetries render protections inert for the poor, takings
 *   compensation is often inadequate). The theater_ratio trajectory (0.42 →
 *   0.58 across 80-year interval) reflects increasing performativity: nominal
 *   Fifth Amendment compliance coexists with shadow procedures (civil
 *   forfeiture, plea bargaining regimes, asset seizure) that neutralize
 *   protections while preserving formal adherence. The analytically observed
 *   natural law frame — that adversarial procedure requires some baseline
 *   protection against compelled self-incrimination — is contradicted by the
 *   structural data showing measurable extractiveness, active enforcement
 *   requirements, and identifiable beneficiaries. This generates a
 *   false-summit diagnostic signal: the constraint is framed as immutable but
 *   behaves as a contingent institutional arrangement.
 *
 * KEY AGENTS:
 *   - Accused persons (powerless/trapped): Primary nominal beneficiary but often trapped by resource barriers; experience the constraint as snare when indigent, rope/tangled_rope when resourced
 *   - Property holders (moderate/constrained): Beneficiaries of takings clause but face resource-intensive litigation to establish just compensation; experience rope/tangled_rope depending on property value and state capacity
 *   - Defense bar (institutional/constrained): Institutional beneficiary; protection requires active enforcement through adversarial litigation; experiences tangled_rope (benefits from leverage points, constrained by enforcement burden)
 *   - Prosecutorial system (institutional/constrained): Institutional victim (loses certain investigative tools); experiences tangled_rope (constrained by Fifth Amendment rules but benefits from procedural legitimacy and case-quality incentives)
 *   - Appellate judiciary (institutional/arbitrage): Manages coordination; experiences rope (pure coordination mechanism with no experienced extraction)
 *   - Legislative branch (institutional/arbitrage): Formally subordinate to Fifth Amendment but enacts shadow procedures that neutralize protections; experiences piton (performative constraint with high theater)
 *   - Analytical observer (analytical/analytical): Risks naturalizing contingent institutional arrangement as a natural law of adversarial process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(criminal_procedure_amendments__fifth_amendment, 0.38).
domain_priors:suppression_score(criminal_procedure_amendments__fifth_amendment, 0.52).
domain_priors:theater_ratio(criminal_procedure_amendments__fifth_amendment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(criminal_procedure_amendments__fifth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(criminal_procedure_amendments__fifth_amendment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(criminal_procedure_amendments__fifth_amendment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(criminal_procedure_amendments__fifth_amendment, tangled_rope).
narrative_ontology:human_readable(criminal_procedure_amendments__fifth_amendment, "Fifth Amendment: Charter Against Compelled Self-Conviction and Uncompensated Taking").
narrative_ontology:topic_domain(criminal_procedure_amendments__fifth_amendment, "political/legal/criminal_procedure").

domain_priors:requires_active_enforcement(criminal_procedure_amendments__fifth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(criminal_procedure_amendments__fifth_amendment, 'cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f').
narrative_ontology:cs_kernel_codification('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', formalized).
narrative_ontology:cs_authority_grounding('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', lineage).
narrative_ontology:cs_interpretation_layer_present('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f').
narrative_ontology:cs_reading_relation('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', criminal_procedure_amendments__sixth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', criminal_procedure_amendments__eighth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', criminal_procedure_amendments__fourth_amendment, influences).
narrative_ontology:cs_reading_relation('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', criminal_procedure_amendments__seventh_amendment, coexists_with).
narrative_ontology:cs_axiom('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', foundational, compelled_self_incrimination_prohibited).
narrative_ontology:cs_axiom_status(compelled_self_incrimination_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', compelled_self_incrimination_prohibited, deontological).
narrative_ontology:cs_axiom('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', foundational, uncompensated_taking_prohibited).
narrative_ontology:cs_axiom_status(uncompensated_taking_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', uncompensated_taking_prohibited, deontological).
narrative_ontology:cs_reference_frame('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', adversarial_accused_protection_framework).
narrative_ontology:cs_drift_state('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', contemporary_shadow_procedure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cfdd9ed2-9fb6-4811-bbdd-a2581f7d5a1f', '').
narrative_ontology:cs_kernel_id(criminal_procedure_amendments__fifth_amendment, criminal_procedure_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__fifth_amendment, accused_persons).
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__fifth_amendment, property_holders).
narrative_ontology:constraint_victim(criminal_procedure_amendments__fifth_amendment, prosecutorial_convenience).
narrative_ontology:constraint_victim(criminal_procedure_amendments__fifth_amendment, state_eminent_domain_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACCUSED WITHOUT RESOURCES (SNARE) — A defendant facing serious charges but lacking means for adequate counsel experiences the Fifth Amendment's protections as largely theoretical. Miranda rights and takings doctrine exist but enforcement requires resources (appeals, expert witnesses, land appraisals) the powerless cannot access. Suppression is high: indigent defendants are trapped within the criminal justice system with no meaningful exit. The constraint appears as pure extraction despite its nominal protections — the procedural guarantees are shadowed by resource barriers that render them inert for those who need them most.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE-RESOURCE DEFENDANT (ROPE) — A defendant with sufficient means for competent counsel experiences the Fifth Amendment primarily as a coordination mechanism. Double jeopardy doctrine prevents harassment through repetitive prosecution; self-incrimination privilege enables attorney-client strategy; due process creates procedural predictability. These are genuine coordination functions that enable defense. Extraction is real (discovery asymmetries, prosecutorial discretion) but the constraint also protects against state overreach. The beneficiary relationship is mixed — the accused benefits from the structure even as they bear some extraction cost.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE BAR (TANGLED ROPE) — Criminal defense attorneys see the Fifth Amendment as both coordinating their professional function and constraining prosecutorial overreach. The constraint enables privileged communication (attorney-client), provides leverage points (exclusionary rules, Miranda), and creates litigation space. But enforcement requires active advocacy — without defense counsel invoking protections, they do not self-execute. The defense bar benefits from having these levers available (enables their professional value) and bears costs (resource-intensive litigation, adversarial burden). Requires active enforcement: no cooperation with prosecutors means fewer negotiated settlements; adversarial activation of Fifth Amendment protections raises case complexity and discovery burdens. Classification derives from the mixed benefit-extraction relationship combined with the active enforcement requirement.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROSECUTORIAL SYSTEM (TANGLED ROPE) — Prosecutors experience the Fifth Amendment as constraining their reach but also as enabling their legitimacy. Self-incrimination doctrine prevents coerced confessions (which create appellate reversals and undermine conviction reliability). Double jeopardy prevents wasteful re-prosecution. Due process requirements create consistency (defendants know the rules, can prepare defenses, reducing unfair surprise). These are coordination functions — the constraint structures how prosecutors work, forcing case quality over case quantity. But coordination comes with extraction: prosecutors lose certain investigative shortcuts (compelled testimony from the accused, ex post facto charges), must honor plea negotiation outcomes, must respect finality. The constraint requires active enforcement by defense counsel to activate — passive defendants cannot self-invoke protections, so the prosecutorial convenience survives longer. Classification reflects that prosecution both benefits from the procedural legitimacy the constraint provides AND bears real extraction (limited investigative tools, finality requirement).
constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: APPELLATE JUDICIARY (ROPE) — Federal appellate courts experience the Fifth Amendment as a pure coordination mechanism: it structures how lower courts must operate, provides clear standards for review, and enables uniform application across jurisdictions. Appeals courts benefit from having stable doctrinal rules (reduces cognitive burden of case-by-case adjudication). The constraint requires active enforcement by appellate counsel (raising Fifth Amendment issues on appeal) but does not extract from the judiciary — judicial resources are deployed according to established procedure, not redirected. This is coordination with near-zero experienced extraction for the institutional actor managing the system.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGISLATIVE BRANCH (PITON) — Congress experiences the Fifth Amendment as a largely performative constraint. Congress cannot override Fifth Amendment protections (constitutional supremacy), but legislators can (and do) enact procedural rules that shadow the Fifth Amendment's protections: asset forfeiture statutes that bypass due process, plea bargaining regimes that pressure guilty pleas even for innocent defendants, bail structures that effectively trap the indigent. The Amendment's theater consists of affirmations of principle (victims' rights legislation, bail reform rhetoric) coupled with rules that neutralize protection (civil forfeiture, mandatory minimums that force plea deals). Theater_ratio is high because legislative activity preserves nominal conformity with the Fifth Amendment while enabling prosecutorial overreach through shadow procedures. The constraint is inert from the legislature's perspective — it does not require enforcement against legislative action, only against executive/judicial action.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Fifth Amendment's core commitments (that no person shall be compelled to be a witness against themselves, that no person shall be deprived of property without due process) reflect an inviolable principle of human dignity and proportional state power. This perspective sees the constraint as emerging naturally from the logic of adversarial procedure itself: if the state is empowered to coerce testimony and seize property, some structural barrier is required or the power becomes unlimited. However, the structural data contradicts the mountain classification — the constraint shows measurable extractiveness (0.38), requires active enforcement, and has identifiable beneficiaries and victims. The engine will compute this as a false summit, revealing that 'natural law of adversarial process' is a framing choice rather than a discovered invariant.
constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(criminal_procedure_amendments__fifth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(criminal_procedure_amendments__fifth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(criminal_procedure_amendments__fifth_amendment, TR),
    TR >= 0.70.

:- end_tests(criminal_procedure_amendments__fifth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Fifth Amendment genuinely constrains state power in specific dimensions (no compelled self-incrimination, no double jeopardy, due process requirements). But extractiveness is not negligible because resource asymmetries, prosecutorial discretion, and shadow procedures (civil forfeiture, plea bargaining) enable the state to capture investigative advantage through alternative mechanisms. The trajectory (0.28 → 0.38) shows rising extractiveness as shadow procedures accumulate and Miranda doctrine narrows through exceptions. Suppression (0.52): Moderate-high. Structural barriers to exiting the criminal justice system are substantial: bail structures trap the poor, plea bargaining regimes coerce guilty pleas even for innocent defendants, public defender constraints prevent meaningful assertion of Fifth Amendment protections. But suppression is not total — resourced defendants can hire competent counsel and invoke protections effectively. Theater ratio (0.58): Moderate-high and rising. Legislative affirmations of Fifth Amendment principles coexist with statutes that shadow its protections. Asset forfeiture doctrine invokes due process language while stripping property without traditional criminal conviction. Bail reform rhetoric coexists with bail structures that effectively detain the poor. Miranda warnings preserve nominal compliance while exceptions have narrowed the privilege. The rising trajectory reflects accumulating procedural theater: more doctrinal complexity, more judicial language affirming principle, more functional erosion through exceptions. Claimed type (tangled_rope): The constraint coordinates the criminal process (prevents harassment, creates privilege, ensures due process) while simultaneously extracting (prosecutorial advantage persists, resource asymmetries render protections inert, shadow procedures neutralize specific protections). Both functions are genuine, neither is negligible.
 *
 * PERSPECTIVAL GAP:
 *   The powerless accused experiences the Fifth Amendment as a snare despite its nominal protections — resource barriers render the self-incrimination privilege, double jeopardy protection, and due process guarantees largely inaccessible. The resourced accused experiences rope or tangled rope — they can hire counsel, invoke protections, and negotiate from a position of relative strength. The defense bar experiences tangled rope — protections enable their professional function but require active, resource-intensive enforcement. The prosecutorial system experiences tangled rope — the constraint reduces investigative shortcuts but improves case quality and legitimacy. The judiciary experiences rope — the constraint coordinates judicial procedure without extracting from judges. The legislature experiences piton — nominally subordinate to the Fifth Amendment but practically free to enact shadow procedures that preserve performative compliance while neutralizing functional effect. The analytical observer risks mountain — seeing compelled self-incrimination as a natural law of justice — but the structural data reveals this as a false summit: the protection is contingent on resources, enforcement, and procedural compliance that vary dramatically across actors and time.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position. Powerless accused: victim status (compelled to participate in system), trapped exit options → high d → high experienced extractiveness despite nominal protections. Resourced accused: beneficiary status (can invoke protections), constrained exit options (still captured but with options) → moderate d → moderate experienced extractiveness. Defense bar: beneficiary status (leverage points enable professional value), constrained exit options (cannot exit adversarial system) → low d → low/neutral experienced extractiveness. Prosecutorial system: victim status (loses investigative shortcuts), constrained exit options (bound by constitutional requirement) → high d → high experienced extractiveness. Appellate judiciary: beneficiary status (coordination mechanism), arbitrage options (can reinterpret doctrine) → low d → negative experienced extractiveness. Legislative branch: beneficiary status (can shadow protections with alternative statutes), arbitrage options (can enact workarounds) → very low d → near-zero experienced extractiveness. Analytical observer: analytical exit options apply canonical d, and observer position yields moderate d reflecting that any analysis of the constraint from outside involves acknowledging both its protective function and its erosion mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by showing that the tangled_rope classification is accurate: it genuinely coordinates the criminal process (prevents certain prosecutorial tactics, creates privilege, requires due process) while genuinely extracting (prosecutorial advantage persists, resource asymmetries render protections inert, shadow procedures neutralize effect). Both functions are real. The false-summit diagnostic for the analytical observer's mountain classification reveals the mandatrophy resolution: the Fifth Amendment is NOT an immutable natural law of adversarial process, but rather a contingent institutional arrangement that has drifted (theater_ratio rising, extractiveness rising) as shadow procedures accumulate. The constraint is maintainable in its current form only because the performative layer (doctrinal affirmations) is sufficiently thick to preserve nominally compliant jurisprudence while the functional layer (actual protections) has eroded. Resolving the false summit would require either (a) genuine commitment to resource equity (making protections functional for the poor) or (b) explicit abandonment of the protection framework in favor of a different legitimacy claim (e.g., cost-benefit analysis of prosecutorial efficiency). The current state is sustainable because neither resolution is politically feasible — the constraint persists as theatrical naturalization of a degraded coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fifth_amendment_scope_ambiguity,
    'Does the Fifth Amendment protect a natural right against self-incrimination, or is it a procedural rule allocating investigative burden between state and accused?',
    'Historical analysis of framing intent, comparative analysis with non-adversarial legal systems, assessment of whether the protection appears in systems without adversarial procedure',
    'If natural right: mountain classification from all perspectives. If procedural rule: tangled_rope from most perspectives, with variation based on actor position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fifth_amendment_scope_ambiguity, conceptual, 'Whether Fifth Amendment protects a natural right or allocates procedural burden').

omega_variable(
    compelled_testimony_baseline,
    'Is the Fifth Amendment''s suppression of compelled self-conviction a constraint on the state, or does it merely specify a different investigative baseline?',
    'Comparison of criminal conviction rates and case outcomes under systems with and without self-incrimination protection; analysis of whether prosecutions are less successful or merely directed differently',
    'If genuine suppression: extractiveness lower (0.30–0.40 range). If baseline shift only: extractiveness higher (0.55–0.70 range) because the state simply uses alternative mechanisms (wiretaps, financial records, witness coercion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compelled_testimony_baseline, empirical, 'Whether Fifth Amendment suppresses extraction or redirects investigative method').

omega_variable(
    takings_doctrine_compensation_adequacy,
    'Is just compensation under the Takings Clause adequate, or does it systematically undervalue what is taken?',
    'Empirical analysis of compensation awards vs fair market value vs replacement cost; assessment of whether owners can afford equivalent property with awarded compensation',
    'If adequate: takings protection is genuine coordination (fair exchange). If systematically inadequate: takings clause is a nominal protection with high extractiveness (0.60+).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(takings_doctrine_compensation_adequacy, empirical, 'Whether takings compensation is adequate').

omega_variable(
    resource_asymmetry_fatal_flaw,
    'Do resource asymmetries between state and accused render Fifth Amendment protections inert for poor defendants, making the constraint effectively a snare from their perspective?',
    'Comparison of Fifth Amendment invocation rates and success rates across wealth quintiles; analysis of whether public defender constraints prevent meaningful assertion of protections',
    'If fatal flaw: constraint reclassifies to snare from powerless perspective even though nominal protections exist. Current classification assumes constrained exit can access counsel; if counsel is structurally unavailable, exit_options should be trapped, driving snare classification across all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_asymmetry_fatal_flaw, empirical, 'Whether resource asymmetry renders Fifth Amendment protections inert for poor defendants').

omega_variable(
    kernel_contest_double_jeopardy_double_counting,
    'Does the Fifth Amendment''s double jeopardy clause logically coexist with the Eighth Amendment''s proportionality requirement, or do they instantiate contradictory baselines for finality vs punishment review?',
    'Doctrinal analysis of how appeals courts navigate cases where a conviction survives double jeopardy (finality) but appears to violate proportionality (harsh sentence). Which doctrine prevails?',
    'If coexist: siblings classified as coexists_with. If contradictory baselines: siblings classified as forecloses relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_double_jeopardy_double_counting, conceptual, 'Whether double jeopardy and proportionality can coexist').

omega_variable(
    kernel_contest_due_process_overlap,
    'Does the Fifth Amendment''s due process guarantee overlap structurally with the Sixth Amendment''s fair trial machinery, or do they protect different dimensions (substance vs procedure)?',
    'Doctrinal mapping of Fifth Amendment due process cases vs Sixth Amendment fair trial cases. Identification of overlap cases and exclusivity cases.',
    'If overlap: both amendments protect the same constraint from different angles — may split into separate stories with shared kernel. If distinct dimensions: no overlap, coexists_with relation holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_due_process_overlap, conceptual, 'Whether Fifth Amendment due process overlaps Sixth Amendment fair trial guarantees').

omega_variable(
    historical_drift_miranda_erosion,
    'Has the Miranda doctrine, which operationalizes the self-incrimination protection, systematically eroded through exceptions and narrow application?',
    'Doctrinal analysis of Miranda exceptions (public safety, impeachment use, derivative evidence); statistical analysis of confession rates and Miranda invocation success rates over time',
    'If substantial erosion: contemporary base_extractiveness may be higher (0.45+) than framing intent suggests. Theater_ratio may rise as nominal protections are preserved while functional effect narrows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_drift_miranda_erosion, empirical, 'Whether Miranda doctrine has eroded Fifth Amendment protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(criminal_procedure_amendments__fifth_amendment, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fifth_tr_t0, criminal_procedure_amendments__fifth_amendment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fifth_tr_t40, criminal_procedure_amendments__fifth_amendment, theater_ratio, 40, 0.52).
narrative_ontology:measurement(fifth_tr_t80, criminal_procedure_amendments__fifth_amendment, theater_ratio, 80, 0.58).

% Extraction over time
narrative_ontology:measurement(fifth_be_t0, criminal_procedure_amendments__fifth_amendment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fifth_be_t40, criminal_procedure_amendments__fifth_amendment, base_extractiveness, 40, 0.33).
narrative_ontology:measurement(fifth_be_t80, criminal_procedure_amendments__fifth_amendment, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fifth_su_t0, criminal_procedure_amendments__fifth_amendment, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fifth_su_t40, criminal_procedure_amendments__fifth_amendment, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(fifth_su_t80, criminal_procedure_amendments__fifth_amendment, suppression_requirement, 80, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(criminal_procedure_amendments__fifth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(criminal_procedure_amendments__fifth_amendment, criminal_procedure_amendments__sixth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__fifth_amendment, criminal_procedure_amendments__fourth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__fifth_amendment, criminal_procedure_amendments__eighth_amendment).

% DUAL FORMULATION NOTE:
% The Fifth Amendment is one reading of the criminal_procedure_amendments kernel. Related constraints (Sixth, Fourth, Eighth Amendment stories) decompose the broader Bill of Rights criminal procedure protections. The Fifth Amendment's focus on self-incrimination suppression and takings compensation distinguishes it from siblings' foci (fair trial machinery, search constraints, proportional punishment). Network links model institutional coupling: Fifth Amendment doctrine influences Fourth Amendment interrogation boundaries, Sixth Amendment counsel requirements, and Eighth Amendment proportionality review. These are not separate phenomena — they are interdependent readings of a single constitutional commitment to constrained state power in criminal matters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(criminal_procedure_amendments__fifth_amendment, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
